@license{
  Copyright (c) 2009-2011 CWI
  All rights reserved. This program and the accompanying materials
  are made available under the terms of the Eclipse Public License v1.0
  which accompanies this distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html
}
@contributor{Mark Hills - Mark.Hills@cwi.nl (CWI)}
@bootstrapParser
module lang::rascal::types::CheckTypes

import List;
import Graph;
import IO;
import Set;
import Map;
import ParseTree;
import Message;
import Node;
import Type;
import Relation;
import util::Reflective;

import lang::rascal::checker::ListUtils;
import lang::rascal::checker::TreeUtils;
import lang::rascal::types::AbstractKind;
import lang::rascal::types::AbstractName;
import lang::rascal::types::AbstractType;
import lang::rascal::types::ConvertType;
import lang::rascal::types::TypeSignature;
import lang::rascal::types::TypeInstantiation;
import lang::rascal::checker::ParserHelper;

import lang::rascal::syntax::RascalRascal;

//
// TODOs
// 1. Add scoping information for boolean operations (and, or, etc) and surrounding
//    constructs (comprehensions, matches, conditionals, etc)
//
// 2. Add support for _ in subscripts DONE: This is only allowed in subscripts
//    in expressions, not in assignables.
//
// 3. Add support for _ in patterns, to ensure we don't accidentally bind _ as a name DONE
//
// 4. Filter out bad assignables? For instance, we cannot have <x,y>.f, but it does
//    parse, so we may be handed an example such as this.
//
// 5. Expand out uses of user types, including those with parameters DONE
//
// 6. Check tags to make sure they are declared
//
// 7. Make sure add errors are encoded as exceptions. This way we don't need
//    to always encode checks to see if we can add the given item, we can just
//    try and catch any errors.
//
// 8. Make sure that, when we have a declaration (even one with a type), we
//    check shadowing! This is done in statements, but is not always done
//    now in patterns.
//
// 9. Make sure that varargs parameters are properly given list types. Also make sure
//    they are just names (not varargs of a tuple pattern, for instance)
//
// 10. Make sure we always instantiate type parameters when we use a constructor. NOTE: This
//     is partially done -- it has been done for call or tree expressions, but not yet for
//     call or tree patterns.
//
// 11. For ADT types, the user type could have other types for bounds which have
//     not yet been introduced (although this would be odd), so we need to
//     ignore those when we are not descending
//
// 12. Remember to pop the function when we exit!!! DONE: We already were doing this
//     by just resetting to the old stack.
//
// 13. Check values created by append NOTE: This is partially done, in that we are gathering
//     the types. Additional checking may still be needed.
//
// 14. Instantiate types in parametric function calls. DONE.
//
// 15. Make sure type var bounds are consistent.
//
// 16. Change shadowing rules to allow shadowing of imported items by items
//     in the current module.

@doc{The source of a label (visit, block, etc).}
data LabelSource = visitLabel() | blockLabel() | forLabel() | whileLabel() | doWhileLabel() | ifLabel();

@doc{Function modifiers.}
data Modifier = javaModifier() | testModifier() | defaultModifier();

@doc{Convert from the concrete to the abstract representation of modifiers.}
Modifier getModifier(FunctionModifier fmod:`java`) = javaModifier();
Modifier getModifier(FunctionModifier fmod:`test`) = testModifier();
Modifier getModifier(FunctionModifier fmod:`default`) = defaultModifier();

@doc{Visibility of declarations.}
data Vis = publicVis() | privateVis() | defaultVis();

@doc{Convert from the concrete to the abstract representation of visibilities.}
Vis getVis(Visibility v:(Visibility)`private`) = privateVis();
Vis getVis(Visibility v:(Visibility)`public`) = publicVis();
default Vis getVis(Visibility v) = defaultVis();

@doc{Abstract values manipulated by the semantics.}
data AbstractValue 
	= label(RName name, LabelSource source, int containedIn, loc at) 
	| variable(RName name, Symbol rtype, bool inferred, int containedIn, loc at)
	| function(RName name, Symbol rtype, bool isVarArgs, int containedIn, loc at)
	| closure(Symbol rtype, int containedIn, loc at)
	| \module(RName name, loc at)
	| overload(set[int] items, Symbol rtype)
	| datatype(RName name, Symbol rtype, int containedIn, set[loc] ats)
	| constructor(RName name, Symbol rtype, int containedIn, loc at)
	| annotation(RName name, Symbol rtype, set[Symbol] onTypes, int containedIn, loc at)
	| \tag(RName name, TagKind tkind, set[Symbol] onTypes, int containedIn, loc at)
	| \alias(RName name, Symbol rtype, int containedIn, loc at) 
	;

@doc{Configurations provide the state used during evaluation.}
data Configuration = config(set[Message] messages, 
							map[loc,Symbol] locationTypes, 
							Symbol expectedReturnType, 
							map[RName,int] labelEnv,
							map[RName,int] fcvEnv,
							map[RName,int] typeEnv,
							map[RName,int] modEnv,
							map[RName,int] annotationEnv,
							map[RName,int] tagEnv,
							map[int,Vis] visibilities,
							map[int,AbstractValue] store,
							map[tuple[int,str],Symbol] adtFields,
							rel[int,Modifier] functionModifiers,
							rel[int,loc] definitions,
							rel[int,loc] uses,
							rel[int,int] adtConstructors,
							list[int] stack,
							list[tuple[RName labelName,LabelSource labelSource,Symbol labelType]] labelStack,
							int nextLoc,
							int uniqueify,
							bool keepMatchVars
						   );

public Configuration newConfiguration() = config({},(),\void(),(),(),(),(),(),(),(),(),(),{},{},{},{},[],[],0,0,false);

@doc{Add a new location type.}
public CheckResult markLocationType(Configuration c, loc l, Symbol t) {
	if (isFailType(t)) return markLocationFailed(c, l, t);
	c.locationTypes[l] = t;
	return < c, t >;
}

@doc{Mark that a location has type fail.}
public CheckResult markLocationFailed(Configuration c, loc l, set[Symbol] ts) {
	res = collapseFailTypes(ts);
	c.locationTypes[l] = res;
	c.messages += getFailures(res);
	return < c, res >;
}

public CheckResult markLocationFailed(Configuration c, loc l, Symbol t) = markLocationFailed(c,l,{t});

public Configuration recoverEnvironments(Configuration cNew, Configuration cOld) {
	cNew.labelEnv = cOld.labelEnv;
	cNew.fcvEnv = cOld.fcvEnv;
	cNew.typeEnv = cOld.typeEnv;
	cNew.modEnv = cOld.modEnv;
	cNew.annotationEnv = cOld.annotationEnv;
	cNew.tagEnv = cOld.tagEnv;
	return cNew;
}

public Configuration recoverEnvironmentsAfterCall(Configuration cNew, Configuration cOld) {
	cNew = recoverEnvironments(cNew,cOld);
	cNew.expectedReturnType = cOld.expectedReturnType;
	cNew.stack = cOld.stack;
	return cNew;
}

public Configuration setExpectedReturn(Configuration c, Symbol t) {
	return c[expectedReturnType = t];
}

public bool labelExists(Configuration c, RName n) = n in c.labelEnv;

public Configuration addLabel(Configuration c, RName n, loc l, LabelSource ls) {
	c.labelEnv[n] = c.nextLoc;
	c.store[c.nextLoc] = label(n,ls,head(c.stack),l);
	c.definitions = c.definitions + < c.nextLoc, l >;
	c.nextLoc = c.nextLoc + 1;
	return c;
}

public bool fcvExists(Configuration c, RName n) = n in c.fcvEnv;

public Configuration addVariable(Configuration c, RName n, bool inf, loc l, Symbol rt) {
	moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
	c.fcvEnv[n] = c.nextLoc;
	if (\module(_,_) := c.store[head(c.stack)]) {
		// If this variable is module-level, also make it referenceable through
		// the qualified name module::variable.
		moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
		c.fcvEnv[appendName(moduleName,n)] = c.nextLoc;
	}
	c.store[c.nextLoc] = variable(n,rt,inf,head(c.stack),l);
	c.definitions = c.definitions + < c.nextLoc, l >;
	c.nextLoc = c.nextLoc + 1;
	return c;
}

public Configuration addVariable(Configuration c, RName n, bool inf, Vis visibility, loc l, Symbol rt) {
	c = addVariable(c,n,inf,l,rt);
	c.visibilities[c.nextLoc-1] = visibility;
	return c;
}

public Configuration addAnnotation(Configuration c, RName n, Symbol rt, Symbol rtOn, Vis visibility, loc l) {
	// TODO: We currently always treat annotation declarations as public, so we just
	// ignore the visibility here. If we decide to allow private annotation declarations,
	// revisit this.
	if (n notin c.annotationEnv) {
		c.annotationEnv[n] = c.nextLoc;
		c.store[c.nextLoc] = annotation(n,rt,{rtOn},head([i | i <- c.stack, \module(_,_) := c.store[i]]),l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.nextLoc = c.nextLoc + 1;
	} else {
		if (!equivalent(rt,c.store[c.annotationEnv[n]].rtype))
			throw "All annotation types in an annotation set much be equivalent";
		c.store[c.annotationEnv[n]].onTypes = c.store[c.annotationEnv[n]].onTypes + rtOn; 
		c.definitions = c.definitions + < c.annotationEnv[n], l >;
	}
	return c;
}

public Configuration addADT(Configuration c, RName n, Vis visibility, loc l, Symbol rt) {
	// TODO: We currently always treat datatype declarations as public, so we just
	// ignore the visibility here. If we decide to allow private datatype declarations,
	// revisit this.
	moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
	if (n notin c.typeEnv) {
		// When we initially add the name, add it as:
		// * the bare name
		// * the name appended to the name of the module
		// This ensures that all valid lookups will be successful.
		c.typeEnv[n] = c.nextLoc;
		c.typeEnv[appendName(moduleName,n)] = c.nextLoc;
		c.store[c.nextLoc] = datatype(n,rt,head([i | i <- c.stack, \module(_,_) := c.store[i]]),{ });
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.nextLoc = c.nextLoc + 1;
	} else {
		c.definitions = c.definitions + < c.typeEnv[n], l >;
	}
	c.store[c.typeEnv[n]].ats = c.store[c.typeEnv[n]].ats + l; 
	return c;
}

public Configuration addAlias(Configuration c, RName n, Vis vis, loc l, Symbol rt) {
	// TODO: We currently always treat datatype declarations as public, so we just
	// ignore the visibility here. If we decide to allow private datatype declarations,
	// revisit this.
	moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
	// When we initially add the name, add it as:
	// * the bare name
	// * the name appended to the name of the module
	// This ensures that all valid lookups will be successful.
	c.typeEnv[n] = c.nextLoc;
	c.typeEnv[appendName(moduleName,n)] = c.nextLoc;
	c.store[c.nextLoc] = \alias(n,rt,head([i | i <- c.stack, \module(_,_) := c.store[i]]),l);
	c.definitions = c.definitions + < c.nextLoc, l >;
	c.nextLoc = c.nextLoc + 1;
	return c;
}

public Configuration addConstructor(Configuration c, RName n, loc l, Symbol rt) {
	// First, verify that the ADT is in the type environment. If not, this is an error
	// (especially since the constructor should be defined in the same construct as
	// the ADT!)
	adtName = RSimpleName(rt.\adt.name);
	if (adtName notin c.typeEnv) throw "Unexpected error, adt <prettyPrintName(adtName)> not found!";
	adtId = c.typeEnv[adtName];
	
	// Now, process the arguments. This performs several consistency checks, namely:
	// * either all fields must have labels, or none should have labels
	// * labels should not be repeated in the same constructor
	// * labels shared between constructors should have matching types
	args = getConstructorArgumentTypes(rt);
	if (size(args) > 0) {
		labeledArgs = [ arg | arg <- args, \label(_,_) := arg ];
		if (size(labeledArgs) > 0) {
			if (size(labeledArgs) != size(args)) {
				c = addScopeError(c,"On constructor definitions, either all fields should be labeled or no fields should be labeled", l);
			} else {
				set[str] seenAlready = { };
				for (\label(fn,ft) <- args) {
					if (fn in seenAlready) {
						c = addScopeError(c,"Field name <fn> cannot be repeated in the same constructor", l);
					} else {
						seenAlready = seenAlready + fn;
						if (<adtId,fn> in c.adtFields) {
							if (!equivalent(ft, c.adtFields[<adtId,fn>])) {
								c = addScopeError(c,"Field <fn> already defined as type <prettyPrintType(c.adtFields[<adtId,fn>])> on datatype <prettyPrintName(adtName)>, cannot redefine to type <prettyPrintType(ft)>",l);
							}
						} else {
							c.adtFields[<adtId,fn>] = ft;
						}
					}
				}
			}
		}
	}
	
	// Add the constructor. This also performs an overlap check if this is not the first
	// constructor with this name -- note that we only check for overlaps with constructors
	// defined in the same ADT, since there is no language mechanism to distinguish them
	// when used in a program (we cannot prefix the constructor with the ADT name, for
	// instance).
	moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
	if (n notin c.fcvEnv) {
		// On the initial add, we create three versions of the constructor name:
		// * the bare name
		// * the name, qualified with the ADT name
		// * the name, qualified with the name of the module
		// This ensures that all valid lookups of the name are successful.
		c.fcvEnv[n] = c.nextLoc;
		c.fcvEnv[appendName(adtName,n)] = c.nextLoc;
		c.fcvEnv[appendName(moduleName,n)] = c.nextLoc;
		c.store[c.nextLoc] = constructor(n,rt,head([i | i <- c.stack, \module(_,_) := c.store[i]]),l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.adtConstructors = c.adtConstructors + < adtId, c.nextLoc >;
		c.nextLoc = c.nextLoc + 1;
	} else if (overload(items,overloaded(itemTypes)) := c.store[c.fcvEnv[n]]) {
		c.store[c.nextLoc] = constructor(n,rt,head([i | i <- c.stack, \module(_,_) := c.store[i]]),l);
		c.store[c.fcvEnv[n]] = overload(items + c.nextLoc, overloaded(itemTypes + rt));
		c.definitions = c.definitions + < c.nextLoc, l >;
		overlaps = { i | i <- c.adtConstructors[adtId], c.store[i].name == n, comparable(c.store[i].rtype,rt) };
		if (size(overlaps) > 0)
			c = addScopeError(c,"Constructor overlaps existing constructors in the same datatype",l);
		c.adtConstructors = c.adtConstructors + < adtId, c.nextLoc >;
		c.nextLoc = c.nextLoc + 1;
	} else if (constructor(_,_,_,_) := c.store[c.fcvEnv[n]]) {
		c.store[c.nextLoc] = constructor(n,rt,head([i | i <- c.stack, \module(_,_) := c.store[i]]),l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		overlaps = { i | i <- c.adtConstructors[adtId], c.store[i].name == n, comparable(c.store[i].rtype,rt) };
		if (size(overlaps) > 0)
			c = addScopeError(c,"Constructor overlaps existing constructors in the same datatype",l);
		c.adtConstructors = c.adtConstructors + < adtId, c.nextLoc >;
		c.store[c.nextLoc+1] = overload({ c.fcvEnv[n], c.nextLoc }, overloaded({ c.store[c.fcvEnv[n]].rtype, rt }));
		c.fcvEnv[n] = c.nextLoc+1;
		c.nextLoc = c.nextLoc + 2;
	} else {
		throw "Invalid addition: cannot add constructor into scope, it clashes with non-constructor variable or function names";
	}
	return c;
}

public Configuration addModule(Configuration c, RName n, loc l) {
	c.modEnv[n] = c.nextLoc;
	c.store[c.nextLoc] = \module(n,l);
	c.definitions = c.definitions + < c.nextLoc, l >;
	c.stack = c.nextLoc + c.stack;
	c.nextLoc = c.nextLoc + 1;
	return c;
}

public Configuration popModule(Configuration c) {
	if (\module(_,_) !:= c.store[head(c.stack)]) throw "Error, can only be called when a module is the top item on the stack";
	c.stack = tail(c.stack);
	return c;
}

public Configuration addClosure(Configuration c, Symbol rt, loc l) {
	c.store[c.nextLoc] = closure(rt,head(c.stack),l);
	c.definitions = c.definitions + < c.nextLoc, l >;
	c.stack = c.nextLoc + c.stack;
	c.nextLoc = c.nextLoc + 1;
	c.expectedReturnType = getFunctionReturnType(rt);
	return c;
}

public Configuration addFunction(Configuration c, RName n, Symbol rt, bool isVarArgs, Vis visibility, loc l) {
	// TODO: Handle the visibility properly. The main point is that we should not have variants
	// for the same function that are given different visibilities.
	// TODO: Verify the scoping is working properly for the second and third cases. It should be the
	// case that, if we cannot shadow, this means the name exists within the same function or
	// module. We may want to be stricter, though, and say that functions can only be defined within
	// a module or function scope, not (for instance) inside control flow.
	// TODO: Check for overlaps. But, we need to figure out if this is even possible anymore, we naturally
	// have overlaps because of the pattern-based dispatch.
	rt@isVarArgs = isVarArgs;

	if (n notin c.fcvEnv || varCanShadow(c,n)) {
		c.fcvEnv[n] = c.nextLoc;
		if (\module(_,_) := c.store[head(c.stack)]) {
			// If this function is module-level, also make it referenceable through
			// the qualified name module::function.
			moduleName = head([m | i <- c.stack, m:\module(_,_) := c.store[i]]).name;
			c.fcvEnv[appendName(moduleName,n)] = c.nextLoc;
		}
		c.store[c.nextLoc] = function(n,rt,isVarArgs,head(c.stack),l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.visibilities[c.nextLoc] = visibility;
		c.stack = c.nextLoc + c.stack;
		c.nextLoc = c.nextLoc + 1;
	} else if (overload(items, overloaded(itemTypes)) := c.store[c.fcvEnv[n]]) {
		c.store[c.nextLoc] = function(n,rt,isVarArgs,head(c.stack),l);
		c.store[c.fcvEnv[n]] = overload(items + c.nextLoc, overloaded(itemTypes + rt));
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.visibilities[c.nextLoc] = visibility;
		c.stack = c.nextLoc + c.stack;
		c.nextLoc = c.nextLoc + 1;
	} else if (function(_,_,_,_,_) := c.store[c.fcvEnv[n]]) {
		c.store[c.nextLoc] = function(n,rt,isVarArgs,head(c.stack),l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.visibilities[c.nextLoc] = visibility;
		c.stack = c.nextLoc + c.stack;
		c.store[c.nextLoc + 1] = overload({ c.fcvEnv[n], c.nextLoc }, overloaded({ c.store[c.fcvEnv[n]].rtype, rt }));
		c.fcvEnv[n] = c.nextLoc + 1;
		c.nextLoc = c.nextLoc + 2;
	} else {
		throw "Invalid addition: cannot add function <prettyPrintName(n)> at <l> into scope, it clashes with non-function variable or constructor names";
	}
	return c;
}

public Configuration addTag(Configuration c, TagKind tk, RName n, set[Symbol] onTypes, Vis visibility, loc l) {
	// TODO: We currently always treat datatype declarations as public, so we just
	// ignore the visibility here. If we decide to allow private datatype declarations,
	// revisit this.
	if (rn in c.tagEnv) {
		currentVal = c.store[c.tagEnv[n]];
		if (tk != currentVal.tkind) throw "Cannot add tag with same name but different kind into environment!";
		c.store[c.tagEnv[n]].onTypes = c.store[c.tagEnv[n]].onTypes + onTypes;
		c.definitions[c.tagEnv[n]] = c.definitions + < c.tagEnv[n], l >; 
	} else {
		c.tagEnv[n] = c.nextLoc;
		c.store[c.nextLoc] = \tag(n, tk, onTypes, head([i | i <- c.stack, \module(_,_) := c.store[i]]), l);
		c.definitions = c.definitions + < c.nextLoc, l >;
		c.nextLoc = c.nextLoc + 1;
	}
	
	return c;
}

@doc{Check to see if a var with name n can shadow. It always can if the name is not yet defined. If the name is defined,
     it can be shadowed if it was declared outside the current function (or module, if we are at the top level). }
public bool varCanShadow(Configuration c, RName n) {
	// If the name isn't in scope, we can always add it
	if (n notin c.fcvEnv) return true;
	
	// If the name is the same as the name of one of the function we are nested inside,
	// we can never add it.
	if (n in { fname | idx <- index(c.stack), function(fname,_,_,_,_) := c.store[c.stack[idx]] }) return false;

	// This gets the scopes defined back to the most recent function or module.	
	stackItems = toSet(head(c.stack,head([ idx | idx <- index(c.stack), function(_,_,_,_,_) := c.store[c.stack[idx]] || closure(_,_,_) := c.store[c.stack[idx]] || \module(_,_) := c.store[c.stack[idx]]])+1));

	av = c.store[c.fcvEnv[n]];
	if (overload(items,_) := av) {
		return isEmpty(stackItems & { i.containedIn | i <- items });
	} else {
		return av.containedIn notin stackItems;
	}
}

public Configuration assignVariableType(Configuration c, RName n, Symbol t) {
	c.store[c.fcvEnv[n]].rtype = t;
	return c;
}

public Configuration addScopeMessage(Configuration c, Message m) = c[messages = c.messages + m];

public Configuration addScopeError(Configuration c, str s, loc l) = addScopeMessage(c,error(s,l));
public Configuration addScopeWarning(Configuration c, str s, loc l) = addScopeMessage(c,warning(s,l));
public Configuration addScopeInfo(Configuration c, str s, loc l) = addScopeMessage(c,info(s,l));

@doc{Represents the result of checking an expression.}
alias CheckResult = tuple[Configuration conf, Symbol res];

@doc{Marks if a function is a var-args function.}
public anno bool Symbol@isVarArgs;

@doc{Marks the location(s) where a defined type (function, constructor, etc) is defined.}
public anno set[loc] Symbol@definedAt;

@doc{Strip the label off a symbol, if it has one at the top.}
private Symbol stripLabel(Symbol::\label(str s, Symbol t)) = stripLabel(t);
private default Symbol stripLabel(Symbol t) = t;

@doc{Check the types of Rascal expressions: NonEmptyBlock (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`{ <Statement+ ss> }`, Configuration c) {
	// TODO: Do we need to extract out function declarations first, or do they have to be in order here?
	for (s <- ss) < c, t1 > = checkStmt(s, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	return markLocationType(c,exp@\loc,t1);
}

@doc{Check the types of Rascal expressions: Bracket (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`( <Expression e> )`, Configuration c) {
	< c, t1 > = checkExp(e,c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	return markLocationType(c,exp@\loc,t1);
}

@doc{Check the types of Rascal expressions: Closure (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Type t> <Parameters ps> { <Statement+ ss> }`, Configuration c) {
	< c, rt > = convertAndExpandType(t,c);

	// Enter a new scope for the closure	
	Symbol funType = Symbol::\func(rt,[]);
	c2 = addClosure(c, funType, exp@\loc);
	
	// First process the paramter types to calculate the type of the closure
	< c2, ptTuple > = checkParameters(ps, c2);
	list[Symbol] parameterTypes = getTupleFields(ptTuple);
	paramFailures = { pt | pt <- parameterTypes, isFailType(pt) };
	if (size(paramFailures) > 0) {
		funType = collapseFailTypes(paramFailures + makeFailType("Could not calculate function type because of errors calculating the parameter types", exp@\loc));		
	} else {
		funType = makeFunctionTypeFromTuple(rt, false, \tuple(parameterTypes));
	}
	c2.store[head(c2.stack)].rtype = funType;
	
	// Now process the body
	for (s <- ss) < c2, st > = checkStmt(s, c2);
	
	// Recover the environment, removing any vars added for parameters or inside the body
	c = recoverEnvironmentsAfterCall(c2,c);

	if (isFailType(funType))
		return markLocationFailed(c, exp@\loc, funType); 
	else
		return markLocationType(c,exp@\loc, funType);
}

@doc{Check the types of Rascal expressions: StepRange (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`[ <Expression ef> , <Expression es> .. <Expression el> ]`, Configuration c) {
	< c, t1 > = checkExp(ef, c);
	< c, t2 > = checkExp(es, c);
	< c, t3 > = checkExp(el, c);

	if (!isFailType(t1) && !isFailType(t2) && !isFailType(t3) && subtype(t1,\num()) && subtype(t2,\num()) && subtype(t3,\num())) {
		return markLocationType(c,exp@\loc,\list(lubList([t1,t2,t3])));
	} else {
		if (!isFailType(t1) && !subtype(t1,\num())) t1 = makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t1)>", ef@\loc);
		if (!isFailType(t2) && !subtype(t2,\num())) t2 = makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t2)>", es@\loc);
		if (!isFailType(t3) && !subtype(t3,\num())) t3 = makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t3)>", el@\loc);
		return markLocationFailed(c,exp@\loc,{t1,t2,t3});
	}
}

@doc{Check the types of Rascal expressions: VoidClosure}
public CheckResult checkExp(Expression exp:(Expression)`<Parameters ps> { <Statement* ss> }`, Configuration c) {
	Symbol rt = \void();

	// Enter a new scope for the closure	
	Symbol funType = Symbol::\func(rt,[]);
	c2 = addClosure(c, funType, exp@\loc);
	
	// First process the paramter types to calculate the type of the closure
	< c2, ptTuple > = checkParameters(ps, c2);
	list[Symbol] parameterTypes = getTupleFields(ptTuple);
	paramFailures = { pt | pt <- parameterTypes, isFailType(pt) };
	if (size(paramFailures) > 0) {
		funType = collapseFailTypes(paramFailures + makeFailType("Could not calculate function type because of errors calculating the parameter types", exp@\loc));		
	} else {
		funType = makeFunctionTypeFromTuple(rt, false, \tuple(parameterTypes));
	}
	c2.store[head(c2.stack)].rtype = funType;
	
	// Now process the body
	for (s <- ss) < c2, st > = checkStmt(s, c2);
	
	// Recover the environment, removing any vars added for parameters or inside the body
	c = recoverEnvironmentsAfterCall(c2,c);

	if (isFailType(funType))
		return markLocationFailed(c, exp@\loc, funType); 
	else
		return markLocationType(c,exp@\loc, funType);
}

@doc{Check the types of Rascal expressions: Visit (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Label l> <Visit v>`, Configuration c) {
	if ((Label)`<Name n> :` := l) {
		// Add checks involving label environment
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", l@\loc));
		c2 = addLabel(c,labelName,l@\loc,visitLabel());
		c2.labelStack = < labelName, visitLabel(), \void() > + c2.labelStack;
		< c2, vt > = checkVisit(v,c2);
		// Recovering the environment removes the label from the environment
		c2.labelStack = tail(c2.labelStack);
		c = recoverEnvironments(c2,c);
		if (isFailType(vt)) return markLocationFailed(c,exp@\loc,vt);
		return markLocationType(c,exp@\loc,vt);
	} else {
		c.labelStack = < RSimpleName(""), visitLabel(), \void() > + c.labelStack;
		< c, vt > = checkVisit(v,c);
		c.labelStack = tail(c.labelStack);
		if (isFailType(vt)) return markLocationFailed(c,exp@\loc,vt);
		return markLocationType(c,exp@\loc,vt);
	} 
}

@doc{Check the types of Rascal expressions: Reducer (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`( <Expression ei> | <Expression er> | <{Expression ","}+ egs> )`, Configuration c) {
	// Check the initializer and the generators, both of which run without "it" in scope
	< c, t1 > = checkExp(ei, c);
	list[Symbol] ts = [];
	for (eg <- egs) { < c, t2 > = checkExp(eg,c); ts += t2; }
	
	// If the initializer isn't fail, introduce the variable "it" into scope; it is 
	// available in er (the result), but not in the rest, and we need it to check er.
	// Note that this means we cannot check er if we cannot assign an initial type to
	// "it", since we have no information on which to base a reasonable assumption. 
	Symbol erType = t1;
	if (!isFailType(t1)) {
		c2 = addVariable(c, RSimpleName("it"), true, exp@\loc, erType);
		< c2, t3 > = checkExp(er, c2);
		if (!isFailType(t3)) {
			if (!equivalent(erType,t3) && lub(erType,t3) == t3) {
				erType = t3;
				c2 = assignVariableType(c2, RSimpleName("it"), erType);
				< c2, t3 > = checkExp(er, c2);
				if (!isFailType(t3)) {
					if (!equivalent(erType,t3)) {
						erType = makeFailType("Type of it does not stabilize", exp@\loc);
						c2 = assignVariableType(c2, RSimpleName("it"), erType);
					}
				} else {
					erType = t3;
				}
			}
		} else {
			erType = t3;
		}
		// Remove "it" from the scope again
		c = recoverEnvironments(c2,c);
	}

	// Calculate the final type. If we had failures, it is a failure, else it
	// is the type of the reducer step.
	failTypes = { t | t <- (ts + t1 + erType), isFailType(t) };
	if (size(failTypes) > 0) {
		return markLocationFailed(c,exp@\loc,collapseFailTypes(failTypes));
	} else {
		return markLocationType(c,exp@\loc,erType);
	}
}

@doc{Check the types of Rascal expressions: ReifiedType (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`type ( <Expression es> , <Expression ed> )`, Configuration c) {
	// TODO: Is there anything we can do statically to make the result type more accurate?
	< c, t1 > = checkExp(es, c);
	< c, t2 > = checkExp(ed, c);
	if (!isFailType(t1) && !subtype(t1,\adt("Symbol",[])))
		t1 = makeFailType("Expected subtype of Symbol, instead found <prettyPrintType(t1)>",es@\loc);
	if (!isFailType(t1) && !subtype(t2,\map(\adt("Symbol",[]),\adt("Production",[]))))
		t2 = makeFailType("Expected subtype of map[Symbol,Production], instead found <prettyPrintType(t2)>",ed@\loc);
	if (isFailType(t1) || isFailType(t2))
		return markLocationFailed(c,exp@\loc,collapseFailTypes({t1,t2}));
	else
		return markLocationType(c,exp@\loc,\type(\value()));
}

@doc{Check the types of Rascal expressions: CallOrTree}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> ( <{Expression ","}* eps> )`, Configuration c) {
	list[Expression] epsList = [ epsi | epsi <- eps ];
	< c, t1 > = checkExp(e, c);
	list[Symbol] tl = [];
	for (ep <- eps) { < c, t2 > = checkExp(ep, c); tl += t2; }
	
	set[Symbol] matchAlts(set[Symbol] alts) {
		set[Symbol] matches = { };
		for (a <- alts) {
			if (isFunctionType(a)) {
				list[Symbol] args = getFunctionArgumentTypes(a);
				// NOTE: We cannot assume the annotation is set, since we only set it when we add a
				// function (and have the info available); we don't have the information when we only
				// have a function type, such as with a function parameter.
				bool varArgs = ( ((a@isVarArgs)?) ? a@isVarArgs : false );
				if (!varArgs) {
					if (size(epsList) == size(args) && size(epsList) == 0) {
						matches += a;
					} else if (size(epsList) == size(args) && all(idx <- index(epsList), subtype(tl[idx],args[idx]))) {
						matches += a;
					}
				} else {
					if (size(epsList) >= size(args)-1) {
						if (size(epsList) == 0) {
							matches += a;
						} else {
							list[Symbol] fixedPart = head(tl,size(args)-1);
							list[Symbol] varPart = tail(tl,size(tl)-size(args)+1);
							list[Symbol] fixedArgs = head(args,size(args)-1);
							Symbol varArgsType = getListElementType(last(args));
							if (size(fixedPart) == 0 && all(idx <- index(varPart),subtype(varPart[idx],varArgsType)))
								matches += a;
							else if (all(idx <- index(fixedPart), subtype(fixedPart[idx],fixedArgs[idx])) && all(idx2 <- index(varPart),subtype(varPart[idx2],varArgsType)))
								matches += a;
						}
					}
				}
			} else {
				list[Symbol] args = getConstructorArgumentTypes(a);
				if (size(epsList) == size(args) && size(epsList) == 0) {
					matches += a;
				} else if (size(epsList) == size(args) && all(idx <- index(epsList), subtype(tl[idx],args[idx]))) {
					matches += a;
				}
			}
		}
		return matches;
	}
	
	// check for failures
	set[Symbol] failures = { };
	
	// e was either a name or an expression that evaluated to a function, a constructor,
	// a source location, or a string
	if (isFunctionType(t1) || isConstructorType(t1) || isOverloadedType(t1)) {
		set[Symbol] alts = (isFunctionType(t1) || isConstructorType(t1)) ? { t1 } : getOverloadOptions(t1);
		set[Symbol] matches = matchAlts(alts);
		if (size(matches) == 0) {
			return markLocationFailed(c,exp@\loc,makeFailType("No matching functions or constructors found",exp@\loc));
		} else if (size(matches) > 1) {
			return markLocationFailed(c,exp@\loc,makeFailType("Multiple functions or constructors found which could be applied",exp@\loc));
		} else {
			rt = getOneFrom(matches);
			if (isFunctionType(rt)) {
				if (typeContainsTypeVars(rt)) {
					// If the function is parametric, we need to calculate the actual types of the
					// parameters and make sure they fall within the proper bounds.
					formalArgs = getFunctionArgumentTypes(rt);
					bool varArgs = ( ((rt@isVarArgs)?) ? rt@isVarArgs : false );
					set[Symbol] typeVars = { *collectTypeVars(fa) | fa <- formalArgs };
					map[str,Symbol] bindings = ( getTypeVarName(tv) : \void() | tv <- typeVars );
					
					bool cannotInstantiate = false;
					if (!varArgs) {
						// First try to get the bindings between the type vars and the actual types for each of the
						// function parameters. Here this is not a varargs function, so there are the same number of
						// formals as actuals.
						for (idx <- index(tl)) {
							try {
								bindings = match(formalArgs[idx],tl[idx],bindings);
							} catch : {
								c = addScopeError(c,"Cannot instantiate parameter <idx+1>, parameter type <prettyPrintType(tl[idx])> violates bound of type parameter in formal argument with type <prettyPrintType(formalArgs[idx])>", epsList[idx]@\loc);
								cannotInstantiate = true;  
							}
						}
					} else {
						// Get the bindings between the type vars and the actual types for each function parameter. Since
						// this is a var-args function, we need to take that into account. The first for loop takes care
						// of the fixes parameters, while the second takes care of those that are mapped to the var-args
						// parameter.
						for (idx <- index(tl), idx < size(formalArgs)) {
							try {
								bindings = match(formalArgs[idx],tl[idx],bindings);
							} catch : {
								c = addScopeError(c,"Cannot instantiate parameter <idx+1>, parameter type <prettyPrintType(tl[idx])> violates bound of type parameter in formal argument with type <prettyPrintType(formalArgs[idx])>", epsList[idx]@\loc);
								cannotInstantiate = true;  
							}
						}
						for (idx <- index(tl), idx >= size(formalArgs)) {
							try {
								bindings = match(getListElementType(formalArgs[size(formalArgs)-1]),tl[idx],bindings);
							} catch : {
								c = addScopeError(c,"Cannot instantiate parameter <idx+1>, parameter type <prettyPrintType(tl[idx])> violates bound of type parameter in formal argument with type <prettyPrintType(getListElementType(formalArgs[size(formalArgs)-1]))>", epsList[idx]@\loc);
								cannotInstantiate = true;  
							}
						}
					}
					// Based on the above, either give an error message (if we could not match the function's parameter types) or
					// try to instantiate the entire function type. The instantiation should only fail if we cannot instantiate
					// the return type correctly, for instance if the instantiation would violate the bounds.
					// NOTE: We may instantiate and still have type parameters, since we may be calling this from inside
					// a function, using a value with a type parameter as its type.
					if (cannotInstantiate) {
						return markLocationFailed(c,exp@\loc,makeFailType("Cannot instantiate type parameters in function invocation", exp@\loc));
					} else {
						try {
							//println("Attempting to instantiate type <rt> with bindings <bindings>");
							rt = instantiate(rt, bindings);
							//println("Instantiated as <rt>");
						} catch : {
							return markLocationFailed(c,exp@\loc,makeFailType("Cannot instantiate type parameters in function invocation", exp@\loc));
						}
					}
				} 
				< c, rtp > = markLocationType(c,e@\loc,rt);
				return markLocationType(c,exp@\loc,getFunctionReturnType(rt));
			} else {
				if (typeContainsTypeVars(rt)) {
					// If the constructor is parametric, we need to calculate the actual types of the
					// parameters and make sure they fall within the proper bounds.
					formalArgs = getConstructorArgumentTypes(rt);
					set[Symbol] typeVars = { collectTypeVars(fa) | fa <- formalArgs };
					map[str,Symbol] bindings = ( getTypeVarName(tv) : \void() | tv <- typeVars );
					bool cannotInstantiate = false;
					for (idx <- index(tl)) {
						try {
							bindings = match(formalArgs[idx],tl[idx],bindings);
						} catch : {
							c = addScopeError(c,"Cannot instantiate parameter <idx+1>, parameter type <prettyPrintType(tl[idx])> violates bound of type parameter in formal argument with type <prettyPrintType(formalArgs[idx])>", epsList[idx]@\loc);
							cannotInstantiate = true;  
						}
					}
					if (cannotInstantiate) {
						return markLocationFailed(c,exp@\loc,makeFailType("Cannot instantiate type parameters in constructor", exp@\loc));
					} else {
						try {
							//println("Attempting to instantiate type <rt> with bindings <bindings>");
							rt = instantiate(rt, bindings);
							//println("Instantiated as <rt>");
						} catch : {
							return markLocationFailed(c,exp@\loc,makeFailType("Cannot instantiate type parameters in constructor", exp@\loc));
						}
					}
				}
				< c, rtp > = markLocationType(c,e@\loc,rt);
				return markLocationType(c,exp@\loc,getConstructorResultType(rt));
			}		
		}
	} else if (isLocType(t1)) {
		if (size(tl) == 4) {
			// We are expecting a signature of int, int, tuple[int,int], tuple[int,int], make sure we got it
			if (!isIntType(tl[0])) 
				failures += makeFailType("Expected int, found <prettyPrintType(tl[0])>", epsList[0]@\loc); 	
						
			if (!isIntType(tl[1])) 
				failures += makeFailType("Expected int, found <prettyPrintType(tl[1])>", epsList[1]@\loc);
				 			
			if (!isTupleType(tl[2])) {
				failures += makeFailType("Expected tuple[int,int], found <prettyPrintType(tl[2])>", epsList[2]@\loc);
			} else {
				tf1 = getTupleFields(tl[2]);
				if (!(size(tf1) == 2 && isIntType(tf1[0]) && isIntType(tf1[1])))
					failures += makeFailType("Expected tuple[int,int], found <prettyPrintType(tl[2])>", epsList[2]@\loc);
			}	
				
			if (!isTupleType(tl[3])) { 
				failures += makeFailType("Expected tuple[int,int], found <prettyPrintType(tl[3])>", epsList[3]@\loc);
			} else {
				tf2 = getTupleFields(tl[3]);
				if (!(size(tf2) == 2 && isIntType(tf2[0]) && isIntType(tf2[1])))
					failures += makeFailType("Expected tuple[int,int], found <prettyPrintType(tl[2])>", epsList[2]@\loc);
			} 			
		} else {
			failures += makeFailType("Expected 4 arguments: int, int, tuple[int,int], and tuple[int,int]", exp@\loc); 
		}
		
		if (size(failures) > 0)
			return markLocationFailed(c,exp@\loc,failures);
		else
			return markLocationType(c,exp@\loc,\loc());
	} else if (isStrType(t1)) {
		return markLocationType(c,exp@\loc,\node());
	}
	
	return markLocationFailed(c,exp@\loc,makeFailType("Cannot use type <prettyPrintType(t1)> in calls", exp@\loc)); 
}

@doc{Check the types of Rascal expressions: Literal (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Literal l>`, Configuration c) {
	return checkLiteral(l, c);
}

@doc{Check the types of Rascal expressions: Any (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`any ( <{Expression ","}+ egs> )`, Configuration c) {
	set[Symbol] failures = { };
	for (eg <- egs) { 
		< c, t1 > = checkExp(eg,c);
		if (isFailType(t1)) {
			failures += t1;
		} else if (!isBoolType(t1)) {
			failures += makeFailType("Expected type bool, found <prettyPrintType(t1)>", eg@\loc);
		} 
	}
	if (size(failures) > 0) return markLocationFailed(c, exp@\loc, collapseFailTypes(failures));
	return markLocationType(c, exp@\loc, \bool());
}

@doc{Check the types of Rascal expressions: All (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`all ( <{Expression ","}+ egs> )`, Configuration c) {
	set[Symbol] failures = { };
	for (eg <- egs) { 
		< c, t1 > = checkExp(eg,c);
		if (isFailType(t1)) {
			failures += t1;
		} else if (!isBoolType(t1)) {
			failures += makeFailType("Expected type bool, found <prettyPrintType(t1)>", eg@\loc);
		} 
	}
	if (size(failures) > 0) return markLocationFailed(c, exp@\loc, collapseFailTypes(failures));
	return markLocationType(c, exp@\loc, \bool());
}

@doc{Check the types of Rascal expressions: Comprehension (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Comprehension cp>`, Configuration c) {
	return checkComprehension(cp, c);
}

@doc{Check the types of Rascal expressions: Set (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`{ <{Expression ","}* es> }`, Configuration c) {
	list[Symbol] tl = [ \void() ];
	for (e <- es) { < c, t1 > = checkExp(e,c); tl += t1; }
	if (all(t <- tl, !isFailType(t))) {
		return markLocationType(c, exp@\loc, \set(lubList(tl)));
	} else {
		return markLocationFailed(c, exp@\loc, {t|t<-tl});
	}
}

@doc{Check the types of Rascal expressions: List (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`[ <{Expression ","}* es> ]`, Configuration c) {
	list[Symbol] tl = [ \void() ];
	for (e <- es) { < c, t1 > = checkExp(e,c); tl += t1; }
	if (all(t <- tl, !isFailType(t))) {
		return markLocationType(c, exp@\loc, \list(lubList(tl)));
	} else {
		return markLocationFailed(c, exp@\loc, {t|t<-tl});
	}
}

@doc{Check the types of Rascal expressions: ReifyType (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`# <Type t>`, Configuration c) {
	< c, rt > = convertAndExpandType(t,c);
	return markLocationType(c, exp@\loc, \type(rt));
}

@doc{Check the types of Rascal expressions: Range (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`[ <Expression ef> .. <Expression el> ]`, Configuration c) {
	< c, t1 > = checkExp(ef, c);
	< c, t2 > = checkExp(el, c);
	
	if (!isFailType(t1) && !isFailType(t2) && subtype(t1,\num()) && subtype(t2,\num())) {
		return markLocationType(c,exp@\loc,\list(lubList([t1,t2])));
	} else {
		if (!subtype(t1,\num())) t1 = makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t1)>", ef@\loc);
		if (!subtype(t2,\num())) t2 = makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t2)>", el@\loc);
		return markLocationFailed(c,exp@\loc,{t1,t2});
	}
}

@doc{Check the types of Rascal expressions: Tuple (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`< <Expression e1>, <{Expression ","}* es> >`, Configuration c) {
	// TODO: This doesn't work right now because of a bug in the parser or the matcher.
	// So, we will need to match over the AsFix representation instead.
	< c, t1 > = checkExp(e1, c);
	list[Symbol] tl = [ t1 ];
	for (e <- es) { < c, t2 > = checkExp(e,c); tl += t2; }
	if (all(t <- tl, !isFailType(t))) {
		return markLocationType(c, exp@\loc, \tuple(tl));
	} else {
		return markLocationFailed(c, exp@\loc, {t|t<-tl});
	}
}

@doc{Check the types of Rascal expressions: Map (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`( <{Mapping[Expression] ","}* mes> )`, Configuration c) {
	list[Symbol] td = [ \void() ];
	list[Symbol] tr = [ \void() ];
	set[Symbol] failures = { };
	
	for ((Mapping[Expression])`<Expression ed> : <Expression er>` <- mes) {
		< c, t1 > = checkExp(ed, c);
		< c, t2 > = checkExp(er, c);

		if (isFailType(t1)) 
			failures += t1;
		else
			td += t1;

		if (isFailType(t2)) 
			failures += t2;
		else
			tr += t2;
	}
	
	if (size(failures) > 0)
		return markLocationFailed(c, exp@\loc, failures);
	else
		return markLocationType(c, exp@\loc, \map(lubList(td),lubList(tr)));
}

@doc{Check the types of Rascal expressions: it (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`it`, Configuration c) {
	if (fcvExists(c, RSimpleName("it"))) {
		c.uses = c.uses + < c.fcvEnv[RSimpleName("it")], exp@\loc >;
		return markLocationType(c, exp@\loc, c.store[c.fcvEnv[RSimpleName("it")]].rtype);
	} else {
		return markLocationFailed(c, exp@\loc, makeFailType("Name it not in scope", exp@\loc));
	}
}

@doc{Check the types of Rascal expressions: QualifiedName}
public CheckResult checkExp(Expression exp:(Expression)`<QualifiedName qn>`, Configuration c) {
	// TODO: Need to handle qualified names (i.e., names with ::) as well. This will just handle
	// regular names without qualifiers.
	n = convertName(qn);
	if (fcvExists(c, n)) {
		c.uses = c.uses + < c.fcvEnv[n], exp@\loc >;
		return markLocationType(c, exp@\loc, c.store[c.fcvEnv[n]].rtype);
	} else {
		println("Name <prettyPrintName(n)> was not found, here is the scope: <c.fcvEnv>");
		return markLocationFailed(c, exp@\loc, makeFailType("Name is not in scope", exp@\loc));
	}
}

@doc{Check the types of Rascal expressions: Subscript (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> [ <{Expression ","}+ es> ]`, Configuration c) {
	list[Symbol] tl = [ ];
	set[Symbol] failures = { };
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) failures = failures + t1;
	for (esi <- es) {
		// Subscripts can also use the "_" character, which means to ignore that position; we do
		// that here by treating it as \value(), which is comparable to all other types and will
		// thus work when calculating the type below.
		if ((Expression)`_` := esi) {
			tl += \value();
		} else { 
			< c, t2 > = checkExp(esi,c); 
			tl += t2; 
			if (isFailType(t2)) failures = failures + t2;
		} 
	}
	if (size(failures) > 0) {
		// If we do not have valid types for e or the subscripting expressions, we cannot compute
		// the type properly, so return right away with the failures. 
		return markLocationFailed(c, exp@\loc, failures);
	}
	if (isListType(t1)) {
		if (size(tl) != 1)
			return markLocationFailed(c,exp@\loc,makeFailType("Expected only 1 subscript for a list expression, not <size(tl)>",exp@\loc));
		else if (!isIntType(tl[0]))
			return markLocationFailed(c,exp@\loc,makeFailType("Expected subscript of type int, not <prettyPrintType(tl[0])>",exp@\loc));
		else
			return markLocationType(c,exp@\loc,getListElementType(t1));
	} else if (isRelType(t1)) {
		if (size(tl) >= size(getRelFields(t1)))
			return markLocationFailed(c,exp@\loc,makeFailType("For a relation with arity <size(getRelFields(t1))> you can have at most <size(getRelFields(t1))-1> subscripts",exp@\loc));
		else {
			relFields = getRelFields(t1);
			failures = { makeFailType("At subscript <idx+1>, subscript type <prettyPrintType(tl[idx])> must be comparable to relation field type <prettyPrintType(relFields[idx])>", exp@\loc) | idx <- index(tl), !comparable(tl[idx],relFiels[idx]) };
			if (size(failures) > 0)
				return markLocationFailed(c,exp@\loc,failures);
			else if ((size(relFields) - size(tl)) == 1)
				return markLocationType(c,exp@\loc,\set(last(relFields)));
			else
				return markLocationType(c,exp@\loc,\rel(tail(relFields,size(relFields)-size(tl))));
		}
	} else if (isMapType(t1)) {
		if (size(tl) != 1)
			return markLocationFailed(c,exp@\loc,makeFailType("Expected only 1 subscript for a map expression, not <size(tl)>",exp@\loc));
		else if (!equivalent(tl[0],getMapDomainType(t1)))
			return markLocationFailed(c,exp@\loc,makeFailType("Expected subscript of type <prettyPrintType(getMapDomainType(t1))>, not <prettyPrintType(tl[0])>",exp@\loc));
		else
			return markLocationType(c,exp@\loc,getMapRangeType(t1));
	} else if (isNodeType(t1)) {
		if (size(tl) != 1)
			return markLocationFailed(c,exp@\loc,makeFailType("Expected only 1 subscript for a node expression, not <size(tl)>",exp@\loc));
		else if (!isIntType(tl[0]))
			return markLocationFailed(c,exp@\loc,makeFailType("Expected subscript of type int, not <prettyPrintType(tl[0])>",exp@\loc));
		else
			return markLocationType(c,exp@\loc,\value());
	} else if (isTupleType(t1)) {
		if (size(tl) != 1)
			return markLocationFailed(c,exp@\loc,makeFailType("Expected only 1 subscript for a tuple expression, not <size(tl)>",exp@\loc));
		else if (!isIntType(tl[0]))
			return markLocationFailed(c,exp@\loc,makeFailType("Expected subscript of type int, not <prettyPrintType(tl[0])>",exp@\loc));
		else
			return markLocationType(c,exp@\loc,lubList(getTupleFields(t1)));
	} else if (isStrType(t1)) {
		if (size(tl) != 1)
			return markLocationFailed(c,exp@\loc,makeFailType("Expected only 1 subscript for a string expression, not <size(tl)>",exp@\loc));
		else if (!isIntType(tl[0]))
			return markLocationFailed(c,exp@\loc,makeFailType("Expected subscript of type int, not <prettyPrintType(tl[0])>",exp@\loc));
		else
			return markLocationType(c,exp@\loc,\str());
	} else {
		return markLocationFailed(c,exp@\loc,makeFailType("Expressions of type <prettyPrintType(t1)> cannot be subscripted", exp@\loc));
	}
}

@doc{Field names and types for built-ins}
private map[Symbol,map[str,Symbol]] fieldMap =
    ( \loc() :
        ( "scheme" : \str(), "authority" : \str(), "host" : \str(), "path" : \str(), "parent" : \loc(),
          "file" : \str(), "ls" : \list(\loc()), "extension" : \str(), "fragment" : \str(), 
          "query" : \str(), "user" : \str(), "port" : \int(), "length" : \int(), "offset" : \int(), 
          "begin" : \tuple([\label("line",\int()),\label("column",\int())]), 
          "end" : \tuple([\label("line",\int()),\label("column",\int())]), "uri" : \str()
        ),
      \datetime() :
        ( "year" : \int(), "month" : \int(), "day" : \int(), "hour" : \int(), "minute" : \int(), 
          "second" : \int(), "millisecond" : \int(), "timezoneOffsetHours" : \int(), 
          "timezoneOffsetMinutes" : \int(), "century" : \int(), "isDate" : \bool(), 
          "isTime" : \bool(), "isDateTime" : \bool(), "justDate" : \datetime(), "justTime" : \datetime()
        )
    );

private rel[Symbol,str] writableFields = ({ \loc() } * { "uri","scheme","authority","host","path","file","parent","extension","fragment","query","user","port","length","offset","begin","end" })
									   + ({ \datetime() } * { "year", "month", "day", "hour", "minute", "second", "millisecond","timezoneOffsetHours", "timezoneOffsetMinutes" });
									   
@doc{Check the types of Rascal expressions: Field Access (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> . <Name f>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	
	// If the type is a failure, we don't know how to look up the field name, so just return
	// right away.
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);

	return markLocationType(c, exp@\loc, computeFieldType(t1, convertName(f), exp@\loc, c));
}

public Symbol computeFieldType(Symbol t1, RName fn, loc l, Configuration c) {
	fAsString = prettyPrintName(fn);
	if (isLocType(t1)) {
		if (fAsString in fieldMap[\loc()])
			return fieldMap[\loc()][fAsString];
		else
			returnmakeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
	} else if (isDateTimeType(t1)) {
		if (fAsString in fieldMap[\datetime()])
			return fieldMap[\datetime()][fAsString];
		else
			return makeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
	} else if (isRelType(t1)) {
		rt = getRelElementType(t1);
		if (tupleHasField(rt, fAsString))
			return getTupleFieldType(rt, fAsString);
		else
			return makeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
	} else if (isMapType(t1)) {
		rt = getMapFieldsAsTuple(t1);
		if (tupleHasField(rt, fAsString))
			return getTupleFieldType(rt, fAsString);
		else
			return makeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
	} else if (isADTType(t1)) {
		adtName = RSimpleName(getADTName(t1));
		if (<c.typeEnv[adtName],fAsString> notin c.adtFields)
			return makeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
		else
			return c.adtFields[<c.typeEnv[adtName],fAsString>];  
	} else if (isTupleType(t1)) {
		if (tupleHasField(t1, fAsString))
			return getTupleFieldType(t1, fAsString);
		else
			return makeFailType("Field <fAsString> does not exist on type <prettyPrintType(t1)>", l);
	} 

	return makeFailType("Cannot access fields on type <prettyPrintType(t1)>", l);
}

@doc{Check the types of Rascal expressions: Field Update (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> [ <Name n> = <Expression er> ]`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	< c, t2 > = checkExp(er, c);
	
	// If the type of e is a failure, we don't know how to look up the field name, so just return
	// right away. t2 may have failures as well, so include that in the failure marking.
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,{t1,t2});
	
	// Now get the field type. If this fails, return right away as well.
	ft = computeFieldType(t1, convertName(n), exp@\loc, c);
	if (isFailType(t2) || isFailType(ft)) return markLocationFailed(c,exp@\loc,{t2,ft});
	if ((isLocType(t1) || isDateTimeType(t1)) && "<n>" notin writableFields[t1])
		return markLocationFailed(c,exp@\loc,makeFailType("Cannot update field <n> on type <prettyPrintType(t1)>",exp@\loc)); 

	// To assign, the type of er (t2) must be a subtype of the type of the field (ft)	
	if (!subtype(t2,ft)) return markLocationFailed(c,exp@\loc,makeFailType("Cannot assign type <prettyPrintType(t2)> into field of type <prettyPrintType(ft)>",exp@\loc));

	return markLocationType(c, exp@\loc, t1);
}

@doc{Check the types of Rascal expressions: Field Project (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> < <{Field ","}+ fs> >`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	
	// If the type is a failure, we don't know how to look up the field name, so just return
	// right away.
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);

	// Get back the fields as a tuple, if this is one of the allowed subscripting types.
	Symbol rt = \void();
	if (isRelType(t1)) {
		rt = getRelElementType(t1);
	} else if (isMapType(t1)) {
		rt = getMapFieldsAsTuple(t1);
	} else if (isTupleType(t1)) {
		rt = t1;
	} else {
		return markLocationFailed(c, exp@\loc, makeFailType("Type <prettyPrintType(t1)> does not allow fields", exp@\loc));
	}
	
	// Get back the types of each of the subscripts
	// TODO: Need to enforce uniqueness of field labels
	set[Symbol] failures = { };
	list[Symbol] subscripts = [ ];
	for (f <- fs) {
		if ((Field)`<IntegerLiteral il>` := f) {
			int offset = toInt("<il>");
			if (!tupleHasField(rt, offset))
				failures += makeFailType("Field subscript <il> out of range", f@\loc);
			else
				subscripts += getTupleFieldType(rt, offset);
		} else if ((Field)`<Name fn>` := f) {
			if (tupleHasField(rt, "<fn>"))
				failures += makeFailType("Field <prettyPrintName(fn)> does not exist", f@\loc);
			else
				subscripts += getTupleFieldType(rt, "<fn>");
		} else {
			throw "Unhandled field case: <f>";
		}
	}
	
	if (size(failures) > 0) return markLocationFailed(c, exp@\loc, failures);

	if (isRelType(t1)) {
		if (size(subscripts) > 1) return markLocationType(c, exp@\loc, \rel(subscripts));
		return markLocationType(c, exp@\loc, \set(head(subscripts)));
	} else if (isMapType(t1)) {
		if (size(subscripts) > 1) return markLocationType(c, exp@\loc, \rel(subscripts));
		return markLocationType(c, exp@\loc, \set(head(subscripts)));
	} else if (isTupleType(t1)) {
		if (size(subscripts) > 1) return markLocationType(c, exp@\loc, \tuple(subscripts));
		return markLocationType(c, exp@\loc, head(subscripts));
	}	
}

@doc{Check the types of Rascal expressions: Set Annotation (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> [ @ <Name n> = <Expression er> ]`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	< c, t2 > = checkExp(er, c);

	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isNodeType(t1) || isADTType(t1)) {
		aname = convertName(n);
		if (aname in c.annotationEnv, true in { subtype(t1,ot) | ot <- c.store[c.annotationEnv[aname]].onTypes }) {
			aType = c.store[c.annotationEnv[aname]].rtype;
			if (isFailType(aType)) {
				return markLocationFailed(c,exp@\loc,aType);
			} else {
				if (subtype(t2,aType)) {
					return markLocationType(c,exp@\loc,t1);
				} else {
					return markLocationFailed(c,exp@\loc,makeFailType("Cannot assign value of type <prettyPrintType(t2)> to annotation of type <prettyPrintType(aType)>", exp@\loc));
				}
			}
		} else {
			return markLocationFailed(c,exp@\loc,makeFailType("Annotation <n> not declared on <prettyPrintType(t1)> or its supertypes",exp@\loc));
		}
	} else {
		return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected node or ADT types, found <prettyPrintType(t1)>", e@\loc));
	}
}

@doc{Check the types of Rascal expressions: Get Annotation (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> @ <Name n>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	if (isNodeType(t1) || isADTType(t1)) {
		aname = convertName(n);
		if (aname in c.annotationEnv, true in { subtype(t1,ot) | ot <- c.store[c.annotationEnv[aname]].onTypes }) {
			aType = c.store[c.annotationEnv[aname]].rtype;
			if (isFailType(aType))
				return markLocationFailed(c,exp@\loc,aType);
			else
				return markLocationType(c,exp@\loc,aType);
		} else {
			return markLocationFailed(c,exp@\loc,makeFailType("Annotation <n> not declared on <prettyPrintType(t1)> or its supertypes",exp@\loc));
		}
	} else {
		return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected node or ADT types, found <prettyPrintType(t1)>", e@\loc));
	}
}

@doc{Check the types of Rascal expressions: Is (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> is <Name n>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	if (isNodeType(t1) || isADTType(t1)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected node or ADT types, found <prettyPrintType(t1)>", e@\loc));
}

@doc{Check the types of Rascal expressions: Has (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> has <Name n>`, Configuration c) {
	// TODO: Do we need to do anything with the name? We don't need to check for existence, that's the
	// whole point of the has expression.
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	if (isRelType(t1) || isTupleType(t1) || isADTType(t1)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected relation, tuple, or ADT types, found <prettyPrintType(t1)>", e@\loc));
}

@doc{Check the types of Rascal expressions: Transitive Closure (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> +`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);

	if (isRelType(t1)) {
		list[Symbol] flds = getRelFields(t1);
		if (size(flds) == 0) {
			return markLocationType(c,exp@\loc,t1);
		} else if (size(flds) == 2 && equivalent(flds[0],flds[1])) {	
			return markLocationType(c,exp@\loc,t1);
		} else {
			t1 = makeFailType("Invalid type: expected a binary relation over equivalent types, found <prettyPrintType(t1)>", e@\loc);
			return markLocationFailed(c,exp@\loc,t1);
		}
	} else {
		t1 = makeFailType("Invalid type: expected a binary relation, found <prettyPrintType(t1)>", e@\loc);
		return markLocationFailed(c,exp@\loc,t1);
	}
}

@doc{Check the types of Rascal expressions: Transitive Reflexive Closure (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> *`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);

	if (isRelType(t1)) {
		list[Symbol] flds = getRelFields(t1);
		if (size(flds) == 0) {
			return markLocationType(c,exp@\loc,t1);
		} else if (size(flds) == 2 && equivalent(flds[0],flds[1])) {	
			return markLocationType(c,exp@\loc,t1);
		} else {
			t1 = makeFailType("Invalid type: expected a binary relation over equivalent types, found <prettyPrintType(t1)>", e@\loc);
			return markLocationFailed(c,exp@\loc,t1);
		}
	} else {
		t1 = makeFailType("Invalid type: expected a binary relation, found <prettyPrintType(t1)>", e@\loc);
		return markLocationFailed(c,exp@\loc,t1);
	}
}

@doc{Check the types of Rascal expressions: Is Defined (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e> ?`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c,exp@\loc,t1);
	return markLocationType(c,exp@\loc,\bool());
}

@doc{Check the types of Rascal expressions: Negation (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`! <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	if (isBoolType(t1)) return markLocationType(c,exp@\loc,t1);
	return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected bool, found <prettyPrintType(t1)>", e@\loc));
}

@doc{Check the types of Rascal expressions: Negative (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`- <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	if (isIntType(t1) || isRealType(t1) || isRatType(t1) || isNumType(t1)) return markLocationType(c,exp@\loc,t1);
	return markLocationFailed(c,exp@\loc,makeFailType("Invalid type: expected numeric type, found <prettyPrintType(t1)>", e@\loc));
}

@doc{Check the types of Rascal expressions: Splice (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`* <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	if (isListType(t1)) return markLocationType(c, exp@\loc, getListElementType(t1));
	if (isSetType(t1)) return markLocationType(c, exp@\loc, getSetElementType(t1));
	if (isBagType(t1)) return markLocationType(c, exp@\loc, getBagElementType(t1));
	if (isRelType(t1)) return markLocationType(c, exp@\loc, getRelElementType(t1));
	return markLocationType(c, exp@\loc, t1);
}

@doc{Check the types of Rascal expressions: AsType (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`[ <Type t> ] <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	
	// TODO: Currently the interpreter verifies this is a non-terminal type, but this just
	// shows up in the type system as being another ADT. Should we keep a separate non-terminal
	// type, or somehow mark the ADT to indicate it is produced from a non-terminal? This could
	// also be done by making an entry in the symbol table, but leaving the type alone...
	< c, rt > = convertAndExpandType(t,c);
	
	set[Symbol] failures = { };
	if (!isADTType(rt)) failures += makeFailType("Expected non-terminal type, instead found <prettyPrintType(rt)>", t@\loc);
	if (!isFailType(t1) && !isStrType(t1)) failures += makeFailType("Expected str, instead found <prettyPrintType(t1)>", e@\loc);
	if (isFailType(t1)) failures += t1;

	if (size(failures) > 0) return markLocationFailed(c, exp@\loc, collapseFailTypes(failures));
	return markLocationType(c, exp@\loc, rt);	
}

@doc{Check the types of Rascal expressions: Composition (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> o <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);

	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});

	if (isMapType(t1) && isMapType(t2)) {
		if (subtype(getMapRangeType(t1),getMapDomainType(t2))) {
			return markLocationType(c, exp@\loc, makeMapType(stripLabel(getMapDomainType(t1)),stripLabel(getMapRangeType(t2))));
		} else {
			return markLocationFailed(c, exp@\loc, makeFailType("<prettyPrintType(getMapRangeType(t1))> must be a subtype of <prettyPrintType(getMapDomainType(t2))>", exp@\loc));
		}
	}
	
	if (isRelationType(t1) && isRelationType(t2)) {
		list[Symbol] lflds = getRelFields(t1);
		list[Symbol] rflds = getRelFields(t2);
		set[Symbol] failures = { };
		if (size(lflds) != 0 && size(lflds) != 2)
			failures += makeFailType("Relation <prettyPrintType(t1)> should have arity of 0 or 2", e1@\loc); 
		if (size(rflds) != 0 && size(rflds) != 2)
			failures += makeFailType("Relation <prettyPrintType(t2)> should have arity of 0 or 2", e2@\loc);
		if (!comparable(lflds[1],rflds[0]))
			failures += makeFailType("Range of relation <prettyPrintType(t1)> must be comparable to domain of relation <prettyPrintType(t1)>", exp@\loc);
		if (size(failures) > 0) return markLocationFailed(c, exp@\loc, failures);
		if (size(lflds) == 0 || size(rflds) == 0)
			return markLocationType(c, exp@\loc, \rel([]));
		else
			return markLocationType(c, exp@\loc, \rel([lflds[0],rflds[1]])); 
	}

	if (isFunctionType(t1) && isFunctionType(t2))
		throw "Not yet implemented";

	return markLocationFailed(c, exp@\loc, makeFailType("Composition not defined for <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: Product (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> * <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,computeProductType(t1,t2,exp@\loc));
}

Symbol computeProductType(Symbol t1, Symbol t2, loc l) {
	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2))
		return numericArithTypes(t1, t2);
	
	if (isListType(t1) && isListType(t2))
		return \list(\tuple([getListElementType(t1),getListElementType(t2)]));
	if (isRelType(t1) && isRelType(t2))
		return \rel([getRelElementType(t1),getRelElementType(t2)]);
	if (isSetType(t1) && isSetType(t2))
		return \rel([getSetElementType(t1),getSetElementType(t2)]);
	
	return makeFailType("Product not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>", l);
}

@doc{Check the types of Rascal expressions: Join}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> join <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	throw "Not yet implemented";
}

@doc{Check the types of Rascal expressions: Remainder (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> % <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isIntType(tl) && isIntType(t2)) return markLocationType(c,exp@\loc,\int());
	return markLocationFailed(c,exp@\loc,makeFailType("Remainder not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>",exp@\loc));
}

@doc{Check the types of Rascal expressions: Division (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> / <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,computeDivisionType(t1,t2,exp@\loc));
}

Symbol computeDivisionType(Symbol t1, Symbol t2, loc l) {
	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2))
		return numericArithTypes(t1, t2);
	return makeFailType("Division not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>", l);
}

@doc{Check the types of Rascal expressions: Intersection (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> & <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,computeIntersectionType(t1,t2,exp@\loc));
}

Symbol computeIntersectionType(Symbol t1, Symbol t2, loc l) {
	if (isListType(t1) && isListType(t2))
		return lub(t1,t2);
	if (isRelType(t1) && isRelType(t2))
		return lub(t1,t2);
	if (isSetType(t1) && isSetType(t2))
		return lub(t1,t2);
	if (isMapType(t1) && isMapType(t2) && equivalent(getMapDomainType(t1),getMapDomainType(t2)) && equivalent(getMapRangeType(t1),getMapRangeType(t2)))
		return t1;
	
	return makeFailType("Intersection not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>", l);
}

@doc{Check the types of Rascal expressions: Addition (DONE)}
// TODO: Currently, this isn't parsing right: 1 + [2] doesn't match this
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> + <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,computeAdditionType(t1,t2,exp@\loc));
}

@doc{General function to calculate the type of an addition.}
Symbol computeAdditionType(Symbol t1, Symbol t2, loc l) {
	// Numbers
	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2))
		return numericArithTypes(t1, t2);
	
	// Other non-containers
	if (isStrType(t1) && isStrType(t2))
		return \str();
	if (isBoolType(t1) && isBoolType(t2))
		return \bool();
	if (isLocType(t1) && isLocType(t2))
		return \loc();
	// TODO: Need to properly handle field name propagation here
	if (isTupleType(t1) && isTupleType(t2))
		return \tuple(getTupleFields(t1),getTupleFields(t2));
				
	if (isListType(t1) && isListType(t2))
		return lub(t1,t2);
	if (isSetType(t1) && isSetType(t2))
		return lub(t1,t2);
	if (isMapType(t1) && isMapType(t2))
		return lub(t1,t2);
	
	if (isListType(t1) && !isContainerType(t2))
		return \list(lub(getListElementType(t1),t2));
	if (isSetType(t1) && !isContainerType(t2)) // Covers relations too
		return \set(lub(getSetElementType(t1),t2));
	if (isBagType(t1) && !isContainerType(t2))
		return \bag(lub(getBagElementType(t1),t2));
		
	if (isListType(t2) && !isContainerType(t1))
		return \list(lub(t1,getListElementType(t2)));
	if (isSetType(t2) && !isContainerType(t1)) // Covers relations too
		return \set(lub(t1,getSetElementType(t2)));
	if (isBagType(t2) && !isContainerType(t1))
		return \bag(lub(t1,getBagElementType(t2)));
		
	if (isListType(t1))
		return \list(lub(getListElementType(t1),t2));
	if (isSetType(t1)) // Covers relations too
		return \set(lub(getSetElementType(t1),t2));
	if (isBagType(t1))
		return \bag(lub(getBagElementType(t1),t2));
		
	return makeFailType("Addition not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>", l);
}

@doc{Check the types of Rascal expressions: Subtraction (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> - <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,computeSubtractionType(t1,t2,exp@\loc));
}

Symbol computeSubtractionType(Symbol t1, Symbol t2, loc l) {
	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2))
		return numericArithTypes(t1, t2);

	// Maybe we should check what we are trying to subtract, but currently we don't
	// inside the interpreter, so TODO: should we check the type of t2?	
	if (isListType(t1)) {
		if (!comparable(getListElementType(t1),t2))
			return makeFailType("List of type <prettyPrintType(t1)> could never contain subtracted element of type <prettyPrintType(t2)>", l); 
		return t1;
	}
	if (isSetType(t1)) { // Covers relations too
		if (!comparable(getSetElementType(t1),t2))
			return makeFailType("Set of type <prettyPrintType(t1)> could never contain subtracted element of type <prettyPrintType(t2)>", l); 
		return t1;
	}
	if (isBagType(t1)) {
		if (!comparable(getBagElementType(t1),t2))
			return makeFailType("Bag of type <prettyPrintType(t1)> could never contain subtracted element of type <prettyPrintType(t2)>", l); 
		return t1;
	}
	if (isMapType(t1)) {
		if (!comparable(t1,t2))
			return makeFailType("Map of type <prettyPrintType(t1)> could never contain a sub-map of type <prettyPrintType(t2)>", l); 
		return t1;
	}

	return makeFailType("Subtraction not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>", l);
}

@doc{Check the types of Rascal expressions: AppendAfter (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> << <Expression e2>`, Configuration c) {
	// TODO: Revisit once this feature has been implemented
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	throw "Not implemented";
}

@doc{Check the types of Rascal expressions: InsertBefore (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> >> <Expression e2>`, Configuration c) {
	// TODO: Revisit once this feature has been implemented
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	throw "Not implemented";
}

@doc{Check the types of Rascal expressions: Modulo (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> mod <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isIntType(tl) && isIntType(t2)) return markLocationType(c,exp@\loc,\int());
	return markLocationFailed(c,exp@\loc,makeFailType("Modulo not defined on <prettyPrintType(t1)> and <prettyPrintType(t2)>",exp@\loc));
}

@doc{Check the types of Rascal expressions: Not In (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> notin <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isRelType(t2)) {
		et = getRelElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isSetType(t2)) {
		et = getSetElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isMapType(t2)) {
		et = getMapDomainType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with domain type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isListType(t2)) {
		et = getListElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	}
	return markLocationFailed(c,exp@\loc,makeFailType("notin not defined for <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: In (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> in <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isRelType(t2)) {
		et = getRelElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isSetType(t2)) {
		et = getSetElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isMapType(t2)) {
		et = getMapDomainType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with domain type of <prettyPrintType(t2)>",exp@\loc));
	} else if (isListType(t2)) {
		et = getListElementType(t2);
		if (comparable(t1,et))
			return markLocationType(c,exp@\loc,\bool());
		else
			return markLocationFailed(c,exp@\loc,makeFailType("Cannot compare <prettyPrintType(t1)> with element type of <prettyPrintType(t2)>",exp@\loc));
	}
	return markLocationFailed(c,exp@\loc,makeFailType("in not defined for <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: Greater Than or Equal (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> >= <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});

	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2)) {
		return markLocationType(c,exp@\loc,\bool());
	}
	
	if (isDateTimeType(t1) && isDateTimeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isBoolType(t1) && isBoolType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isListType(t1) && isListType(t2) && comparable(getListElementType(t1),getListElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isMapType(t1) && isMapType(t2) && comparable(getMapDomainType(t1),getMapDomainType(t2)) && comparable(getMapRangeType(t1),getMapRangeType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isNodeType(t1) && isNodeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isRelType(t1) && isRelType(t2) && comparable(getRelElementType(t1),getRelElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isSetType(t1) && isSetType(t2) && comparable(getSetElementType(t1),getSetElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isStrType(t1) && isStrType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isTupleType(t1) && isTupleType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isValueType(t1) || isValueType(t2))
		return markLocationType(c,exp@\loc,\bool());
		
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: Less Than or Equal (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> <= <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});

	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2)) {
		return markLocationType(c,exp@\loc,\bool());
	}
	
	if (isDateTimeType(t1) && isDateTimeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isBoolType(t1) && isBoolType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isListType(t1) && isListType(t2) && comparable(getListElementType(t1),getListElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isMapType(t1) && isMapType(t2) && comparable(getMapDomainType(t1),getMapDomainType(t2)) && comparable(getMapRangeType(t1),getMapRangeType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isNodeType(t1) && isNodeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isRelType(t1) && isRelType(t2) && comparable(getRelElementType(t1),getRelElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isSetType(t1) && isSetType(t2) && comparable(getSetElementType(t1),getSetElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isStrType(t1) && isStrType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isTupleType(t1) && isTupleType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isValueType(t1) || isValueType(t2))
		return markLocationType(c,exp@\loc,\bool());
		
	println("Type 1 is: <t1>");
	println("Type 2 is: <t2>");
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: Less Than (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> < <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});

	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2)) {
		return markLocationType(c,exp@\loc,\bool());
	}
	
	if (isDateTimeType(t1) && isDateTimeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isBoolType(t1) && isBoolType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isListType(t1) && isListType(t2) && comparable(getListElementType(t1),getListElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isMapType(t1) && isMapType(t2) && comparable(getMapDomainType(t1),getMapDomainType(t2)) && comparable(getMapRangeType(t1),getMapRangeType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isNodeType(t1) && isNodeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isRelType(t1) && isRelType(t2) && comparable(getRelElementType(t1),getRelElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isSetType(t1) && isSetType(t2) && comparable(getSetElementType(t1),getSetElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isStrType(t1) && isStrType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isTupleType(t1) && isTupleType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isValueType(t1) || isValueType(t2))
		return markLocationType(c,exp@\loc,\bool());
		
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: Greater Than (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> > <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});

	if (subtype(t1, \num()) && subtype(t2, \num()) && !isVoidType(t1) && !isVoidType(t2)) {
		return markLocationType(c,exp@\loc,\bool());
	}
	
	if (isDateTimeType(t1) && isDateTimeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isBoolType(t1) && isBoolType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isListType(t1) && isListType(t2) && comparable(getListElementType(t1),getListElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isMapType(t1) && isMapType(t2) && comparable(getMapDomainType(t1),getMapDomainType(t2)) && comparable(getMapRangeType(t1),getMapRangeType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isNodeType(t1) && isNodeType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isRelType(t1) && isRelType(t2) && comparable(getRelElementType(t1),getRelElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isSetType(t1) && isSetType(t2) && comparable(getSetElementType(t1),getSetElementType(t2)))
		return markLocationType(c,exp@\loc,\bool());
	if (isStrType(t1) && isStrType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isTupleType(t1) && isTupleType(t2))
		return markLocationType(c,exp@\loc,\bool());
	if (isValueType(t1) || isValueType(t2))
		return markLocationType(c,exp@\loc,\bool());
		
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: Equals (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> == <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (comparable(t1,t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: Non Equals (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> != <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (comparable(t1,t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("<prettyPrintType(t1)> and <prettyPrintType(t2)> incomparable", exp@\loc));
}

@doc{Check the types of Rascal expressions: If Defined Otherwise (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> ? <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	return markLocationType(c,exp@\loc,lub(t1,t2));
}

@doc{Check the types of Rascal expressions: No Match (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Pattern p> !:= <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	< c, t2 > = calculatePatternType(p, c, t1);
	if (isFailType(t2)) return markLocationFailed(c, exp@\loc, t2);
	return markLocationType(c, exp@\loc, \bool());
}

@doc{Check the types of Rascal expressions: Match (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Pattern p> := <Expression e>`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	< c, t2 > = calculatePatternType(p, c, t1);
	if (isFailType(t2)) return markLocationFailed(c, exp@\loc, t2);
	return markLocationType(c, exp@\loc, \bool());
}

@doc{Check the types of Rascal expressions: Enumerator}
public CheckResult checkExp(Expression exp:(Expression)`<Pattern p> <- <Expression e>`, Configuration c) {
	// TODO: For concrete lists, what should we use as the type?
	// TODO: For nodes, ADTs, and tuples, would it be better to use the lub of all the possible types?
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1)) return markLocationFailed(c, exp@\loc, t1);
	Symbol t2 = \void();
	if (isSetType(t1))
		< c, t2 > = calculatePatternType(p, c, getSetElementType(t1));
	else if (isListType(t1))
		< c, t2 > = calculatePatternType(p, c, getListElementType(t1));
	else if (isMapType(t1))
		< c, t2 > = calculatePatternType(p, c, getMapDomainType(t1));
	else if (isADTType(t1) || isTupleType(t1) || isNodeType(t1))
		< c, t2 > = calculatePatternType(p, c, \value());
	
	if (isFailType(t2)) return markLocationFailed(c, exp@\loc, t2);
	return markLocationType(c, exp@\loc, \bool());
}

@doc{Check the types of Rascal expressions: Implication (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> ==> <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isBoolType(t1) && isBoolType(t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Logical and not defined for types <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: Equivalence (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> <==> <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isBoolType(t1) && isBoolType(t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Logical and not defined for types <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: And (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> && <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isBoolType(t1) && isBoolType(t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Logical and not defined for types <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: Or (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> || <Expression e2>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	if (isFailType(t1) || isFailType(t2)) return markLocationFailed(c,exp@\loc,{t1,t2});
	if (isBoolType(t1) && isBoolType(t2)) return markLocationType(c,exp@\loc,\bool());
	return markLocationFailed(c,exp@\loc,makeFailType("Logical or not defined for types <prettyPrintType(t1)> and <prettyPrintType(t2)>", exp@\loc));
}

@doc{Check the types of Rascal expressions: If Then Else (DONE)}
public CheckResult checkExp(Expression exp:(Expression)`<Expression e1> ? <Expression e2> : <Expression e3>`, Configuration c) {
	< c, t1 > = checkExp(e1, c);
	< c, t2 > = checkExp(e2, c);
	< c, t3 > = checkExp(e3, c);
	if (isFailType(t1) || isFailType(t2) || isFailType(t3)) return markLocationFailed(c,exp@\loc,{t1,t2,t3});
	if (!isBoolType(t1)) return markLocationFailed(c,exp@\loc,makeFailType("Expected bool, found <prettyPrintType(t1)>",e1@\loc));
	return markLocationType(c,exp@\loc,lub(t2,t3));
}

@doc{Calculate the arith type for the numeric types, taking account of coercions.}
public Symbol numericArithTypes(Symbol l, Symbol r) {
	if (isIntType(l) && isIntType(r)) return \int();
	if (isIntType(l) && isRatType(r)) return \rat();
	if (isIntType(l) && isRealType(r)) return \real();
	if (isIntType(l) && isNumType(r)) return \num();

	if (isRatType(l) && isIntType(r)) return \rat();
	if (isRatType(l) && isRatType(r)) return \rat();
	if (isRatType(l) && isRealType(r)) return \real();
	if (isRatType(l) && isNumType(r)) return \num();

	if (isRealType(l) && isIntType(r)) return \real();
	if (isRealType(l) && isRatType(r)) return \real();
	if (isRealType(l) && isRealType(r)) return \real();
	if (isRealType(l) && isNumType(r)) return \num();

	if (isNumType(l) && isIntType(r)) return \num();
	if (isNumType(l) && isRatType(r)) return \num();
	if (isNumType(l) && isRealType(r)) return \num();
	if (isNumType(l) && isNumType(r)) return \num();

	throw "Only callable for numeric types, given <prettyPrintType(l)> and <prettyPrintType(r)>";
}

@doc{Check the types of Rascal literals: IntegerLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<IntegerLiteral il>`, Configuration c) = markLocationType(c, l@\loc, \int());

@doc{Check the types of Rascal literals: RealLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<RealLiteral rl>`, Configuration c) = markLocationType(c, l@\loc, \real());

@doc{Check the types of Rascal literals: BooleanLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<BooleanLiteral bl>`, Configuration c) = markLocationType(c, l@\loc, \bool());

@doc{Check the types of Rascal literals: DateTimeLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<DateTimeLiteral dtl>`, Configuration c) = markLocationType(c, l@\loc, \datetime());

@doc{Check the types of Rascal literals: RationalLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<RationalLiteral rl>`, Configuration c) = markLocationType(c, l@\loc, \rat());

@doc{Check the types of Rascal literals: RegExpLiteral (DONE)}
public CheckResult checkLiteral(Literal l:(Literal)`<RegExpLiteral rl>`, Configuration c) {
	// Extract all the names used in the regular expression.
	//
	// NOTE: We cannot use concrete syntax matching here, because it confuses the parser. NamedRegExp is defined
	// as Name:RegExp, but the : is interpreted as defining a variable becomes pattern instead, which causes an
	// exception to be thrown in the interpreter.
	//
	// TODO: This is wrong, since we need to account for the order -- we could have a name that is defined and then
	// used to the left.
	list[Tree] nameUses = [];
	list[Tree] nameDefs = [];
	rel[Tree,Tree] defUses = { };
	
	visit(rl) {
		case \appl(\prod(lex("RegExp"),[_,\lex("Name"),_],_),list[Tree] prds) : nameUses += prds[1];
		case \appl(\prod(lex("RegExp"),[_,\lex("Name"),_,_,_],_),list[Tree] prds) : nameDefs += prds[1];
		case \appl(\prod(lex("NamedRegExp"),[_,\lex("Name"),_],_),list[Tree] prds) : defUses += < last(nameDefs), prds[1] >;
	}

	// Come up with a consolidated, ordered list. All the items in nameUses and nameDefs are at the top level, so we don't have
	// to worry about nesting here. All the nested names are inside defUses.
	list[Tree] consolidated = sort(nameUses + nameDefs, bool(Tree l, Tree r) { return l.begin.line < r.begin.line || (l.begin.line <= r.begin.line && l.begin.column < r.begin.column); });
	
	// Process the names in the regexp, making sure they are defined or adding them into scope as needed.
	if (size(consolidated) > 0) {
		for (n <- consolidated) {
			RName rn = convertName(n);
			if (n in nameUses) {
				// If this is just a use, it should be defined already. It can be of any type -- it will just be
				// converted to a string before being used.
				if (!fcvExists(c, rn)) {
					c = addScopeMessage(c, error("Name is undefined", n@\loc));
				} else {
					c.uses += < c.fcvEnv[rn], n@\loc >;
				}
			} else {
				// If this is a definition, add it into scope if possible.
				if (varCanShadow(c, rn)) {
					c = addVariable(c, rn, false, n@\loc, \str());
				} else {
					c = addScopeMessage(c, error("Name is already bound in the current scope", n@\loc));
				}
				// Then process names used in the def part.
				for (cn <- defUses[n]) {
					if (!fcvExists(c,convertName(cn))) {
						c = addScopeMessage(c, error("Name is undefined", cn@\loc));
					} else {
						c.uses += < c.fcvEnv[convertName(cn)], cn@\loc >;
					}
				}
			}
		}
	}
	
	// This always appears in a pattern, so we don't need to either add a scope or back out the vars we added (that
	// will be taken care of in the pattern checking logic). We return str here just to match against the intended
	// type of the subject.
	return markLocationType(c, l@\loc, \str());
}

@doc{Check the types of Rascal literals: StringLiteral}
public CheckResult checkLiteral(Literal l:(Literal)`<StringLiteral sl>`, Configuration c) {
	// TODO: Add all the code needed for string interpolation.
	return markLocationType(c, l@\loc, \str());
}

@doc{Check the types of Rascal literals: LocationLiteral}
public CheckResult checkLiteral(Literal l:(Literal)`<LocationLiteral ll>`, Configuration c) {
	// TODO: Add any code needed for interpolation inside location literals.
	return markLocationType(c, l@\loc, \loc());
}

@doc{Check the types of Rascal parameters: Default (DONE) }
public CheckResult checkParameters((Parameters)`( <Formals fs> )`, Configuration c) = checkFormals(fs, c);

@doc{Check the types of Rascal parameters: VarArgs (DONE) }
public CheckResult checkParameters((Parameters)`( <Formals fs> ... )`, Configuration c) = checkFormals(fs, c);

@doc{Check the types of Rascal formals: Default}
public CheckResult checkFormals((Formals)`<{Pattern ","}* ps>`, Configuration c) {
	list[Symbol] formals = [ ];
	for (p <- ps) {
		< c, t > = calculatePatternType(p, c);
		formals += t;
	}
	return < c, \tuple(formals) >;
}

@doc{Defs and uses of names; allows marking them while still keeping them in the same list or set.}
data DefOrUse = def(RName name) | use(RName name);

@doc{A compact representation of patterns}
data PatternTree 
	= setNode(list[PatternTree] children)
	| listNode(list[PatternTree] children)
	| nameNode(RName name)
	| multiNameNode(RName name)
	| spliceNode(PatternTree child)
	| negativeNode(PatternTree child)
	| literalNode(Symbol rtype)
	| literalNode(list[tuple[DefOrUse,loc]] names)
	| tupleNode(list[PatternTree] children)
	| typedNameNode(RName name, loc at, Symbol rtype)
	| mapNode(list[tuple[PatternTree,PatternTree]] mapChildren)
	| reifiedTypeNode(PatternTree s, PatternTree d)
	| callOrTreeNode(PatternTree head, list[PatternTree] args)
	| varBecomesNode(RName name, loc at, PatternTree child)
	| asTypeNode(Symbol rtype, PatternTree child)
	| deepNode(PatternTree child)
	| antiNode(PatternTree child)
	| tvarBecomesNode(Symbol rtype, RName name, loc at, PatternTree child)
	;
	
@doc{Mark pattern trees with the source location of the pattern}
public anno loc PatternTree@at;

@doc{A shorthand for the results to expect from binding -- an updated configuration and an updated pattern tree.}
public alias BindResult = tuple[Configuration,PatternTree];

@doc{Extract a tree representation of the pattern.}
public BindResult extractPatternTree(Pattern pat:(Pattern)`{ <{Pattern ","}* ps> }`, Configuration c) {
	list[PatternTree] tpList = [ ];
	for (p <- ps) { < c, pti > = extractPatternTree(p,c); tpList = tpList + pti; }
	return < c, setNode(tpList)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`[ <{Pattern ","}* ps> ]`, Configuration c) {
	list[PatternTree] tpList = [ ];
	for (p <- ps) { < c, pti > = extractPatternTree(p,c); tpList = tpList + pti; }
	return < c, listNode(tpList)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<QualifiedName qn>`, Configuration c) {
	return < c, nameNode(convertName(qn))[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<QualifiedName qn>*`, Configuration c) {
	return < c, multiNameNode(convertName(qn))[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`* <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, spliceNode(pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`+ <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, spliceNode(pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`- <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, negativeNode(pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<IntegerLiteral il>`, Configuration c) {
	return < c, literalNode(\int())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<RealLiteral rl>`, Configuration c) {
	return < c, literalNode(\real())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<BooleanLiteral bl>`, Configuration c) {
	return < c, literalNode(\bool())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<DateTimeLiteral dtl>`, Configuration c) {
	return < c, literalNode(\datetime())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<RationalLiteral rl>`, Configuration c) {
	return < c, literalNode(\rat())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<RegExpLiteral rl>`, Configuration c) {
	list[tuple[DefOrUse,loc]] names = [ ];
		
	top-down visit(rl) {
		case \appl(\prod(lex("RegExp"),[_,\lex("Name"),_],_),list[Tree] prds) : names += < use(convertName(prds[1])), prds[1]@\loc >;
		case \appl(\prod(lex("RegExp"),[_,\lex("Name"),_,_,_],_),list[Tree] prds) : names += < def(convertName(prds[1])), prds[1]@\loc >;
		case \appl(\prod(lex("NamedRegExp"),[_,\lex("Name"),_],_),list[Tree] prds) : names += < use(convertName(prds[1])), prds[1]@\loc >;
	}
	
	return < c, literalNode(names)[@at = pat@\loc] >;
}
// TODO: Should account for comprehensions in strings and locations
// TODO: Need to expand out converted types, which probably means modifying this
// code to also take and return the configuration. However, that is cumbersome...
public BindResult extractPatternTree(Pattern pat:(Pattern)`<StringLiteral sl>`, Configuration c) {
	return < c, literalNode(\str())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<LocationLiteral ll>`, Configuration c) {
	return < c, literalNode(\loc())[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`< <Pattern p1>, <{Pattern ","}* ps> >`, Configuration c) {
	< c, pt1 > = extractPatternTree(p1, c);
	list[PatternTree] ptlist = [ pt1 ];
	for (p <- ps) { < c, pti > = extractPatternTree(p,c); ptlist = ptlist + pti; }
	return < c, tupleNode(ptlist)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<Type t> <Name n>`, Configuration c) {
	< c, rt > = convertAndExpandType(t,c);
	return < c, typedNameNode(convertName(n), n@\loc, rt)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`( <{Mapping[Pattern] ","}* mps> )`, Configuration c) {
	list[tuple[PatternTree,PatternTree]] res = [ ];
	for ((Mapping[Pattern])`<Pattern pd> : <Pattern pr>` <- mps) {
		< c, pdt > = extractPatternTree(pd,c);
		< c, prt > = extractPatternTree(pr,c);
		res += < pdt, prt >;
	}
	return < c, mapNode(res)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`type ( <Pattern s>, <Pattern d> )`, Configuration c) {
	< c, pti1 > = extractPatternTree(s,c);
	< c, pti2 > = extractPatternTree(d,c);
	return < c, reifiedTypeNode(pti1,pti2)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<Pattern p> ( <{Pattern ","}* ps> )`, Configuration c) { 
	< c, pti > = extractPatternTree(p,c);
	list[PatternTree] psList = [ ];
	for (p <- ps) { < c, psi > = extractPatternTree(p,c); psList = psList + psi; }
	return < c, callOrTreeNode(pti,psList)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<Name n> : <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, varBecomesNode(convertName(n), n@\loc, pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`[ <Type t> ] <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	< c, rt > = convertAndExpandType(t,c);
	return < c, asTypeNode(rt, pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`/ <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, deepNode(pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`! <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	return < c, antiNode(pti)[@at = pat@\loc] >;
}
public BindResult extractPatternTree(Pattern pat:(Pattern)`<Type t> <Name n> : <Pattern p>`, Configuration c) {
	< c, pti > = extractPatternTree(p,c);
	< c, rt > = convertAndExpandType(t,c);
	return < c, tvarBecomesNode(rt,convertName(n),n@\loc,pti)[@at = pat@\loc] >;
}

@doc{Allows PatternTree nodes to be annotated with types.}
public anno Symbol PatternTree@rtype;

@doc{A quick predicate to say whether we can use the type in a type calculation}
public bool concreteType(Symbol t) = size({ ti | /Symbol ti := t, \failure(_) := ti || \inferred(_) := ti }) == 0; 

@doc{Calculate the type of pattern. If a subject is given, this is used as part of the type calculation to ensure the subject can be bound to the pattern.}
public CheckResult calculatePatternType(Pattern pat, Configuration c, Symbol subjects...) {
	if (size(subjects) > 1) throw "Invalid invocation, only one subject allowed, not <size(subjects)>";
	
	// Init: extract the pattern tree, which gives us an abstract representation of the pattern
	< c, pt > = extractPatternTree(pat,c);
	Configuration cbak = c;
	set[Symbol] failures = { };
	
	// Step 1: Do an initial assignment of types to the names present
	// in the tree and to nodes with invariant types (such as int
	// literals and guarded patterns).
	pt = top-down visit(pt) {
		case ptn:setNode(ptns) : {
			for (idx <- index(ptns), multiNameNode(n) := ptns[idx]) {
				if (RSimpleName("_") == n) {
					ptns[idx] = ptns[idx][@rtype = \inferred(c.uniqueify)];
					c.uniqueify = c.uniqueify + 1;
				} else if (!fcvExists(c, n)) {
					c = addVariable(c, n, true, ptns[idx]@at, \set(\inferred(c.uniqueify)));
					ptns[idx] = ptns[idx][@rtype = \inferred(c.uniqueify)];
					c.uniqueify = c.uniqueify + 1;
				} else {
					c.uses = c.uses + < c.fcvEnv[n], ptn@at >;
					Symbol rt = c.store[c.fcvEnv[n]].rtype;
					if (isSetType(rt))
						ptns[idx] = ptns[idx][@rtype = getSetElementType(rt)];
					else
						failures += makeFailType("Expected type set, not <prettyPrintType(rt)>", ptns[idx]@at); 
				}
			}
			insert(ptn[children=ptns]);
		}

		case ptn:listNode(ptns) : {
			for (idx <- index(ptns), multiNameNode(n) := ptns[idx]) {
				if (RSimpleName("_") == n) {
					ptns[idx] = ptns[idx][@rtype = \inferred(c.uniqueify)];
					c.uniqueify = c.uniqueify + 1;
				} else if (!fcvExists(c, n)) {
					c = addVariable(c, n, true, ptns[idx]@at, \list(\inferred(c.uniqueify)));
					ptns[idx] = ptns[idx][@rtype = \inferred(c.uniqueify)];
					c.uniqueify = c.uniqueify + 1;
				} else {
					c.uses = c.uses + < c.fcvEnv[n], ptn@at >;
					Symbol rt = c.store[c.fcvEnv[n]].rtype;
					if (isListType(rt))
						ptns[idx] = ptns[idx][@rtype = getListElementType(rt)];
					else
						failures += makeFailType("Expected type list, not <prettyPrintType(rt)>", ptns[idx]@at); 
				}
			}
			insert(ptn[children=ptns]);
		}

		case ptn:nameNode(n) : { 
			if (RSimpleName("_") == n) {
				insert(ptn[@rtype = \inferred(c.uniqueify)]);
				c.uniqueify = c.uniqueify + 1;
			} else if (!fcvExists(c, n)) {
				c = addVariable(c, n, true, ptn@at, \inferred(c.uniqueify));
				c.uniqueify = c.uniqueify + 1;
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			} else {
				c.uses = c.uses + < c.fcvEnv[n], ptn@at >;
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			}
		}
		
		case ptn:literalNode(rt) => ptn[@rtype = rt]
		
		case ptn:literalNode(list[tuple[DefOrUse,loc]] names) : {
			for ( < d, l > <- names ) {
				if (def(n) := d) {
					if (varCanShadow(c, n)) {
						c = addVariable(c, n, false, l, \str());
					} else {
						failures += makeFailType("Invalid declaration of name <prettyPrintName(n)>, name already in scope", ptn@at);
						c.uses = c.uses + < c.fcvEnv[n], ptn@at >; // Just use the current declaration in scope, that is the semantics
					}
				} else if (use(n) := d) {
					if (!fcvExists(c, n)) {
						failures += makeFailType("Name <prettyPrintName(n)> not yet defined", ptn@at);
					} else {
						c.uses = c.uses + < c.fcvEnv[n], l >; 
					}
				} 
			}
			insert(ptn[@rtype = \str()]);
		}
		
		case ptn:typedNameNode(n, l, rt) : { 
			if (RSimpleName("_") == n) {
				insert(ptn[@rtype = rt]);
			} else if (varCanShadow(c, n)) {
				c = addVariable(c, n, false, l, rt);
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			} else {
				failures += makeFailType("Invalid declaration of name <prettyPrintName(n)>, name already in scope", ptn@at);
				c.uses = c.uses + < c.fcvEnv[n], ptn@at >; // Just use the current declaration in scope, that is the semantics
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			}
		}
		
		case ptn:varBecomesNode(n, l, _) : { 
			if (RSimpleName("_") == n) {
				insert(ptn[@rtype = \inferred(c.uniqueify)]);
				c.uniqueify = c.uniqueify + 1;
			} else if (!fcvExists(c, n)) {
				c = addVariable(c, n, true, l, \inferred(c.uniqueify));
				c.uniqueify = c.uniqueify + 1;
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			}  else {
				c.uses = c.uses + < c.fcvEnv[n], ptn@at >;
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			}
		}
		
		case ptn:asTypeNode(rt, _) => ptn[@rtype = rt]
		
		// TODO: Not sure if this is the best choice, but it is the choice
		// the current interpreter makes...
		case ptn:antiNode(_) => ptn[@rtype = \value()]
		
		case ptn:tvarBecomesNode(rt, n, l, _) : { 
			if (RSimpleName("_") == n) {
				insert(ptn[@rtype = rt]);
			} else if (varCanShadow(c, n)) {
				c = addVariable(c, n, false, l, rt);
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			} else {
				failures += makeFailType("Invalid declaration of name <prettyPrintName(n)>, name already in scope", ptn@at);
				c.uses = c.uses + < c.fcvEnv[n], ptn@at >; // Just use the current declaration in scope, that is the semantics
				insert(ptn[@rtype = c.store[c.fcvEnv[n]].rtype]);
			}
		}
	}
	
	bool modified = true;

	PatternTree updateRT(PatternTree pt, Symbol rt) {
		if ( (pt@rtype)? && (pt@rtype == rt) ) return pt;
		modified = true;
		return pt[@rtype = rt];
	}
	
	// Step 2: push types up from the leaves to the root, and back down from the root to the leaves,
	// until the type stabilizes
	bool firstTime = true;
	while(modified) {
		modified = false;

		// In this first visit, we try to propagate type information up from the leaves of the
		// pattern tree towards the root. This gives us a way to use the types assigned to
		// names, literals, etc to find the final types of other patterns.
		pt = bottom-up visit(pt) {
			case ptn:setNode([]) => updateRT(ptn, \set(\void()))
			
			case ptn:setNode(ptns) => updateRT(ptn,\set(lubList([pti@rtype | pti <- ptns]))) 
									  when all(idx <- index(ptns), (ptns[idx]@rtype)?, concreteType(ptns[idx]@rtype))
									  
			case ptn:listNode([]) => updateRT(ptn, \list(\void()))
			
			case ptn:listNode(ptns) => updateRT(ptn,\list(lubList([pti@rtype | pti <- ptns]))) 
									  when all(idx <- index(ptns), (ptns[idx]@rtype)?, concreteType(ptns[idx]@rtype))
									  
			case ptn:spliceNode(cp) : throw "Not yet implemented";
	
			case ptn:negativeNode(cp) => updateRT(ptn, cp@rtype) when (cp@rtype)? && concreteType(cp@rtype) && !isVoidType(cp@rtype) && subtype(cp@rtype, \num())
	
			case ptn:negativeNode(cp) :
				if ( (cp@rtype)? && concreteType(cp@rtype))
					failures += makeFailType("Cannot apply negative pattern to subpattern of type <prettyPrintType(cp@rtype)>", ptn@at);
					
			case ptn:tupleNode(ptns) => updateRT(ptn,\tuple([pti@rtype|pti <- ptns]))
										when all(idx <- index(ptns), (ptns[idx]@rtype)?, concreteType(ptns[idx]@rtype))
										
			case ptn:mapNode([]) => updateRT(ptn,\map(\void(),\void()))
										
			case ptn:mapNode(ptns) => updateRT(ptn,\map(lubList([d@rtype|<d,_> <- ptns]),lubList([r@rtype|<_,r><-ptns])))
									  when all(idx <- index(ptns), <d,r> := ptns[idx], (d@rtype)?, (r@rtype)?, concreteType(d@rtype), concreteType(r@rtype))
									  
			case ptn:antiNode(cp) => updateRT(ptn, cp@rtype) when (cp@rtype)? && concreteType(cp@rtype)
			
			case ptn:varBecomesNode(n,l,cp) : {
				if ( (cp@rtype)? && concreteType(cp@rtype)) {
					Symbol rt = (RSimpleName("_") == n) ? ptn@rtype : c.store[c.fcvEnv[n]].rtype;
					bool isInferred = (RSimpleName("_") == n) ? true : c.store[c.fcvEnv[n]].inferred;
					if (isInferred) {
						Symbol rtNew = lub(rt, cp@rtype);
						if (!equivalent(rtNew,rt)) {
							if (RSimpleName("_") != n) c.store[c.fcvEnv[n]].rtype = rtNew;
							insert updateRT(ptn, rtNew);
						}
					} else {
						if (!subtype(cp@rtype, rt))
							failures += makeFailType("Cannot assign pattern of type <prettyPrintType(cp@rtype)> to non-inferred variable of type <prettyPrintType(rt)>", ptn@at);
					}
				}
			}
	
			case ptn:tvarBecomesNode(rt,n,l,cp) : {
				if ( (cp@rtype)? && concreteType(cp@rtype)) {
					Symbol rt = (RSimpleName("_") == n) ? ptn@rtype : c.store[c.fcvEnv[n]].rtype;
					if (!subtype(cp@rtype, rt))
						failures += makeFailType("Cannot assign pattern of type <prettyPrintType(cp@rtype)> to non-inferred variable of type <prettyPrintType(rt)>", ptn@at);
				}
			}
			
			case ptn:reifiedTypeNode(sp,dp) :
				throw "Not yet implemented";
	
			case ptn:callOrTreeNode(ph,pargs) : {
				if ( (ph@rtype)? && concreteType(ph@rtype) ) {
					if (isConstructorType(ph@rtype) || isOverloadedType(ph@rtype)) {
						// alts contains all possible constructors of this name
						set[Symbol] alts = (isOverloadedType(ph@rtype)) ? { o | o <- getOverloadOptions(ph@rtype), isConstructorType(o) } : {ph@rtype};
						// matches holds all the constructors that match the arity and types in the pattern
						set[Symbol] matches = { };
						
						if (size(pargs) == 0) {
							// if we have no arguments, then all the alternatives could match
							matches = alts;
						} else {
							// filter first based on the arity of the constructor
							for (a <- alts, size(getConstructorArgumentTypes(a)) == size(pargs)) {
								// next, find the bad matches, which are those argument positions where we have concrete
								// type information and that information does not match the alternative
								badMatches = { idx | idx <- index(pargs), (pargs[idx]@rtype)?, concreteType(pargs[idx]@rtype), !subtype(pargs[idx]@rtype, getConstructorArgumentTypes(a)[idx]) };
								if (size(badMatches) == 0) 
									// if we had no bad matches, this is a valid alternative
									matches += a;
							}
						}
						
						if (size(matches) == 1) {
							// Push the binding back down the tree with the information in the constructor type; if
							// this doesn't cause errors, save the updated children back into the tree, along with
							// the match type
							Symbol matchType = getOneFrom(matches);
							list[PatternTree] newChildren = [ ];
							try {
								for (idx <- index(pargs)) {
									< c, newarg > = bind(pargs[idx],getConstructorArgumentTypes(matchType)[idx],c);
									newChildren += newarg;
								}
							} catch v : {
								println("Could not bind: <v>");
								newChildren = pargs;
							}
							insert updateRT(ptn[head=ph[@rtype=matchType]][args=newChildren], getConstructorResultType(matchType));
						} else {
							println("Found <matches> matches");
						}
					} else if (isStrType(ph@rtype)) {
						insert updateRT(ptn, \node());
					}
				}
			}		
		}
		
		if (size(subjects) == 1) {
			try {
				< c, pt > = bind(pt, getOneFrom(subjects), c);
				// Why do this? Because we want to bind at least once, and the first bind could
				// modify the tree, but we don't have a good, cheap way of telling. After that, we
				// can assume that, if we didn't change anything above, we won't change anything if
				// we bind again.
				if (firstTime) {
					modified = true;
					firstTime = false;
				}
			} catch v : {
				println("Cannot bind: <v>");
				; // May want to do something here in the future, but for now just consume the exception, this still fails to assign a type...
			}
		}
	}
    
    if ( (pt@rtype)? ) {
    	unresolved = { pti | /PatternTree pti := pt, !((pti@rtype)?) || !concreteType(pti@rtype) };
    	if (size(unresolved) > 0)
			return < cbak, makeFailType("Type of pattern could not be computed, please add additional type annotations", pat@\loc) >;
		else
			return < c, pt@rtype >;
    } else {
    	println("Could not compute type for the following pattern tree: <pt>");
    	return < cbak, makeFailType("Type of pattern could not be computed, please add additional type annotations", pat@\loc) >;	
    }
}

@doc{Bind a subject type to a pattern tree.}
public BindResult bind(PatternTree pt, Symbol rt, Configuration c) {
	// NOTE: We assume the bind triggers an error at the point of bind failure.
	// So, if we are looking at a set node, we just have to make sure that the
	// type we are binding to it is a set of something.
	//
	// TODO: Add more checks here. If we push information through a node that will
	// cause a failure on the push back up, we will still catch it. However, if we
	// are using bind as a proxy for which overload to use, we will have better
	// luck if we catch more errors here. Examples: negation should check for numerics,
	// and typed name becomes should make sure the result is of a compatible type.
	//
	// TODO: Anything for * variables?
	switch(pt) {
		case setNode(cs) : {
			if (isSetType(rt)) {
				list[PatternTree] res = [ ];
				for (csi <- cs) { < c, pti > = bind(csi, getSetElementType(rt), c); res += pti; }
				return < c, pt[children = res] >; 
			}
		}

		case listNode(cs) : {
			if (isListType(rt)) {
				list[PatternTree] res = [ ];
				for (csi <- cs) { < c, pti > = bind(csi, getListElementType(rt), c); res += pti; }
				return < c, pt[children = res] >; 
			}
		}
		
		case nameNode(RSimpleName("_")) : {
			Symbol currentType = pt@rtype;
			if (isInferredType(currentType))
				return < c, pt[@rtype = rt] >;
			else
				return < c, pt[@rtype = lub(currentType, rt)] >;
		}
		
		case nameNode(rn) : {
			Symbol currentType = c.store[c.fcvEnv[rn]].rtype;
			if (c.store[c.fcvEnv[rn]].inferred) {
				if (isInferredType(currentType)) {
					c.store[c.fcvEnv[rn]].rtype = rt;
				} else {
					c.store[c.fcvEnv[rn]].rtype = lub(currentType, rt);
				}
				return < c, pt[@rtype = c.store[c.fcvEnv[rn]].rtype] >;
			} else {
				if (comparable(currentType, rt))
					return < c, pt >;
				else
					throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
			}
		}
		
		case multiNameNode(RSimpleName("_")) : {
			Symbol currentType = pt@rtype;
			if (isInferredType(currentType))
				return < c, pt[@rtype = rt] >;
			else
				return < c, pt[@rtype = lub(rt,pt@rtype)] >;
		}

		case multiNameNode(rn) : {
			Symbol currentType = c.store[c.fcvEnv[rn]].rtype;
			if (c.store[c.fcvEnv[rn]].inferred) {
				if (isSetType(currentType) && isInferredType(getSetElementType(currentType))) {
					c.store[c.fcvEnv[rn]].rtype = \set(rt);
					return < c, pt[@rtype = rt] >;
				} else if (isListType(currentType) && isInferredType(getListElementType(currentType))) {
					c.store[c.fcvEnv[rn]].rtype = \list(rt);
					return < c, pt[@rtype = rt] >;
				} else if (isSetType(currentType)) {
					c.store[c.fcvEnv[rn]].rtype = \set(lub(currentType, rt));
					return < c, pt[@rtype = getSetElementType(c.store[c.fcvEnv[rn]].rtype)] >;
				} else if (isListType(currentType)) {
					c.store[c.fcvEnv[rn]].rtype = \list(lub(currentType, rt));
					return < c, pt[@rtype = getListElementType(c.store[c.fcvEnv[rn]].rtype)] >;
				}
			} else {
				if (isSetType(currentType) && comparable(getSetElementType(currentType), rt))
					return < c, pt >;
				else if (isListType(currentType) && comparable(getListElementType(currentType), rt))
					return < c, pt >;
				else
					throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
			}
		}
		
		case spliceNode(cp) : {
			throw "Not yet implemented";
		}
		
		case negativeNode(cp) : {
			< c, cpNew > = bind(cp, rt, c);
			return < c, pt[child = cpNew] >;
		}
		
		case literalNode(nt) : {
			if (!isInferredType(rt) && !comparable(pt@rtype,rt))
				throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
			else
				return < c, pt >;
		}
		
		case literalNode(list[tuple[DefOrUse,loc]] names) : {
			if (!isInferredType(rt) && !comparable(pt@rtype,rt))
				throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
			else
				return < c, pt >;
		}
		
		case tupleNode(cs) : {
			if (isTupleType(rt)) {
				list[Symbol] tfields = getTupleFields(rt);
				if (size(tfields) == size(cs)) {
					list[PatternTree] res = [ ];
					for (idx <- index(tfields)) { < c, pti > = bind(cs[idx], tfields[idx], c); res += pti; }
					return < c, pt[children = res] >; 
				} else {
					throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
				}
			}
		}
		
		case typedNameNode(n, l, nt) : {
			Symbol currentType = (RSimpleName("_") == n) ? pt@rtype : c.store[c.fcvEnv[n]].rtype;
			if (comparable(currentType, rt))
				return < c, pt >;
			else
				throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
		}
		
		case mapNode(list[tuple[PatternTree,PatternTree]] mapChildren) : {
			if (isMapType(rt)) {
				list[tuple[PatternTree,PatternTree]] res = [ ];
				for (<d1,r1> <- mapChildren) { 
					< c, pt1 > = bind(d1, getMapDomainType(rt), c); 
					< c, pt2 > = bind(r1, getMapRangeType(rt), c); 
					res += < pt1, pt2 >; 
				}
				return < c, pt[children = res] >; 
			}
		}
		
		case reifiedTypeNode(ps,pd) : {
			// TODO: What else do we need to do here?
			throw "Not yet implemented";
		}
		
		case callOrTreeNode(ph, cs) : {
			// TODO: Should we implement this? We already always push back down when we hit call or tree nodes,
			// so this may just be redundant.
			;
		}
		
		case varBecomesNode(RSimpleName("_"), l, cp) : {
			Symbol currentType = pt@rtype;
			< c, cpnew > = bind(cp, rt, c);
			if (isInferredType(currentType)) {
				return < c, pt[@rtype = rt][child = cpnew] >;
			} else {
				return < c, pt[@rtype = lub(currentType,rt)][child = cpnew] >;
			}
		}
		
		case varBecomesNode(n, l, cp) : {
			Symbol currentType = c.store[c.fcvEnv[rn]].rtype;
			if (c.store[c.fcvEnv[rn]].inferred) {
				if (isInferredType(currentType)) {
					c.store[c.fcvEnv[rn]].rtype = rt;
				} else {
					c.store[c.fcvEnv[rn]].rtype = lub(c.store[c.fcvEnv[rn]].rtype, rt);
				}
				< c, cpnew > = bind(cp, rt, c);
				return < c, pt[@rtype = c.store[c.fcvEnv[rn]].rtype][child = cpnew] >;
			} else {
				if (comparable(currentType, rt)) {
					< c, cpnew > = bind(cp, rt, c);
					return < c, pt[child = cpnew] >;
				} else {
					throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
				}
			}
		}
		
		case asTypeNode(nt, cp) : {
			< c, cpNew > = bind(cp, rt, c);
			return < c, pt[child = cpNew] >;
		}
		
		case deepNode(cp) : {
			< c, cpNew > = bind(cp, \value(), c);
			return < c, pt[child = cpNew] >;
		}
		
		case antiNode(cp) : {
			< c, cpNew > = bind(cp, rt, c);
			return < c, pt[child = cpNew] >;
		}
		
		case tvarBecomesNode(nt, n, l, cp) : {
			Symbol currentType = (RSimpleName("_") == n) ? pt@rtype : c.store[c.fcvEnv[rn]].rtype;
			if (comparable(currentType, rt)) {
				< c, cpNew > = bind(cp, rt, c);
				return < c, pt[child = cpNew] >;
			} else {
				throw "Bind error, cannot bind subject of type <prettyPrintType(rt)> to pattern of type <prettyPrintType(pt@rtype)>";
			}
		}
	}
	
	throw "Bind Error: Cannot bind pattern tree <pt> to type <prettyPrintType(rt)>";
}

@doc{Check the type of Rascal statements: Assert (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`assert <Expression e>;`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	if (isFailType(t1))
		return markLocationFailed(c, stmt@\loc, t1);
	else if (!isBoolType(t1))
		return markLocationFailed(c, stmt@\loc, makeFailType("Invalid type <prettyPrintType(t1)>, expected expression of type bool", e@\loc));
	return markLocationType(c, stmt@\loc, \bool());
}

@doc{Check the type of Rascal statements: AssertWithMessage (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`assert <Expression e> : <Expression em>;`, Configuration c) {
	< c, t1 > = checkExp(e, c);
	< c, t2 > = checkExp(em, c);
	set[Symbol] failures = { };
	
	if (isFailType(t1)) failures += t1;
	if (!isFailType(t1) && !isBoolType(t1))
		failures += makeFailType("Invalid type <prettyPrintType(t1)>, expected expression of type bool", e@\loc);
		
	if (isFailType(t2)) failures += t2;
	if (!isFailType(t2) && !isStrType(t2))
		failures += makeFailType("Invalid type <prettyPrintType(t2)>, expected expression of type str", em@\loc);
		
	if (size(failures) > 0)
		return markLocationFailed(c, stmt@\loc, failures);
	else
		return markLocationType(c, stmt@\loc, \bool());
}

@doc{Check the type of Rascal statements: Expression (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<Expression e>;`, Configuration c) {
	< c, t1 > = checkExp(e,c);
	if (isFailType(t1))
		return markLocationFailed(c, stmt@\loc, t1);
	else
		return markLocationType(c, stmt@\loc, t1);
}

@doc{Check the type of Rascal statements: Visit}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> <Visit v>`, Configuration c) {
	// TODO: Check domain values over iterations
	if ((Label)`<Name n> :` := l) {
		// Add checks involving label environment
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", l@\loc));
		c2 = addLabel(c,labelName,l@\loc,visitLabel());
		c2.labelStack = < labelName, visitLabel(), \void() > + c2.labelStack;
		< c2, vt > = checkVisit(v,c2);
		// Recovering the environment removes the label from the environment
		c2.labelStack = tail(c2.labelStack);
		c = recoverEnvironments(c2,c);
		if (isFailType(vt)) return markLocationFailed(c,exp@\loc,vt);
		return markLocationType(c,exp@\loc,vt);
	} else {
		c2.labelStack = < RSimpleName(""), visitLabel(), \void() > + c2.labelStack;
		< c, vt > = checkVisit(v,c);	
		c2.labelStack = tail(c2.labelStack);
		if (isFailType(vt)) return markLocationFailed(c,exp@\loc,vt);
		return markLocationType(c,exp@\loc,vt);
	} 
}

@doc{Check the type of Rascal statements: While}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> while ( <{Expression ","}+ conds> ) <Statement bdy>`, Configuration c) {
	// TODO: Check domain values over iterations
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,whileLabel());
		c2.labelStack = < labelName, whileLabel(), \void() > + c2.labelStack;
		
		set[Symbol] failures = { };
		
		for (cond <- conds) { 
			< c2, t1 > = checkExp(cond, c2);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c2, t2 > = checkStmt(bdy, c2);
		if (isFailType(t2)) failures += t2;
		
		< _, _, loopElements > = head(c2.labelStack);
		println("In loop at <stmt@\loc>, append result is <prettyPrintType(loopElements)>");
		c2.labelStack = tail(c2.labelStack);

		c = recoverEnvironments(c2, c);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \list(loopElements));	
	} else {
		c.labelStack = < RSimpleName(""), whileLabel(), \void() > + c.labelStack;
		set[Symbol] failures = { };
		
		for (cond <- conds) { 
			< c, t1 > = checkExp(cond, c);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c, t2 > = checkStmt(bdy, c);
		if (isFailType(t2)) failures += t2;
		< _, _, loopElements > = head(c.labelStack);
		println("In loop at <stmt@\loc>, append result is <prettyPrintType(loopElements)>");
		c.labelStack = tail(c.labelStack);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \list(loopElements));	
	}
}

@doc{Check the type of Rascal statements: DoWhile}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> do <Statement bdy> while (<Expression cond>);`, Configuration c) {
	// TODO: Check domain values over iterations
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,doWhileLabel());
		c2.labelStack = < labelName, doWhileLabel(), \void() > + c2.labelStack;

		set[Symbol] failures = { };
		
		< c2, t1 > = checkStmt(bdy, c2);
		if (isFailType(t1)) failures += t1;
		
		< c2, t2 > = checkExp(cond, c2);
		if (isFailType(t2)) 
			failures += t2;
		else if (!isBoolType(t2))
			failures += makeFailType("Unexpected type <prettyPrintType(t2)>, expected type bool", cond@\loc);

		c2.labelStack = tail(c2.labelStack);				
		c = recoverEnvironments(c2, c);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \value());	
	} else {
		set[Symbol] failures = { };
		c.labelStack = < RSimpleName(""), doWhileLabel(), \void() > + c.labelStack;
		
		< c, t1 > = checkStmt(bdy, c);
		if (isFailType(t1)) failures += t1;

		< c, t2 > = checkExp(cond, c);
		if (isFailType(t2)) 
			failures += t2;
		else if (!isBoolType(t2))
			failures += makeFailType("Unexpected type <prettyPrintType(t2)>, expected type bool", cond@\loc);
		
		c.labelStack = tail(c.labelStack);
				
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \value());	
	}
}

@doc{Check the type of Rascal statements: For}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> for ( <{Expression ","}+ gens> ) <Statement bdy>`, Configuration c) {
	// TODO: Check domain values over iterations
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,forLabel());
		c2.labelStack = < labelName, forLabel(), \void() > + c2.labelStack;
		
		set[Symbol] failures = { };
		
		for (gen <- gens) { 
			< c2, t1 > = checkExp(gen, c2);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", gen@\loc);
		}
				
		< c2, t2 > = checkStmt(bdy, c2);
		if (isFailType(t2)) failures += t2;
		
		< _, _, loopElements > = head(c2.labelStack);
		
		c2.labelStack = tail(c2.labelStack);
		
		c = recoverEnvironments(c2, c);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \list(loopElements));	
	} else {
		set[Symbol] failures = { };
		c.labelStack = < RSimpleName(""), forLabel(), \void() > + c.labelStack;
		for (gen <- gens) { 
			< c, t1 > = checkExp(gen, c);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", gen@\loc);
		}
				
		< c, t2 > = checkStmt(bdy, c);
		if (isFailType(t2)) failures += t2;

		< _, _, loopElements > = head(c.labelStack);
		c.labelStack = tail(c.labelStack);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \list(loopElements));	
	}
}

@doc{Check the type of Rascal statements: IfThen (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> if ( <{Expression ","}+ conds> ) <Statement bdy>`, Configuration c) {
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,ifLabel());
		c2.labelStack = < labelName, ifLabel(), \void() > + c2.labelStack;
		
		set[Symbol] failures = { };
		
		for (cond <- conds) { 
			< c2, t1 > = checkExp(cond, c2);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c2, t2 > = checkStmt(bdy, c2);
		if (isFailType(t2)) failures += t2;
		
		c2.labelStack = tail(c2.labelStack);
		c = recoverEnvironments(c2, c);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \value());	
	} else {
		set[Symbol] failures = { };
		c.labelStack = < RSimpleName(""), ifLabel(), \void() > + c.labelStack;
		
		for (cond <- conds) { 
			< c, t1 > = checkExp(cond, c);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c, t2 > = checkStmt(bdy, c);
		if (isFailType(t2)) failures += t2;

		c.labelStack = tail(c.labelStack);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, \value());	
	}
}

@doc{Check the type of Rascal statements: IfThenElse (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> if ( <{Expression ","}+ conds> ) <Statement thenBody> else <Statement elseBody>`, Configuration c) {
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,ifLabel());
		c2.labelStack = < labelName, ifLabel(), \void() > + c2.labelStack;
		
		set[Symbol] failures = { };
		
		for (cond <- conds) { 
			< c2, t1 > = checkExp(cond, c2);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c2, t2 > = checkStmt(thenBody, c2);
		if (isFailType(t2)) failures += t2;

		< c2, t3 > = checkStmt(elseBody, c2);
		if (isFailType(t3)) failures += t3;
		
		c2.labelStack = tail(c2.labelStack);

		c = recoverEnvironments(c2, c);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, lub(t2,t3));	
	} else {
		set[Symbol] failures = { };
		c.labelStack = < RSimpleName(""), ifLabel(), \void() > + c.labelStack;
		
		for (cond <- conds) { 
			< c, t1 > = checkExp(cond, c);
			if (isFailType(t1)) 
				failures += t1;
			else if (!isBoolType(t1))
				failures += makeFailType("Unexpected type <prettyPrintType(t1)>, expected type bool", cond@\loc);
		}
				
		< c, t2 > = checkStmt(thenBody, c);
		if (isFailType(t2)) failures += t2;

		< c, t3 > = checkStmt(elseBody, c);
		if (isFailType(t3)) failures += t3;

		c.labelStack = tail(c.labelStack);
		
		if (size(failures) > 0)
			return markLocationFailed(c, stmt@\loc, failures);
		else
			return markLocationType(c, stmt@\loc, lub(t2,t3));	
	}
}

@doc{Check the type of Rascal statements: Switch}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> switch ( <Expression e> ) { <Case+ cases> }`, Configuration c) {
	println("WARNING: SWITCH STATEMENT NOT YET IMPLEMENTED");
	return < c, \void() >;
}

@doc{Check the type of Rascal statements: Fail (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`fail <Target target>;`, Configuration c) {
	if ((Target)`<Name n>` := target) {
		rn = convertName(n);
		// TODO: Check to see what category the label is in?
		if (rn notin c.labelEnv) return markLocationFailed(c, stmt@\loc, makeFailType("Target label not defined", stmt@\loc));
	}	
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Break (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`break <Target target>;`, Configuration c) {
	if ((Target)`<Name n>` := target) {
		rn = convertName(n);
		// TODO: Check to see what category the label is in?
		if (rn notin c.labelEnv) return markLocationFailed(c, stmt@\loc, makeFailType("Target label not defined", stmt@\loc));
	}	
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Continue (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`continue <Target target>;`, Configuration c) {
	if ((Target)`<Name n>` := target) {
		rn = convertName(n);
		// TODO: Check to see what category the label is in?
		if (rn notin c.labelEnv) return markLocationFailed(c, stmt@\loc, makeFailType("Target label not defined", stmt@\loc));
	}	
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Filter (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`filter;`, Configuration c) {
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Solve}
public CheckResult checkStmt(Statement stmt:(Statement)`solve ( <{QualifiedName ","}+ vars> <Bound bound> ) <Statement body>`, Configuration c) {
	return < c, \void() >;
}

@doc{Check the type of Rascal statements: Try}
public CheckResult checkStmt(Statement stmt:(Statement)`try <Statement body> <Catch+ handlers>`, Configuration c) {
	return < c, \void() >;
}

@doc{Check the type of Rascal statements: TryFinally}
public CheckResult checkStmt(Statement stmt:(Statement)`try <Statement body> <Catch+ handlers> finally <Statement fbody>`, Configuration c) {
	return < c, \void() >;
}

@doc{Check the type of Rascal statements: NonEmptyBlock (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<Label lbl> { <Statement+ stmts> }`, Configuration c) {
	if ((Label)`<Name n> :` := lbl) {
		labelName = convertName(n);
		if (labelExists(c,labelName)) c = addMessage(c,error("Cannot reuse label names: <n>", lbl@\loc));
		c2 = addLabel(c,labelName,lbl@\loc,blockLabel());
		c2.labelStack = < labelName, blockLabel(), \void() > + c2.labelStack;
		list[Symbol] stmtTypes = [];
		for (s <- stmts) { < c2, t1 > = checkStmt(s, c2); stmtTypes = stmtTypes + t1; }
		c2.labelStack = tail(c2.labelStack);
		c = recoverEnvironments(c2, c);
		if (isFailType(last(stmtTypes)))
			return markLocationFailed(c, stmt@\loc, last(stmtTypes));
		else
			return markLocationType(c, stmt@\loc, last(stmtTypes));	
	} else {
		list[Symbol] stmtTypes = [];
		c.labelStack = < RSimpleName(""), blockLabel(), \void() > + c.labelStack;
		for (s <- stmts) { < c, t1 > = checkStmt(s, c); stmtTypes = stmtTypes + t1; }
		c.labelStack = tail(c.labelStack);
		if (isFailType(last(stmtTypes)))
			return markLocationFailed(c, stmt@\loc, last(stmtTypes));
		else
			return markLocationType(c, stmt@\loc, last(stmtTypes));	
	}
}

@doc{Check the type of Rascal statements: EmptyStatement (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`;`, Configuration c) {
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: GlobalDirective (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`global <Type t> <{QualifiedName ","}+ names>;`, Configuration c) {
	throw "Not Implemented";
}

@doc{Check the type of Rascal statements: Assignment}
public CheckResult checkStmt(Statement stmt:(Statement)`<Assignable a> <Assignment op> <Statement s>`, Configuration c) {
	// First, evaluate the statement, which gives us the type that we will assign into the assignable. If this is a
	// failure, we cannot figure out the type of the assignable, so just return right away.
	< c, t1 > = checkStmt(s, c);
	if (isFailType(t1)) return markLocationFailed(c, stmt@\loc, t1);
	< c, t2 > = checkAssignment(op, a, t1, stmt@\loc, c);
	if (isFailType(t2)) return markLocationFailed(c, stmt@\loc, t2);
	return markLocationType(c, stmt@\loc, t2);
}

@doc{Check the type of Rascal statements: Return (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`return <Statement s>`, Configuration c) {
	< c, t1 > = checkStmt(s, c);
	if (!isFailType(t1) && !subtype(t1, c.expectedReturnType))
		return markLocationFailed(c, stmt@\loc, makeFailType("Invalid return type <prettyPrintType(t1)>, expected return type <prettyPrintType(c.expectedReturnType)>", stmt@\loc)); 
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Throw (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`throw <Statement s>`, Configuration c) {
	< c, t1 > = checkStmt(s, c);
	return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: Insert (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`insert <DataTarget dt> <Statement s>`, Configuration c) {
	set[Symbol] failures = { };

	< c, t1 > = checkStmt(s, c);
	if (isFailType(t1)) {
		failures += t1;
	}

	if ((DataTarget)`<Name n>:` := dt) {
		rn = convertName(n);
		// TODO: Check to see what category the label is in?
		if (rn notin c.labelEnv) {
			failures += makeFailType("Target label not defined", dt@\loc);
		} else if (visitLabel() !:= c.labelEnv[rn]) {
			failures += makeFailType("Target label must refer to a visit statement or expression", dt@\loc);
		} else {
			if (insideCase(c)) {
				Symbol cct = currentCaseType(c,rn);
				if (!isFailType(t1) && !subtype(t1,cct)) {
					failures += makeFailType("Inserted type <prettyPrintType(t1)> must be a subtype of case type <prettyPrintType(cct)>", stmt@\loc);
				} 
			} else {
				failures += makeFailType("insert cannot occur outside of a case", stmt@\loc);
			}
		}
	} else {
		if (insideCase(c)) {
			Symbol cct = currentCaseType(c,rn);
			if (!isFailType(t1) && !subtype(t1,cct)) {
				failures += makeFailType("Inserted type <prettyPrintType(t1)> must be a subtype of case type <prettyPrintType(cct)>", stmt@\loc);
			} 
		} else {
			failures += makeFailType("insert cannot occur outside of a case", stmt@\loc);
		}
	}
	
	
	if (size(failures) > 0)
		return markLocationFailed(c, stmt@\loc, failures);
	else
		return markLocationType(c, stmt@\loc, \void());
}

public Configuration addAppendTypeInfo(Configuration c, Symbol t, RName rn, set[LabelSource] ls, loc l) {
	throw "Not implemented";
}

public Configuration addAppendTypeInfo(Configuration c, Symbol t, set[LabelSource] ls, loc l) {
	possibleIndexes = [ idx | idx <- index(c.labelStack), c.labelStack[idx].labelSource in ls ];
	if (size(possibleIndexes) == 0) {
		c = addScopeError(c, "Cannot add append information, no valid surrounding context found", l);
	} else {
		c.labelStack[possibleIndexes[0]].labelType = lub(c.labelStack[possibleIndexes[0]].labelType, t);
	}
	return c;
}

@doc{Check the type of Rascal statements: Append (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`append <DataTarget dt> <Statement s>`, Configuration c) {
	set[Symbol] failures = { };

	< c, t1 > = checkStmt(s, c);
	if (isFailType(t1)) failures += t1;
	println("Type for append is <prettyPrintType(t1)>");
	if ((DataTarget)`<Name n>:` := dt) {
		rn = convertName(n);
		// TODO: Check to see what category the label is in?
		if (rn notin c.labelEnv) 
			failures += makeFailType("Target label not defined", dt@\loc);
		else
			c = addAppendTypeInfo(c, t1, rn, { forLabel(), whileLabel(), doWhileLabel() }, stmt@\loc);
	} else {
		c = addAppendTypeInfo(c, t1, { forLabel(), whileLabel(), doWhileLabel() }, stmt@\loc);
	}
	
	if (size(failures) > 0)
		return markLocationFailed(c, stmt@\loc, failures);
	else
		return markLocationType(c, stmt@\loc, \void());
}

@doc{Check the type of Rascal statements: FunctionDeclaration (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<FunctionDeclaration fd>`, Configuration c) {
	c = checkFunctionDeclaration(fd, true, c);
	return < c, \void() >;
}

@doc{Check the type of Rascal statements: LocalVariableDeclaration (DONE)}
public CheckResult checkStmt(Statement stmt:(Statement)`<LocalVariableDeclaration vd>;`, Configuration c) {
	if ((LocalVariableDeclaration)`<Declarator d>` := vd || (LocalVariableDeclaration)`dynamic <Declarator d>` := vd) {
		if ((Declarator)`<Type t> <{Variable ","}+ vars>` := d) {
			< c, rt > = convertAndExpandType(t,c);
			
			for (v <- vars) {
				if ((Variable)`<Name n> = <Expression init>` := v || (Variable)`<Name n>` := v) {
					if ((Variable)`<Name n> = <Expression init>` := v) {
						< c, t1 > = checkExp(init, c);
						if (!isFailType(t1) && !subtype(t1,rt)) 
							c = addScopeMessage(c, error("Initializer type <prettyPrintType(t1)> not assignable to variable of type <prettyPrintType(rt)>", v@\loc));						
					}
										
					RName rn = convertName(n);
					if (varCanShadow(c, rn)) {
						c = addVariable(c, rn, false, n@\loc, rt);
					} else {
						c = addScopeMessage(c, error("Illegal redeclaration of existing name", v@\loc));						
					}
				} 
			}
		}
	}
	
	return < c, \void() >;
}

public test bool callOrTreeExp1() = < _, \node()> := checkExp(parseExpression("\"mynode\"()"), newConfiguration());
public test bool callOrTreeExp2() = < _, \node()> := checkExp(parseExpression("\"mynode\"(1,2,false,5.3)"), newConfiguration());
public test bool callOrTreeExp3() = < _, \loc()> := checkExp(parseExpression("|project://Test/Project|(1,2,\<3,4\>,\<5,6\>)"), newConfiguration());
public test bool callOrTreeExp4() = < _, t> := checkExp(parseExpression("|project://Test/Project|(1.2,2,\<3,4\>,\<5,6\>)"), newConfiguration()) && isFailType(t);
public test bool callOrTreeExp5() = < _, t> := checkExp(parseExpression("|project://Test/Project|(1,2.3,\<3,4\>,\<5,6\>)"), newConfiguration()) && isFailType(t);
public test bool callOrTreeExp6() = < _, t> := checkExp(parseExpression("|project://Test/Project|(1,2,\<3.4,4\>,\<5,6\>)"), newConfiguration()) && isFailType(t);
public test bool callOrTreeExp7() = < _, t> := checkExp(parseExpression("|project://Test/Project|(1,2,\<3,4\>,\<5.5,6\>)"), newConfiguration()) && isFailType(t);
public test bool callOrTreeExp8() = < _, t> := checkExp(parseExpression("|project://Test/Project|(1,2)"), newConfiguration()) && isFailType(t);

public test bool reducerExp1() = < _, \int() > := checkExp(parseExpression("( 3 | it + x | x \<- [1..10] )"), addModule(newConfiguration(),RSimpleName("Tests"), |file:///tmp|));
public test bool reducerExp2() = < _, \real() > := checkExp(parseExpression("( 3.4 | it + x | x \<- [1..10] )"), addModule(newConfiguration(),RSimpleName("Tests"), |file:///tmp|));
public test bool reducerExp3() = < _, \list(\int()) > := checkExp(parseExpression("( [3] | it + x | x \<- [1..10] )"), addModule(newConfiguration(),RSimpleName("Tests"), |file:///tmp|));
public test bool reducerExp4() = < _, t > := checkExp(parseExpression("( [] | [it] | x \<- [1..10] )"), addModule(newConfiguration(),RSimpleName("Tests"), |file:///tmp|)) && isFailType(t);
public test bool reducerExp5() = < _, \list(\int()) > := checkExp(parseExpression("( [] | it + x | x \<- [1..10] )"), addModule(newConfiguration(),RSimpleName("Tests"), |file:///tmp|));

public test bool literalExp1() = < _, \int()> := checkExp(parseExpression("1"), newConfiguration());
public test bool literalExp2() = < _, \real()> := checkExp(parseExpression("1.1"), newConfiguration());
public test bool literalExp3() = < _, \rat()> := checkExp(parseExpression("1r2"), newConfiguration());
public test bool literalExp4() = < _, \bool()> := checkExp(parseExpression("true"), newConfiguration());
public test bool literalExp5() = < _, \bool()> := checkExp(parseExpression("false"), newConfiguration());
public test bool literalExp6() = < _, \datetime()> := checkExp(parseExpression("$2012-01-27"), newConfiguration());
public test bool literalExp7() = < _, \str()> := checkExp(parseExpression("\"hello world!\""), newConfiguration());
public test bool literalExp8() = < _, \loc()> := checkExp(parseExpression("|project://MyLang/src/myfile.rsc|"), newConfiguration());

public test bool assertStmt1() = < _, \bool()> := checkStmt(parseStatement("assert true;"), newConfiguration());
public test bool assertStmt2() = < _, t> := checkStmt(parseStatement("assert 5;"), newConfiguration()) && isFailType(t);
public test bool assertStmt3() = < _, \bool()> := checkStmt(parseStatement("assert true : \"or else!\";"), newConfiguration());
public test bool assertStmt4() = < _, t> := checkStmt(parseStatement("assert 5 : \"or else!\";"), newConfiguration()) && isFailType(t);
public test bool assertStmt5() = < _, t> := checkStmt(parseStatement("assert true : 10;"), newConfiguration()) && isFailType(t);
public test bool assertStmt6() = < _, t> := checkStmt(parseStatement("assert 5 : 10;"), newConfiguration()) && isFailType(t);
 
public test bool listExp1() = < _, \list(\void()) > := checkExp(parseExpression("[]"), newConfiguration());

public test bool setExp1() = < _, \set(\void()) > := checkExp(parseExpression("{}"), newConfiguration());

public test bool mapExp1() = < _, \map(\void(),\void()) > := checkExp(parseExpression("( )"), newConfiguration());








@doc{A compact representation of assignables}
data AssignableTree 
	= bracketNode(AssignableTree child)
	| variableNode(RName name)
	| subscriptNode(AssignableTree receiver, Symbol subscriptType)
	| fieldAccessNode(AssignableTree receiver, RName name)
	| ifDefinedOrDefaultNode(AssignableTree receiver, Symbol defaultType)
	| constructorNode(RName name, list[AssignableTree] children)
	| tupleNodeAT(list[AssignableTree] children)
	| annotationNode(AssignableTree receiver, RName name)
	;
	
@doc{Mark assignable trees with the source location of the assignable}
public anno loc AssignableTree@at;

@doc{Allows AssignableTree nodes to be annotated with types.}
public anno Symbol AssignableTree@otype;
public anno Symbol AssignableTree@atype;

@doc{Result of building the assignable tree.}
alias ATResult = tuple[Configuration, AssignableTree];

@doc{Extract a tree representation of the assignable and perform basic checks: Bracket (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`(<Assignable ar>)`, Configuration c) {
	< c, atree > = buildAssignableTree(ar, c);
	return < c, bracketNode(atree)[@otype=atree@otype][@atype=atree@atype][@at=assn@\loc] >;
}

@doc{Extract a tree representation of the assignable and perform basic checks: Variable (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<QualifiedName qn>`, Configuration c) {
	// TODO: Need to handle qualified names (i.e., names with ::) as well. This will just handle
	// regular names without qualifiers.
	n = convertName(qn);
	if (fcvExists(c, n)) {
		c.uses = c.uses + < c.fcvEnv[n], assn@\loc >;
		rt = c.store[c.fcvEnv[n]].rtype;
		return < c, variableNode(n)[@otype=rt][@atype=rt][@at=assn@\loc] >;
	} else {
		rt = \inferred(c.uniqueify);
		c.uniqueify = c.uniqueify + 1;  
		c = addVariable(c, n, true, qn@\loc, rt);
		return < c, variableNode(n)[@otype=rt][@atype=rt][@at=assn@\loc] >;
	}
}

@doc{Extract a tree representation of the assignable and perform basic checks: Subscript (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<Assignable ar> [ <Expression sub> ]`, Configuration c) {
	< c, atree > = buildAssignableTree(ar, c);
	< c, tsub > = checkExp(sub, c);
	
	if (!isFailType(atree@atype) && !concreteType(atree@atype)) {
		failure = makeFailType("Assignable <ar> must have an actual type before subscripting", assn@\loc);
		return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=failure][@at=assn@\loc] >;
	}
	if (!isFailType(atree@atype) && !isFailType(tsub)) {
		if (isListType(atree@atype) && isIntType(tsub)) {
			return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=getListElementType(atree@atype)][@at=assn@\loc] >;
		} else if (isMapType(atree@atype) && subtype(tsub,getMapDomainType(atree@atype))) {
			return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=getMapRangeType(atree@atype)][@at=assn@\loc] >;
		} else if (isNodeType(atree@atype) && isIntType(tsub)) {
			return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=\value()][@at=assn@\loc] >;
		} else if (isTupleType(atree@atype) && isIntType(tsub)) {
			return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=\value()][@at=assn@\loc] >;
		} else if (isRelType(atree@atype) && size(getRelFields(atree@atype)) == 2 && subtype(tsub,getRelFields(atree@atype)[0])) {
			return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=getRelFields(atree@atype)[1]][@at=assn@\loc] >;
		}
	}
	if (isFailType(atree@atype) || isFailType(tsub))
		return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=collapseFailTypes({atree@atype,tsub})][@at=assn@\loc] >;
	else
		return < c, subscriptNode(atree,tsub)[@otype=atree@otype][@atype=makeFailType("Cannot subscript assignable of type <prettyPrintType(atree@atype)>",assn@\loc)][@at=assn@\loc] >;
}

@doc{Extract a tree representation of the pattern and perform basic checks: FieldAccess (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<Assignable ar> . <Name fld>`, Configuration c) {
	< c, atree > = buildAssignableTree(ar, c);
	fldName = convertName(fld);
	
	if (!isFailType(atree@atype) && !concreteType(atree@atype)) {
		failure = makeFailType("Assignable <ar> must have an actual type before assigning to a field", assn@\loc);
		return < c, subscriptNode(atree, fldName)[@otype=atree@otype][@atype=failure][@at=assn@\loc] >;
	}
	
	if (!isFailType(atree@atype)) {
		tfield = computeFieldType(atree@atype, fldName, assn@\loc, c);
	
		if (!isFailType(tfield)) {
			if ((isLocType(atree@atype) || isDateTimeType(atree@atype)) && "<fld>" notin writableFields[atree@atype]) {
				tfield = makeFailType("Cannot update field <fld> on type <prettyPrintType(atree@atype)>",assn@\loc);
			}
		} 
		
		return < c, fieldAccessNode(atree, fldName)[@otype=atree@otype][@atype=tfield][@at=assn@\loc] >;
	}
	
	return < c, fieldAccessNode(atree,fldName)[@otype=atree@otype][@atype=atree@atype][@at=assn@\loc] >;
}

@doc{Extract a tree representation of the pattern and perform basic checks: IfDefinedOrDefault}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<Assignable ar> ? <Expression dflt>`, Configuration c) {
	< c, atree > = buildAssignableTree(ar, c);
	< c, tdef > = checkExp(dflt, c);
	
	// TODO: Finish implementation...
	throw "Not implemented";
}

@doc{Extract a tree representation of the pattern and perform basic checks: Constructor (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<Name n> ( <{Assignable ","}+ args> )`, Configuration c) {
	throw "Not implemented";
}

@doc{Extract a tree representation of the pattern and perform basic checks: Tuple (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`< <Assignable a1>, <{Assignable ","}* as> >`, Configuration c) {
	< c, atree > = buildAssignableTree(a1, c);
	list[AssignableTree] trees = [ atree ];

	for (ai <- as) {
		< c, atree > = buildAssignableTree(ai, c);
		trees = trees + atree;
	}
	
	failures = { t@atype |t <- trees, isFailType(t@atype) };
	
	if (size(failures) > 0)
		return < c, tupleNodeAT(trees)[@otype=collapseFailTypes(failures)][@atype=collapseFailTypes(failures)][@at=assn@\loc] >;
	else
		return < c, tupleNodeAT(trees)[@otype=\tuple([t@otype|t<-trees])][@atype=\tuple([t@otype|t<-trees])][@at=assn@\loc] >;
}

@doc{Extract a tree representation of the pattern and perform basic checks: Annotation (DONE)}
public ATResult buildAssignableTree(Assignable assn:(Assignable)`<Assignable ar> @ <Name an>`, Configuration c) {
	< c, atree > = buildAssignableTree(a1, c);
	aname = convertName(an);
	list[AssignableTree] trees = [ atree ];

	if (!isFailType(atree@atype) && !concreteType(atree@atype)) {
		failure = makeFailType("Assignable <ar> must have an actual type before assigning to an annotation", assn@\loc);
		return < c, annotationNode(atree,aname)[@otype=atree@otype][@atype=failure][@at=assn@\loc] >;
	}
	
	if (isNodeType(ttgt) || isADTType(ttgt)) {
		if (aname in c.annotationEnv, true in { subtype(atree@atype,ot) | ot <- c.store[c.annotationEnv[aname]].onTypes }) {
			aType = c.store[c.annotationEnv[aname]].rtype;
			return < c, annotationNode(atree,aname)[@otype=atree@otype][@atype=aType][@at=assn@\loc] >;
		} else {
			rt = makeFailType("Annotation <an> not declared on <prettyPrintType(atree@atype)> or its supertypes",assn@\loc);
			return < c, annotationNode(atree,aname)[@otype=atree@otype][@atype=rt][@at=assn@\loc] >;
		}
	} else {
		rt = makeFailType("Invalid type: expected node or ADT types, found <prettyPrintType(ttgt)>", assn@\loc);
		return < c, annotationNode(atree,aname)[@otype=atree@otype][@atype=rt][@at=assn@\loc] >;
	}
}

@doc{Check the type of Rascal assignments: IfDefined (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`?=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// Now, using the subject type, try to bind it to the assignable tree
	try {
		< c, atree > = bindAssignable(atree, st, c);
	} catch : {
		return markLocationFailed(cbak, l, makeFailType("Unable to bind subject type <prettyPrintType(st)> to assignable", l));
	}

	unresolved = { ati | /AssignableTree ati := atree, !((ati@atype)?) || !concreteType(ati@atype) } + { ati | /AssignableTree ati := atree, !((ati@otype)?) || !concreteType(ati@otype) };
	if (size(unresolved) > 0)
		return markLocationFailed(cbak, l, makeFailType("Type of assignable could not be computed", l));
	else
		return markLocationType(c, l, atree@otype);
}

@doc{Check the type of Rascal assignments: Division (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`/=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// If the assignment point or the overall type are not concrete, we cannot do
	// the assignment -- the subject type cannot influence the type here.
	if (!concreteType(atree@otype) || !concreteType(atree@atype)) return markLocationFailed(cbak, a@\loc, makeFailType("Cannot initialize variables using a += operation", a@\loc));
	
	// Check to ensure the division is valid. If so, the resulting type is the overall
	// type of the assignable, else it is the failure type generated by the operation.
	rt = computeDivisionType(atree@atype, st, l);
	if (isFailType(rt))
		return markLocationType(c, l, rt);
	else
		return markLocationType(c, l, atree@otype);  
}

@doc{Check the type of Rascal assignments: Product (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`*=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// If the assignment point or the overall type are not concrete, we cannot do
	// the assignment -- the subject type cannot influence the type here.
	if (!concreteType(atree@otype) || !concreteType(atree@atype)) return markLocationFailed(cbak, a@\loc, makeFailType("Cannot initialize variables using a += operation", a@\loc));
	
	// Check to ensure the product is valid. If so, the resulting type is the overall
	// type of the assignable, else it is the failure type generated by the operation.
	rt = computeProductType(atree@atype, st, l);
	if (isFailType(rt))
		return markLocationType(c, l, rt);
	else
		return markLocationType(c, l, atree@otype);  
}

@doc{Check the type of Rascal assignments: Intersection (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`&=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// If the assignment point or the overall type are not concrete, we cannot do
	// the assignment -- the subject type cannot influence the type here.
	if (!concreteType(atree@otype) || !concreteType(atree@atype)) return markLocationFailed(cbak, a@\loc, makeFailType("Cannot initialize variables using a += operation", a@\loc));
	
	// Check to ensure the intersection is valid. If so, the resulting type is the overall
	// type of the assignable, else it is the failure type generated by the operation.
	rt = computeIntersectionType(atree@atype, st, l);
	if (isFailType(rt))
		return markLocationType(c, l, rt);
	else
		return markLocationType(c, l, atree@otype);  
}

@doc{Check the type of Rascal assignments: Subtraction (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`-=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// If the assignment point or the overall type are not concrete, we cannot do
	// the assignment -- the subject type cannot influence the type here.
	if (!concreteType(atree@otype) || !concreteType(atree@atype)) return markLocationFailed(cbak, a@\loc, makeFailType("Cannot initialize variables using a += operation", a@\loc));
	
	// Check to ensure the subtraction is valid. If so, the resulting type is the overall
	// type of the assignable, else it is the failure type generated by the operation.
	rt = computeSubtractionType(atree@atype, st, l);
	if (isFailType(rt))
		return markLocationType(c, l, rt);
	else
		return markLocationType(c, l, atree@otype);  
}

@doc{Check the type of Rascal assignments: Default (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// Now, using the subject type, try to bind it to the assignable tree
	try {
		< c, atree > = bindAssignable(atree, st, c);
	} catch : {
		return markLocationFailed(cbak, l, makeFailType("Unable to bind subject type <prettyPrintType(st)> to assignable", l));
	}

	unresolved = { ati | /AssignableTree ati := atree, !((ati@atype)?) || !concreteType(ati@atype) } + { ati | /AssignableTree ati := atree, !((ati@otype)?) || !concreteType(ati@otype) };
	if (size(unresolved) > 0)
		return markLocationFailed(cbak, l, makeFailType("Type of assignable could not be computed", l));
	else
		return markLocationType(c, l, atree@otype);
}

@doc{Check the type of Rascal assignments: Addition (DONE)}
public CheckResult checkAssignment(Assignment assn:(Assignment)`+=`, Assignable a, Symbol st, loc l, Configuration c) {
	cbak = c;
	< c, atree > = buildAssignableTree(a, c);
	
	// If either the overall type or the type of the assignment point is a failure,
	// don't try to proceed further.
	if (isFailType(atree@otype) || isFailType(atree@atype)) return markLocationFailed(cbak, a@\loc, { atree@otype, atree@atype });

	// If the assignment point or the overall type are not concrete, we cannot do
	// the assignment -- the subject type cannot influence the type here.
	if (!concreteType(atree@otype) || !concreteType(atree@atype)) return markLocationFailed(cbak, a@\loc, makeFailType("Cannot initialize variables using a += operation", a@\loc));
	
	// Check to ensure the addition is valid. If so, the resulting type is the overall
	// type of the assignable, else it is the failure type generated by the operation.
	rt = computeAdditionType(atree@atype, st, l);
	if (isFailType(rt))
		return markLocationType(c, l, rt);
	else
		return markLocationType(c, l, atree@otype);  
}

@doc{Check the type of Rascal assignments: Append}
public CheckResult checkAssignment(Assignment assn:(Assignment)`<<=`, Assignable a, Symbol st, Configuration c) {
	< c, atree > = buildAssignableTree(a, c);
	if (isFailType(tRes) || isFailType(tTgt)) return markLocationFailed(c, assn@\loc, { tRes, tTgt });
	throw "Not yet implemented";
}

@doc{Result types for assignables, which include both the overall result type and the type of the immediate target.}
alias AssignableTypeResult = tuple[Configuration conf, Symbol result, Symbol target];
 
@doc{Bind variable types to variables in assignables: Bracket}
public ATResult bindAssignable(AssignableTree atree:bracketNode(AssignableTree child), Symbol st, Configuration c) {
	< c, newChild > = bindAssignable(child, st, c);
	return < c, atree[@otype=newChild@otype][@atype=newChild@atype] >;
}

@doc{Bind variable types to variables in assignables: Variable}
public ATResult bindAssignable(AssignableTree atree:variableNode(RName name), Symbol st, Configuration c) {
	Symbol currentType = c.store[c.fcvEnv[name]].rtype;
	if (c.store[c.fcvEnv[name]].inferred) {
		if (isInferredType(currentType)) {
			c.store[c.fcvEnv[name]].rtype = st;
		} else {
			c.store[c.fcvEnv[name]].rtype = lub(currentType, st);
		}
	} else if (!comparable(currentType, st)) {
		throw "Bind error, cannot bind subject of type <prettyPrintType(st)> to pattern of type <prettyPrintType(currentType)>";
	}
	return < c, atree[@otype=c.store[c.fcvEnv[name]].rtype][@atype=c.store[c.fcvEnv[name]].rtype] >;
}

@doc{Bind variable types to variables in assignables: Subscript}
public ATResult bindAssignable(AssignableTree atree:subscriptNode(AssignableTree receiver, Symbol stype), Symbol st, Configuration c) {
	// NOTE: We should not need to bind further down in this term. This is because, in a case
	// where we have a term like a[x], a MUST have a concrete type, so no parts of a could be
	// inferred (and thus there is nothing to bind).
	return < c, atree >; 
}

@doc{Bind variable types to variables in assignables: FieldAccess}
public ATResult bindAssignable(AssignableTree atree:fieldAccessNode(AssignableTree receiver, RName name), Symbol st, Configuration c) {
	// NOTE: We should not need to bind further down in this term. This is because, in a case
	// where we have a term like a.x, a MUST have a concrete type, so no parts of a could be
	// inferred (and thus there is nothing to bind).
	return < c, atree >; 
}

@doc{Bind variable types to variables in assignables: IfDefinedOrDefault}
public ATResult bindAssignable(AssignableTree atree:ifDefinedOrDefaultNode(AssignableTree receiver, Symbol dtype), Symbol st, Configuration c) {
	throw "Not implemented";
}

@doc{Check the type of Rascal assignables: Constructor}
public ATResult bindAssignable(AssignableTree atree:constructorNode(RName name,list[AssignableTree] children), Symbol st, Configuration c) {
	throw "Not implemented";
}

@doc{Bind variable types to variables in assignables: Tuple}
public ATResult bindAssignable(AssignableTree atree:tupleNodeAT(list[AssignableTree] children), Symbol st, Configuration c) {
	if (isTupleType(st)) {
		list[Symbol] tflds = getTupleFields(st);
		if (size(tflds) == size(children)) {
			set[Symbol] failures = { };
			list[AssignableTree] newChildren = [ ];
			for (idx <- index(children)) {
				< c, newTree > = bindAssignable(children[idx], tflds[idx], c);
				newChildren = newChildren + newTree;
				if (isFailType(newTree@atype)) failures = failures + newTree@atype;
				if (isFailType(newTree@otype)) failures = failures + newTree@otype;
			}
			if (size(failures) > 0)
				return < c, atree[children = newChildren][@atype=collapseFailTypes(failures)][@otype=collapseFailTypes(failures)] >;
			else
				return < c, atree[children = newChildren][@atype=st][@otype=st] >; 
		} else {
			throw "Cannot bind tuple assignable with arity <size(children)> to tuple type <prettyPrintType(st)>";
		}
	} else {
		throw "Cannot bind tuple assignable to non-tuple type <prettyPrintType(st)>";
	}
	return < c, atree >;
}

@doc{Check the type of Rascal assignables: Annotation}
public ATResult bindAssignable(AssignableTree atree:annotationNode(AssignableTree receiver, RName name), Symbol st, Configuration c) {
	// NOTE: We should not need to bind further down in this term. This is because, in a case
	// where we have a term like a@x, a MUST have a concrete type, so no parts of a could be
	// inferred (and thus there is nothing to bind).
	return < c, atree >; 
}

@doc{Check the type of the components of a declaration: Variable}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> <Type t> <{Variable ","}+ vars>;`, bool descend, Configuration c) {
	< c, rt > = convertAndExpandType(t,c);

	for (v <- vars, v@\loc notin c.definitions<1>, v@\loc notin {l | error(_,l) <- c.messages}) {
		// If v@\loc is not in c.definitions and not in scope errors, we haven't processed
		// it yet, so process it now. We process the expression now too, even if not descending,
		// since we want to process the expressions in name order. 
		if ((Variable)`<Name n> = <Expression init>` := v || (Variable)`<Name n>` := v) {
			if ((Variable)`<Name n> = <Expression init>` := v) {
				< c, t1 > = checkExp(init, c);
				if (!isFailType(t1) && !subtype(t1,rt)) 
					c = addScopeMessage(c, error("Initializer type <prettyPrintType(t1)> not assignable to variable of type <prettyPrintType(rt)>", v@\loc));						
			}
								
			RName rn = convertName(n);
			if (varCanShadow(c, rn)) {
				c = addVariable(c, rn, false, getVis(vis), v@\loc, rt);
			} else {
				c = addScopeMessage(c, error("Illegal redeclaration of existing name", v@\loc));						
			}
		} 
	}
	
	return c;
}

@doc{Check the type of the components of a declaration: Annotation}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> anno <Type annoType> <Type onType> @ <Name n>;`, bool descend, Configuration c) {
	// NOTE: We ignore descend here. There is nothing that is done here that should be deferred until
	// later in declaration processing.
	
	if (decl@\loc notin c.definitions<1>) {
		// TODO: Check for conversion errors
		< c, at > = convertAndExpandType(annoType,c);
		< c, ot > = convertAndExpandType(onType,c);
		
		rn = convertName(n);
		
		c = addAnnotation(c,rn,at,ot,getVis(vis),decl@\loc);
	}
	return c;	
}

@doc{Check the type of the components of a declaration: Alias}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> alias <UserType ut> = <Type t>;`, bool descend, Configuration c) {
	// Add the alias, but only if it isn't already defined. If it is defined, the location
	// will be in definitions
	if (decl@\loc notin c.definitions<1>) { 
		// TODO: Check for convert errors
		< c, utype > = convertAndExpandUserType(ut,c);
		
		// Extract the name and parameters
		utypeName = getUserTypeName(utype);
		utypeParams = getUserTypeParameters(utype);
		
		// Add the alias into the type environment
		// TODO: Check to make sure this is possible
		c = addAlias(c,RSimpleName(utypeName),getVis(vis),decl@\loc,\alias(utypeName,utypeParams,\void()));
	}

	// If we can descend, process the aliased type as well, assigning it into
	// the alias.
	if (descend) {
		// If we descend, we also want to add the constructors; if not, we are just
		// adding the ADT into the type environment. We get the adt type out of
		// the store by looking up the definition from this location.
		aliasId = getOneFrom(invert(c.definitions)[decl@\loc]);
		aliasType = c.store[aliasId].rtype;
		// TODO: Check for convert errors
		< c, aliasedType > = convertAndExpandType(t,c);
		c.store[aliasId].rtype = \alias(aliasType.name, aliasType.parameters, aliasedType);
	}
	
	return c;
}

@doc{Check the type of the components of a declaration: Tag}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> tag <Kind k> <Name n> on <{Type ","}+ ts>;`, bool descend, Configuration c) {
	// TODO: Add descend code here; we should introduce the name, but not descend into the type.

	if (decl@\loc notin c.definitions<1>) {
		tk = convertKind(k);
		rn = convertName(n);
		set[Symbol] typeset = { };
		for (t <- ts) {
			< c, rt > = convertAndExpandType(t, c);
			typeset = typeset + rt;
		}
		// TODO: Make sure the add if safe first...
		c = addTag(c, tk, rn, typeset, getVis(vis), decl@\loc);
	}
	
	return c;
}

@doc{Check the type of the components of a declaration: DataAbstract}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> data <UserType ut>;`, bool descend, Configuration c) {
	// NOTE: We ignore descend here. There is nothing that is done here that should be deferred until
	// later in declaration processing.
	if (decl@\loc notin c.definitions<1>) {
		// TODO: Check for convert errors
		< c, utype > = convertAndExpandUserType(ut,c);
		
		// Extract the name and parameters
		utypeName = getUserTypeName(utype);
		utypeParams = getUserTypeParameters(utype);
		
		// Add the ADT into the type environment
		// TODO: Check to make sure this is possible
		c = addADT(c,RSimpleName(utypeName),getVis(vis),decl@\loc,\adt(utypeName,utypeParams));
	}
	return c;
}

@doc{Check the type of the components of a declaration: Data}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<Tags tags> <Visibility vis> data <UserType ut> = <{Variant "|"}+ vs>;`, bool descend, Configuration c) {
	// Add the ADT definition, but only if we haven't already added the definition
	// at this location. If we have, we can just use it if we need it.
	if (decl@\loc notin c.definitions<1>) { 
		// TODO: Check for convert errors
		< c, utype > = convertAndExpanUserType(ut,c);
		if (\user(_,_) !:= utype) throw "Conversion error: type for user type <ut> should be user type, not <prettyPrintType(utype)>";
		
		// Extract the name and parameters
		utypeName = getUserTypeName(utype);
		utypeParams = getUserTypeParameters(utype);
		
		// Add the ADT into the type environment
		// TODO: Check to make sure this is possible
		c = addADT(c,RSimpleName(utypeName),getVis(vis),decl@\loc,\adt(utypeName,utypeParams));
	}
	
	if (descend) {
		// If we descend, we also want to add the constructors; if not, we are just
		// adding the ADT into the type environment. We get the adt type out of
		// the store by looking up the definition from this location.
		adtType = c.store[getOneFrom(invert(c.definitions)[decl@\loc])].rtype;
	
		// Now add all the constructors
		// TODO: Check here for overlap problems
		for (Variant vr:(Variant)`<Name vn> ( < {TypeArg ","}* vargs > )` <- vs) {
			// TODO: Check for convert errors
			list[Symbol] targs = [ ];
			for (varg <- vargs) { < c, vargT > = convertAndExpandTypeArg(varg, c); targs = targs + vargT; } 
			cn = convertName(vn);
			c = addConstructor(c, cn, vr@\loc, Symbol::\cons(adtType,targs)); 		
		}
	}
	
	return c;
}

@doc{Check the type of the components of a declaration: Function}
public Configuration checkDeclaration(Declaration decl:(Declaration)`<FunctionDeclaration fd>`, bool descend, Configuration c) {
	return checkFunctionDeclaration(fd,descend,c);
}

@doc{Check function declarations: Abstract}
public Configuration checkFunctionDeclaration(FunctionDeclaration fd:(FunctionDeclaration)`<Tags tags> <Visibility vis> <Signature sig>;`, bool descend, Configuration c) {
	// TODO: Enforce that this is a java function?
	rn = getFunctionName(sig);

	// First, check to see if we have processed this declaration before. If we have, just get back the
	// id for the function, we don't want to create a new entry for it.
	if (fd@\loc notin c.definitions<1>) { 
		// Strip other functions and variables out of the environment. We do this so we have an appropriate environment
		// for typing the patterns in the function signature. Names used in these patterns cannot be existing variables
		// and/or functions that are live in the current environment. Also, this way we can just get the type and drop
		// all the changes that would be made to the environment.
		cFun = c[fcvEnv = ( ename : c.fcvEnv[ename] | ename <- c.fcvEnv<0>, constructor(_,_,_,_) := c.store[c.fcvEnv[ename]] )];
			
		// Put the function in, so we can enter the correct scope. This also puts the function name into the
		// scope -- we don't want to inadvertently use the function name as the name of a pattern variable,
		// and this makes sure we find it when checking the patterns in the signature.
		cFun = addFunction(cFun, rn, Symbol::\func(\void(),[]), isVarArgs(sig), getVis(vis), fd@\loc);
		< cFun, tFun > = processSignature(sig, cFun);
	
		// We now have the function type. So, we can throw cFun away, and add this as a proper function
		// into the scope. NOTE: This can be a failure type.
		c = addFunction(c, rn, tFun, isVarArgs(sig), getVis(vis), fd@\loc);
	} else {
		funId = getOneFrom(invert(c.definitions)[fd@\loc]);
		c.stack = funId + c.stack;
	}	
	
	// Normally we would now descend into the body. However, here we don't have one. So, pop the stack
	// and exit. NOTE: The names in the parameters are not in scope -- we calculated the type (which added
	// the names) in cFun, but then threw everything but the type away.
	c.stack = tail(c.stack);
	return c;
}

@doc{Check function declarations: Expression}
public Configuration checkFunctionDeclaration(FunctionDeclaration fd:(FunctionDeclaration)`<Tags tags> <Visibility vis> <Signature sig> = <Expression exp>;`, bool descend, Configuration c) {
	rn = getFunctionName(sig);

	if (fd@\loc notin c.definitions<1>) { 
		cFun = c[fcvEnv = ( ename : c.fcvEnv[ename] | ename <- c.fcvEnv<0>, constructor(_,_,_,_) := c.store[c.fcvEnv[ename]] )];
		cFun = addFunction(cFun, rn, Symbol::\func(\void(),[]), isVarArgs(sig), getVis(vis), fd@\loc);
		< cFun, tFun > = processSignature(sig, cFun);
		c = addFunction(c, rn, tFun, isVarArgs(sig), getVis(vis), fd@\loc);
	} else {
		funId = getOneFrom(invert(c.definitions)[fd@\loc]);
		c.stack = funId + c.stack;
	}	
	
	if (descend) {
		// Process the signature, but this time in a copy of the current environment
		// without the names stripped out (since these names will be visible in the
		// body of the function).
		funId = head(c.stack);
		funType = c.store[funId].rtype;
		cFun = c;
		if (!isFailType(funType)) {
			cFun = setExpectedReturn(c, getFunctionReturnType(funType));
		} else {
			cFun = setExpectedReturn(c, \void());
		}
		< cFun, tFun > = processSignature(sig, cFun);
		< cFun, tExp > = checkExp(exp, cFun);
		if (!isFailType(tExp) && !subtype(tExp, cFun.expectedReturnType))
			cFun = addScopeMessage(cFun,error("Unexpected type: type of body expression, <prettyPrintType(tExp)>, must be a subtype of the function return type, <prettyPrintType(cFun.expectedReturnType)>", exp@\loc));
		c = recoverEnvironmentsAfterCall(cFun, c);
	}

	c.stack = tail(c.stack);
	return c;
}

@doc{Check function declarations: Conditional}
public Configuration checkFunctionDeclaration(FunctionDeclaration fd:(FunctionDeclaration)`<Tags tags> <Visibility vis> <Signature sig> = <Expression exp> when <{Expression ","}+ conds>;`, bool descend, Configuration c) {
	rn = getFunctionName(sig);

	if (fd@\loc notin c.definitions<1>) { 
		cFun = c[fcvEnv = ( ename : c.fcvEnv[ename] | ename <- c.fcvEnv<0>, constructor(_,_,_,_) := c.store[c.fcvEnv[ename]] )];
		cFun = addFunction(cFun, rn, Symbol::\func(\void(),[]), isVarArgs(sig), getVis(vis), fd@\loc);
		< cFun, tFun > = processSignature(sig, cFun);
		c = addFunction(c, rn, tFun, isVarArgs(sig), getVis(vis), fd@\loc);
	} else {
		funId = getOneFrom(invert(c.definitions)[fd@\loc]);
		c.stack = funId + c.stack;
	}	
	
	if (descend) {
		// Process the signature, but this time in a copy of the current environment
		// without the names stripped out (since these names will be visible in the
		// body of the function).
		funId = head(c.stack);
		funType = c.store[funId].rtype;
		cFun = c;
		if (!isFailType(funType)) {
			cFun = setExpectedReturn(c, getFunctionReturnType(funType));
		} else {
			cFun = setExpectedReturn(c, \void());
		}
		< cFun, tFun > = processSignature(sig, cFun);
		for (cond <- conds) {
			< cFun, tCond > = checkExp(cond, cFun);
			if (!isFailType(tCond) && !isBoolType(tCond))
				cFun = addScopeMessage(cFun,error("Unexpected type: condition should be of type bool, not type <prettyPrintType(tCond)>", cond@\loc));
		}
		< cFun, tExp > = checkExp(exp, cFun);
		if (!isFailType(tExp) && !subtype(tExp, cFun.expectedReturnType))
			cFun = addScopeMessage(cFun,error("Unexpected type: type of body expression, <prettyPrintType(tExp)>, must be a subtype of the function return type, <prettyPrintType(cFun.expectedReturnType)>", exp@\loc));
		c = recoverEnvironmentsAfterCall(cFun, c);
	}

	c.stack = tail(c.stack);
	return c;
}

@doc{Check function declarations: Default}
public Configuration checkFunctionDeclaration(FunctionDeclaration fd:(FunctionDeclaration)`<Tags tags> <Visibility vis> <Signature sig> <FunctionBody body>`, bool descend, Configuration c) {
	rn = getFunctionName(sig);

	println("Checking function <prettyPrintName(rn)>");
	
	if (fd@\loc notin c.definitions<1>) { 
		cFun = c[fcvEnv = ( ename : c.fcvEnv[ename] | ename <- c.fcvEnv<0>, constructor(_,_,_,_) := c.store[c.fcvEnv[ename]] )];
		cFun = addFunction(cFun, rn, Symbol::\func(\void(),[]), isVarArgs(sig), getVis(vis), fd@\loc);
		< cFun, tFun > = processSignature(sig, cFun);
		c = addFunction(c, rn, tFun, isVarArgs(sig), getVis(vis), fd@\loc);
	} else {
		funId = getOneFrom(invert(c.definitions)[fd@\loc]);
		c.stack = funId + c.stack;
	}	
	
	if (descend) {
		// Process the signature, but this time in a copy of the current environment
		// without the names stripped out (since these names will be visible in the
		// body of the function).
		funId = head(c.stack);
		funType = c.store[funId].rtype;
		cFun = c;
		if (!isFailType(funType)) {
			cFun = setExpectedReturn(c, getFunctionReturnType(funType));
		} else {
			cFun = setExpectedReturn(c, \void());
		}
		< cFun, tFun > = processSignature(sig, cFun);
		if ((FunctionBody)`{ <Statement* ss> }` := body) {
			for (stmt <- ss) {
				< cFun, tStmt > = checkStmt(stmt, cFun);
			}
		}
		c = recoverEnvironmentsAfterCall(cFun, c);
	}

	c.stack = tail(c.stack);
	return c;
}

@doc{Process function signatures: WithThrows}
public CheckResult processSignature(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps> throws <{Type ","}+ exs>`, Configuration c) {
	// TODO: Do something with the exception information
	< c, rType > = convertAndExpandType(t,c);

	< c, ptTuple > = checkParameters(ps, c);
	list[Symbol] parameterTypes = getTupleFields(ptTuple);
	paramFailures = { pt | pt <- parameterTypes, isFailType(pt) };
	funType = \void();
	if (size(paramFailures) > 0) {
		funType = collapseFailTypes(paramFailures + makeFailType("Could not calculate function type because of errors calculating the parameter types", sig@\loc));		
	} else {
		funType = makeFunctionTypeFromTuple(rType, isVarArgs(sig), \tuple(parameterTypes));
	}

	return < c, funType >;
}

@doc{Process function signatures: NoThrows}
public CheckResult processSignature(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps>`, Configuration c) {
	< c, rType > = convertAndExpandType(t,c);

	< c, ptTuple > = checkParameters(ps, c);
	list[Symbol] parameterTypes = getTupleFields(ptTuple);
	paramFailures = { pt | pt <- parameterTypes, isFailType(pt) };
	funType = \void();
	if (size(paramFailures) > 0) {
		funType = collapseFailTypes(paramFailures + makeFailType("Could not calculate function type because of errors calculating the parameter types", sig@\loc));		
	} else {
		funType = makeFunctionTypeFromTuple(rType, isVarArgs(sig), \tuple(parameterTypes));
	}

	return < c, funType >;
}

@doc{Extract the function modifiers from the signature.}
public RName getModifiers(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps> throws <{Type ","}+ exs>`) = getModifiers(mds);
public RName getModifiers(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps>`) = getModifiers(mds);

@doc{Extract the function modifiers from the list of modifiers.}
set[Modifier] getModifiers(FunctionModifiers fmods:(FunctionModifiers)`<FunctionModifier* fms>`) {
	return { getModifier(m) | m <- fms };
}

@doc{Extract the function name from the signature.}
public RName getFunctionName(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps> throws <{Type ","}+ exs>`) = convertName(n);
public RName getFunctionName(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> <Parameters ps>`) = convertName(n);

@doc{Check to see if the function is a varargs function.}
public bool isVarArgs(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> ( <Formals fmls> ) throws <{Type ","}+ exs>`) = false;
public bool isVarArgs(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> ( <Formals fmls> ... ) throws <{Type ","}+ exs>`) = true;
public bool isVarArgs(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> ( <Formals fmls> )`) = false;
public bool isVarArgs(Signature sig:(Signature)`<FunctionModifiers mds> <Type t> <Name n> ( <Formals fmls> ... )`) = true;

//
// TODO: The code for importing signature items duplicates a fair amount of the declaration
// code. Refactor this to prevent duplication.
//
@doc{Import a signature item: Alias}
public Configuration importAlias(RName aliasName, UserType aliasType, Type aliasedType, loc at, Vis vis, bool descend, Configuration c) {
	// If we are not descending, we just record the name in the type environment. If
	// we are descending, we also process the type parameters and the aliased type.
	if (!descend) {
		c = addAlias(c,aliasName,vis,at,\alias(prettyPrintName(aliasName),[],\void()));
	} else {
		aliasId = getOneFrom(invert(c.definitions)[at]);
		< c, utype > = convertAndExpandUserType(aliasType, c);
		utypeParams = getUserTypeParameters(utype);
		< c, rt > = convertAndExpandType(aliasedType,c);
		c.store[aliasId].rtype = \alias(prettyPrintName(aliasName), utypeParams, rt);
	}
	return c;
}

@doc{Import a signature item: Function}
public Configuration importFunction(RName functionName, Signature sig, loc at, Vis vis, Configuration c) {
	// This looks confusing, but is actually fairly simple. We build a new configuration with a modified
	// name environment that only includes constructors. This is done because the function parameters can
	// refer to existing constructors, but not to existing functions or variables. We then add the function
	// into the environment and process the function signature, calculating the type. Finally, we add the
	// function using the original environment, ensuring we don't actually lose the name info and/or add
	// parameter names into scope.
	// NOTE: There is no descend option here. Instead we process imports of function and variable names
	// from top to bottom, and don't descend into them at any point.
	cFun = c[fcvEnv = ( ename : c.fcvEnv[ename] | ename <- c.fcvEnv<0>, constructor(_,_,_,_) := c.store[c.fcvEnv[ename]] )];
	cFun = addFunction(cFun, functionName, Symbol::\func(\void(),[]), isVarArgs(sig), vis, at);
	< cFun, tFun > = processSignature(sig, cFun);
	if (!varCanShadow(c, functionName))
		c = addScopeWarning(c,"Imported function masks already-imported variable, constructor, or function of the same name", at);
	c = addFunction(c, functionName, tFun, isVarArgs(sig), vis, at);
	return c;
}

@doc{Import a signature item: Variable}
public Configuration importVariable(RName variableName, Type variableType, loc at, Vis vis, Configuration c) {
	< c, rt > = convertAndExpandType(variableType,c);
	if (!varCanShadow(c, variableName))
		c = addScopeWarning(c,"Imported variable masks already-imported variable, constructor, or function of the same name", at);
	return addVariable(c, variableName, false, vis, at, rt);						
}

@doc{Import a signature item: ADT}
public Configuration importADT(RName adtName, UserType adtType, loc at, Vis vis, bool descend, Configuration c) {
	// If we are not descending, we just record the name in the type environment. If
	// we are descending, we also process the type parameters.
	if (!descend) {
		c = addADT(c,adtName,vis,at,\adt(prettyPrintName(adtName),[]));
	} else {
		adtId = getOneFrom(invert(c.definitions)[at]);
		< c, utype > = convertAndExpandUserType(adtType,c);
		utypeParams = getUserTypeParameters(utype);
		c.store[adtId].rtype = \adt(prettyPrintName(adtName),utypeParams);
	}
	return c;
}

@doc{Import a signature item: Constructor}
public Configuration importConstructor(RName conName, UserType adtType, list[TypeArg] argTypes, loc adtAt, loc at, Vis vis, Configuration c) {
	// NOTE: We do not have a separate descend stage. Instead, we just add these after the types (aliases
	// and ADTs) have already been added. These are added before functions, though, since they may be
	// used in the function parameters.
	rt = c.store[getOneFrom(invert(c.definitions)[adtAt])].rtype;
	list[Symbol] targs = [ ];
	for (varg <- argTypes) { < c, vargT > = convertAndExpandTypeArg(varg, c); targs = targs + vargT; } 
	return addConstructor(c, conName, at, Symbol::\cons(rt,targs)); 		
}

@doc{Import a signature item: Annotation}
public Configuration importAnnotation(RName annName, Symbol annType, Symbol onType, loc at, Vis vis, Configuration c) {
	// NOTE: We also do not descend here. We just process these once, after all the types have been added.
	< c, atype > = convertAndExpandType(annType,c);
	< c, otype > = convertAndExpandType(onType,c);
	return addAnnotation(c,annName,atype,otype,vis,at);
}

@doc{Import a signature item: Tag}
public Configuration importTag(RName tagName, TagKind tagKind, list[Symbol] taggedTypes, loc at, Vis vis, bool descend, Configuration c) {
	// If we are not descending, we just add the tag name into the environment. If we
	// are descending, we process all the types in the tag (the "tagged types") as well.
	if (!descend) {
		c = addTag(c, tagKind, tagName, { }, vis, at);
	} else {
		tagId = getOneFrom(invert(c.definitions)[at]);
		set[Symbol] typeset = { };
		for (t <- taggedTypes) {
			< c, rt > = convertAndExpandType(t, c);
			typeset = typeset + rt;
		}
		c.store[tagId].onTypes = typeset;
	}
	return c;
}

@doc{Check a given module, including loading the imports and extends items for the module.}
public Configuration checkModule(Module md:(Module)`<Header header> <Body body>`, Configuration c) {
	moduleName = getHeaderName(header);
	importList = getHeaderImports(header);
	map[RName,RSignature] sigMap = ( );
	map[RName,bool] isExtends = ( );
	map[RName,int] moduleIds = ( );
	map[RName,loc] moduleLocs = ( );
	list[RName] importOrder = [ ];
	
	c = addModule(c, moduleName, md@\loc);
	currentModuleId = head(c.stack);
			
	// Get the information about each import, including the module signature
	for (importItem <- importList) {
		if ((Import)`import <ImportedModule im>;` := importItem || (Import)`extend <ImportedModule im>;` := importItem) {
			modName = getNameOfImportedModule(im);
			modTree = getModuleParseTree(prettyPrintName(modName));
			sigMap[modName] = getModuleSignature(modTree);
			moduleLocs[modName] = modTree@\loc;
			importOrder = importOrder + modName;
			c = addModule(c,modName,modTree@\loc);
			moduleIds[modName] = head(c.stack);
			c = popModule(c);
			isExtends[modName] = (Import)`extend <ImportedModule im>;` := importItem;		
		} else {
			// This is where we would need to add support for syntax.
			;
		}
	}
	
	// Add all the aliases and ADTs from each module without descending. Do tags here to, although
	// (when they are really used) we need to add them in a reasonable order. Right now we just
	// ignore them. So, TODO: Handle tags appropriately.
	for (modName <- importOrder) {
		sig = sigMap[modName];
		c.stack = ( isExtends[modName] ? currentModuleId : moduleIds[modName] ) + c.stack;
		for (item <- sig.datatypes) c = importADT(item.adtName, item.adtType, item.at, publicVis(), false, c);
		for (item <- sig.aliases) c = importAlias(item.aliasName, item.aliasType, item.aliasedType, item.at, publicVis(), false, c);
		for (item <- sig.tags) c = importTag(item.tagName, item.tagKind, item.taggedTypes, item.at, publicVis(), false, c);
		c.stack = tail(c.stack);
	}

	// Now, descend into each alias and ADT, ensuring all parameters are correctly added and the
	// aliased type is handled correctly. As above, we do tags here as well.
	for (modName <- importOrder) {
		sig = sigMap[modName];
		c.stack = ( isExtends[modName] ? currentModuleId : moduleIds[modName] ) + c.stack;
		for (item <- sig.datatypes) c = importADT(item.adtName, item.adtType, item.at, publicVis(), true, c);
		for (item <- sig.aliases) c = importAlias(item.aliasName, item.aliasType, item.aliasedType, item.at, publicVis(), true, c);
		for (item <- sig.tags) c = importTag(item.tagName, item.tagKind, item.taggedTypes, item.at, publicVis(), true, c);
		c.stack = tail(c.stack);
	}

	// Add constructors next, ensuring they are visible for the imported functions.
	// NOTE: This is one area where we could have problems. Once the checker is working
	// correctly, TODO: calculate the types in the signature, so we don't risk clashes
	// over constructor names (or inadvertent visibility of constructor names) that would
	// not have been an issue before, when we did not have parameters with patterns.
	for (modName <- importOrder) {
		sig = sigMap[modName];
		c.stack = ( isExtends[modName] ? currentModuleId : moduleIds[modName] ) + c.stack;
		for (item <- sig.publicConstructors) c = importConstructor(item.conName, item.adtType, item.argTypes, item.adtAt, item.at, publicVis(), c);
		c.stack = tail(c.stack);
	}
	
	// Now, bring in all public names, including annotations, public vars, and public functions.
	for (modName <- importOrder) {
		sig = sigMap[modName];
		c.stack = ( isExtends[modName] ? currentModuleId : moduleIds[modName] ) + c.stack;
		for (item <- sig.privateVariables) c = importVariable(item.variableName, item.variableType, item.at, publicVis(), c);
		for (item <- sig.publicFunctions) c = importFunction(item.functionName, item.sig, item.at, publicVis(), c);
		for (item <- sig.annotations) c = importAnnotation(item.annName, item.annType, item.onType, item.at, publicVis(), c);
		c.stack = tail(c.stack);
	}
	
	// Now, bring in the private names, but only for modules that are imported using extends
	for (modName <- importOrder, isExtends[modName]) {
		sig = sigMap[modName];
		c.stack = currentModuleId + c.stack;
		for (item <- sig.privateVariables) c = importVariable(item.variableName, item.variableType, item.at, privateVis(), c);
		for (item <- sig.publicFunctions) c = importFunction(item.functionName, item.sig, item.at, privateVis(), c);
		c.stack = tail(c.stack);
	}
			
	// Process the current module
	if ((Body)`<Toplevel* tls>` := body) {
		list[Declaration] typesAndTags = [ ];
		list[Declaration] annotations = [ ];
		list[Declaration] names = [ ];
		
		c.stack = currentModuleId + c.stack;
		
		for ((Toplevel)`<Declaration decl>` <- tls) {
			switch(decl) {
				case (Declaration)`<Tags _> <Visibility _> <Type _> <{Variable ","}+ _> ;` : names = names + decl;
				case (Declaration)`<Tags _> <Visibility _> anno <Type _> <Type _> @ <Name _>;` : annotations = annotations + decl;
				case (Declaration)`<Tags _> <Visibility _> alias <UserType _> = <Type _> ;` : typesAndTags = typesAndTags + decl;
				case (Declaration)`<Tags _> <Visibility _> tag <Kind _> <Name _> on <{Type ","}+ _> ;` : typesAndTags = typesAndTags + decl;
				case (Declaration)`<Tags _> <Visibility _> data <UserType _> ;` : typesAndTags = typesAndTags + decl;
				case (Declaration)`<Tags _> <Visibility _> data <UserType _> = <{Variant "|"}+ _> ;` : typesAndTags = typesAndTags + decl;
				case (Declaration)`<FunctionDeclaration _>` : names = names + decl;
			}
		}

		// Introduce the type names into the environment
		for (t <- typesAndTags) c = checkDeclaration(t,false,c);
		
		// Now, actually process the type names
		for (t <- typesAndTags) c = checkDeclaration(t,true,c);
		
		// Next, process the annotations
		for (t <- annotations) c = checkDeclaration(t,true,c);
		
		// Next, introduce names into the environment
		for (t <- names) c = checkDeclaration(t,false,c);
		
		// Finally, process the names
		for (t <- names) c = checkDeclaration(t,true,c);
	}
	
	// TODO: Remove the current module from the environment
	return c;
}

@doc{Get the module name from the header.}
public RName getHeaderName((Header)`<Tags tags> module <QualifiedName qn> <ModuleParameters mps> <Import* imports>`) = convertName(qn);
public RName getHeaderName((Header)`<Tags tags> module <QualifiedName qn> <Import* imports>`) = convertName(qn);

@doc{Get the list of imports from the header.}
public list[Import] getHeaderImports((Header)`<Tags tags> module <QualifiedName qn> <ModuleParameters mps> <Import* imports>`) = [i | i<-imports];
public list[Import] getHeaderImports((Header)`<Tags tags> module <QualifiedName qn> <Import* imports>`) = [i | i<-imports];

public CheckResult convertAndExpandType(Type t, Configuration c) {
	rt = convertType(t);
	if ( (rt@errinfo)? && size(rt@errinfo) > 0) {
		for (m <- rt@errinfo) {
			c = addScopeMessage(c,m);
		}
	}
	return < c, expandType(rt, t@\loc, c) >;
}

public CheckResult convertAndExpandTypeArg(TypeArg t, Configuration c) {
	rt = convertTypeArg(t);
	if ( (rt@errinfo)? && size(rt@errinfo) > 0) {
		for (m <- rt@errinfo) {
			c = addScopeMessage(c,m);
		}
	}
	return < c, expandType(rt, t@\loc, c) >;
}

public CheckResult convertAndExpandUserType(UserType t, Configuration c) {
	rt = convertUserType(t);
	if ( (rt@errinfo)? && size(rt@errinfo) > 0) {
		for (m <- rt@errinfo) {
			c = addScopeMessage(c,m);
		}
	}
	
	// Why don't we just expand the entire type? Because we want to keep this as a user type,
	// allowing us to get access to the type name. We do want to expand the parameters, though,
	// to make sure they have properly marked types (i.e., names are expanded into actual
	// types).
	// TODO: What if we create something like this?
	// alias T[&A <: T] = ...
	// This should probably be identified as something incorrect.
	//
	if (\user(utn,utps) := rt) {
		return < c, \user(utn,[expandType(utpi, t@\loc, c) | utpi <- utps ]) >;
	} else {
		throw "Conversion error: type for user type <t> should be user type, not <prettyPrintType(rt)>";
	}
}

public Symbol expandType(Symbol rt, loc l, Configuration c) {
	rt = bottom-up visit(rt) {
		case \user(rn,pl) : {
			if (rn in c.typeEnv) {
				ut = c.store[c.typeEnv[rn]].rtype;
				if (isAliasType(ut)) {
					atps = getAliasTypeParameters(ut);
					if (size(pl) == size(atps)) {
						failures = { };
						for (idx <- index(pl), !subtype(pl[idx],getTypeVarBound(atps[idx]))) 
							failures = failures + makeFailType("Cannot instantiate parameter <idx> with type <prettyPrintType(pl[idx])>, parameter has bound <prettyPrintType(getTypeVarBound(atps[idx]))>", l);
						if (size(failures) == 0) {
							if (size(pl) > 0) {
								bindings = ( getTypeVarName(atps[idx]) : pl[idx] | idx <- index(pl) );
								insert(instantiate(getAliasedType(ut),bindings));
							} else {
								insert(getAliasedType(ut));
							}
						} else {
							return collapseFailTypes(failures);
						} 
					} else {
						return makeFailType("Alias <prettyPrintName(rn)> declares <size(atps)> type parameters, but given <size(pl)> instantiating types", l);
					}
				} else if (isADTType(ut)) {
					atps = getADTTypeParameters(ut);
					if (size(pl) == size(atps)) {
						failures = { };
						for (idx <- index(pl), !subtype(pl[idx],getTypeVarBound(atps[idx]))) 
							failures = failures + makeFailType("Cannot instantiate parameter <idx> with type <prettyPrintType(pl[idx])>, parameter has bound <prettyPrintType(getTypeVarBound(atps[idx]))>", l);
						if (size(failures) == 0) {
							if (size(pl) > 0) {
								insert(\adt(getADTName(ut),pl));
							} else {
								insert(ut);
							}
						} else {
							return collapseFailTypes(failures);
						} 
					} else {
						return makeFailType("Data type <prettyPrintName(rn)> declares <size(atps)> type parameters, but given <size(pl)> instantiating types", l);
					}
				} else {
					throw "User type should not refer to type <prettyPrintType(ut)>";
				}
			} else {
				return makeFailType("Type <prettyPrintName(rn)> not declared", l);
			}
		}
	}
	return rt;
}

@doc{Check the types of Rascal comprehensions: Set (DONE)}
public CheckResult checkComprehension(Comprehension cmp:(Comprehension)`{ <{Expression ","}+ results> | <{Expression ","}+ generators> }`, Configuration c) {
	set[Symbol] failures = { };
	for (gen <- generators) {
		< c, gt > = checkExp(gen,c);
		if (isFailType(gt)) {
			failures = failures + gt;
		} else if (!isBoolType(gt)) {
			failures = failures + makeFailType("Unexpected type <prettyPrintType(gt)>, generator should be an expression of type bool", gen@\loc);
		}
	}
	list[Symbol] elementTypes = [ \void() ];
	for (res <- results) {
		< c, rt > = checkExp(res,c);
		if (isFailType(rt)) {
			failures = failures + rt;
		} else {
			elementTypes = elementTypes + rt;
		}
	}
	if (size(failures) > 0)
		return markLocationFailed(c, cmp@\loc, failures);
	else
		return markLocationType(c, cmp@\loc, \set(lubList(elementTypes)));
}

@doc{Check the types of Rascal comprehensions: Map (DONE)}
public CheckResult checkComprehension(Comprehension cmp:(Comprehension)`( <Expression from> : <Expression to> | <{Expression ","}+ generators> )`, Configuration c) {
	set[Symbol] failures = { };
	for (gen <- generators) {
		< c, gt > = checkExp(gen,c);
		if (isFailType(gt)) {
			failures = failures + gt;
		} else if (!isBoolType(gt)) {
			failures = failures + makeFailType("Unexpected type <prettyPrintType(gt)>, generator should be an expression of type bool", gen@\loc);
		}
	}

	< c, fromType > = checkExp(from,c);
	if (isFailType(fromType)) failures = failures + fromType;
	< c, toType > = checkExp(to,c);
	if (isFailType(toType)) failures = failures + toType;

	if (size(failures) > 0)
		return markLocationFailed(c, cmp@\loc, failures);
	else
		return markLocationType(c, cmp@\loc, \map(fromType,toType));
}

@doc{Check the types of Rascal comprehensions: List (DONE)}
public CheckResult checkComprehension(Comprehension cmp:(Comprehension)`[ <{Expression ","}+ results> | <{Expression ","}+ generators> ]`, Configuration c) {
	set[Symbol] failures = { };
	for (gen <- generators) {
		< c, gt > = checkExp(gen,c);
		if (isFailType(gt)) {
			failures = failures + gt;
		} else if (!isBoolType(gt)) {
			failures = failures + makeFailType("Unexpected type <prettyPrintType(gt)>, generator should be an expression of type bool", gen@\loc);
		}
	}
	list[Symbol] elementTypes = [ \void() ];
	for (res <- results) {
		< c, rt > = checkExp(res,c);
		if (isFailType(rt)) {
			failures = failures + rt;
		} else {
			elementTypes = elementTypes + rt;
		}
	}
	if (size(failures) > 0)
		return markLocationFailed(c, cmp@\loc, failures);
	else
		return markLocationType(c, cmp@\loc, \list(lubList(elementTypes)));
}

public anno map[loc,str] Tree@docStrings;
public anno map[loc,set[loc]] Tree@docLinks;

public Module check(Module m) {
	c = newConfiguration();
	c = checkModule(m, c);

	//| overload(set[int] items, Symbol rtype)
	//| datatype(RName name, Symbol rtype, int containedIn, set[loc] ats)

	map[loc,set[loc]] docLinks = ( );
	for (<l,i> <- invert(c.uses)) {
		set[loc] toAdd = { };
		if (overload(items,_) := c.store[i]) {
			toAdd = { c.store[itm].at | itm <- items };
		} else if (datatype(_,_,_,ats) := c.store[i]) {
			toAdd = ats;
		} else {
			toAdd = { c.store[i].at };
		}	
		if (l in docLinks) {
			docLinks[l] = docLinks[l] + toAdd;
		} else {
			docLinks[l] = toAdd;
		}
	}
			
	if (size(c.messages) > 0)
		return m[@messages = c.messages]
		        [@docStrings = ( l : "TYPE: <prettyPrintType(c.locationTypes[l])>" | l <- c.locationTypes<0>)]
		        [@docLinks = docLinks];
	else
		return m;
}
