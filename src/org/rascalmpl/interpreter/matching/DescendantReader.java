package org.rascalmpl.interpreter.matching;

import java.util.Iterator;
import java.util.Stack;

import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.IMap;
import org.eclipse.imp.pdb.facts.INode;
import org.eclipse.imp.pdb.facts.ISet;
import org.eclipse.imp.pdb.facts.ITuple;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.type.Type;
import org.rascalmpl.interpreter.types.NonTerminalType;
import org.rascalmpl.interpreter.types.RascalTypeFactory;
import org.rascalmpl.values.uptr.Factory;
import org.rascalmpl.values.uptr.SymbolAdapter;
import org.rascalmpl.values.uptr.TreeAdapter;

public class DescendantReader implements Iterator<IValue> {

	Stack<Object> spine = new Stack<Object>();

	private boolean debug = false;

	// is set to true when the descendant reader is allowed to skip layout nodes and do other optimizations
	private boolean interpretTree;
	
	DescendantReader(IValue val, boolean interpretTree){
		if(debug)System.err.println("DescendantReader: " + val);
		this.interpretTree = interpretTree;
		push(val);
	}

	@SuppressWarnings("unchecked")
	public boolean hasNext() {
		while((spine.size() > 0) &&
			   (spine.peek() instanceof Iterator && !((Iterator<Object>) spine.peek()).hasNext())){
			spine.pop();
		}		
		return spine.size() > 0;
	}
	
	@SuppressWarnings("unchecked")
	public IValue next() {
		if(spine.peek() instanceof Iterator){
			Iterator<Object> iter = (Iterator<Object>) spine.peek();
			if(!iter.hasNext()){
				spine.pop();
				return next();
			}
			push((IValue) iter.next());
			return next();
		}
		return (IValue) spine.pop();
	}
	
	private void push(IValue v, Iterator<IValue> children){
		spine.push(v);
		spine.push(children);
	}
	
	private void push(IValue v){
		Type type = v.getType();
		if(type.isNodeType() || type.isConstructorType() || type.isAbstractDataType()){
			if(interpretTree && (type.isConstructorType() || type.isAbstractDataType()) && type == Factory.Tree){
				pushConcreteSyntaxNode((IConstructor) v);
				return;
			}
			push(v,  ((INode) v).getChildren().iterator());
		} else
		if(type.isListType()){
			push(v, ((IList) v).iterator());
		} else
		if(type.isSetType()){
			push(v, ((ISet) v).iterator());
		} else
		if(type.isMapType()){
			push(v, new MapKeyValueIterator((IMap) v));
		} else
		if(type.isTupleType()){
			push(v, new TupleElementIterator((ITuple) v));
		} else {
			spine.push(v);
		}
	}
	
	private void pushConcreteSyntaxNode(IConstructor tree){
		if(debug)System.err.println("pushConcreteSyntaxNode: " + tree);
		String name = tree.getName();
		
		if(name.equals("sort") || name.equals("lit") || 
		   name.equals("char") || name.equals("single")){
			/*
			 * Don't recurse
			 */
			spine.push(tree);
			return;
		}
		
		if(TreeAdapter.isAmb(tree)) {
			for (IValue alt : TreeAdapter.getAlternatives(tree)) {
				pushConcreteSyntaxNode((IConstructor) alt);
			}
			return;
//			throw new ImplementationError("Cannot handle ambiguous subject");
		}
			
		NonTerminalType ctype = (NonTerminalType) RascalTypeFactory.getInstance().nonTerminalType(tree);
		if(debug)System.err.println("ctype.getSymbol=" + ctype.getSymbol());
		IConstructor sym = ctype.getSymbol();
        if(SymbolAdapter.isAnyList(sym)){
        	sym = SymbolAdapter.getSymbol(sym);
        	
        	int delta = 1;          // distance between "real" list elements, e.g. non-layout and non-separator
        	IList listElems = (IList) tree.get(1);
			if(SymbolAdapter.isIterPlus(sym) || SymbolAdapter.isIterStar(sym)){
				if(debug)System.err.println("pushConcreteSyntaxChildren: isIterPlus or isIterStar");
				delta = 1; // new iters never have layout separators
			} else if (SymbolAdapter.isIterPlusSeps(sym) || SymbolAdapter.isIterStarSeps(sym)) {
				if(debug)System.err.println("pushConcreteSyntaxChildren: isIterPlusSeps or isIterStarSeps");
				delta = SymbolAdapter.getSeparators(sym).length() + 1;
			}
			if(debug)
				for(int i = 0; i < listElems.length(); i++){
					System.err.println("#" + i + ": " + listElems.get(i));
				}
        	
			for(int i = listElems.length() - 1; i >= 0 ; i -= delta){
				if(debug)System.err.println("adding: " + listElems.get(i));
				pushConcreteSyntaxNode((IConstructor)listElems.get(i));
			}
		} else {
			if(debug)System.err.println("pushConcreteSyntaxNode: appl");
			/*
			 * appl(prod(...), [child0, layout0, child1, ...])
			 */
			spine.push(tree);
			IList applArgs = (IList) tree.get(1);
			int delta = (SymbolAdapter.isLiteral(sym)) ? 1 : 2;   // distance between elements
			
			for(int i = applArgs.length() - 1; i >= 0 ; i -= delta){
				//spine.push(applArgs.get(i));
				pushConcreteSyntaxNode((IConstructor) applArgs.get(i));
			}
		}
	}

	public void remove() {
		throw new UnsupportedOperationException("remove from DescendantReader");
	}
}
