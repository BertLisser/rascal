package org.rascalmpl.interpreter;

import static org.rascalmpl.interpreter.result.ResultFactory.bool;
import static org.rascalmpl.interpreter.result.ResultFactory.makeResult;
import static org.rascalmpl.interpreter.result.ResultFactory.nothing;
import static org.rascalmpl.interpreter.utils.Utils.unescape;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Stack;
import java.util.Map.Entry;

import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IInteger;
import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.IListWriter;
import org.eclipse.imp.pdb.facts.IMap;
import org.eclipse.imp.pdb.facts.IMapWriter;
import org.eclipse.imp.pdb.facts.ISet;
import org.eclipse.imp.pdb.facts.ISetWriter;
import org.eclipse.imp.pdb.facts.ISourceLocation;
import org.eclipse.imp.pdb.facts.IString;
import org.eclipse.imp.pdb.facts.ITuple;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.IWriter;
import org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException;
import org.eclipse.imp.pdb.facts.exceptions.UndeclaredFieldException;
import org.eclipse.imp.pdb.facts.io.PBFReader;
import org.eclipse.imp.pdb.facts.io.PBFWriter;
import org.eclipse.imp.pdb.facts.type.Type;
import org.eclipse.imp.pdb.facts.type.TypeFactory;
import org.eclipse.imp.pdb.facts.type.TypeStore;
import org.joda.time.DateTime;
import org.joda.time.format.ISODateTimeFormat;
import org.rascalmpl.ast.ASTFactory;
import org.rascalmpl.ast.ASTFactoryFactory;
import org.rascalmpl.ast.AbstractAST;
import org.rascalmpl.ast.BasicType;
import org.rascalmpl.ast.Bound;
import org.rascalmpl.ast.Case;
import org.rascalmpl.ast.Catch;
import org.rascalmpl.ast.Command;
import org.rascalmpl.ast.Declaration;
import org.rascalmpl.ast.Expression;
import org.rascalmpl.ast.Field;
import org.rascalmpl.ast.FunctionDeclaration;
import org.rascalmpl.ast.FunctionModifier;
import org.rascalmpl.ast.Import;
import org.rascalmpl.ast.Label;
import org.rascalmpl.ast.Module;
import org.rascalmpl.ast.Name;
import org.rascalmpl.ast.NullASTVisitor;
import org.rascalmpl.ast.QualifiedName;
import org.rascalmpl.ast.ShellCommand;
import org.rascalmpl.ast.Statement;
import org.rascalmpl.ast.Strategy;
import org.rascalmpl.ast.StringConstant;
import org.rascalmpl.ast.StringLiteral;
import org.rascalmpl.ast.Toplevel;
import org.rascalmpl.ast.Assignable.Constructor;
import org.rascalmpl.ast.Assignable.FieldAccess;
import org.rascalmpl.ast.Command.Shell;
import org.rascalmpl.ast.DateTimeLiteral.DateAndTimeLiteral;
import org.rascalmpl.ast.DateTimeLiteral.DateLiteral;
import org.rascalmpl.ast.DateTimeLiteral.TimeLiteral;
import org.rascalmpl.ast.Declaration.Alias;
import org.rascalmpl.ast.Declaration.Annotation;
import org.rascalmpl.ast.Declaration.Data;
import org.rascalmpl.ast.Declaration.DataAbstract;
import org.rascalmpl.ast.Declaration.Function;
import org.rascalmpl.ast.Declaration.Rule;
import org.rascalmpl.ast.Declaration.Tag;
import org.rascalmpl.ast.Declaration.Test;
import org.rascalmpl.ast.Declaration.Variable;
import org.rascalmpl.ast.Declaration.View;
import org.rascalmpl.ast.Expression.Addition;
import org.rascalmpl.ast.Expression.All;
import org.rascalmpl.ast.Expression.Ambiguity;
import org.rascalmpl.ast.Expression.And;
import org.rascalmpl.ast.Expression.Anti;
import org.rascalmpl.ast.Expression.Any;
import org.rascalmpl.ast.Expression.Bracket;
import org.rascalmpl.ast.Expression.CallOrTree;
import org.rascalmpl.ast.Expression.Closure;
import org.rascalmpl.ast.Expression.Composition;
import org.rascalmpl.ast.Expression.Comprehension;
import org.rascalmpl.ast.Expression.Descendant;
import org.rascalmpl.ast.Expression.Division;
import org.rascalmpl.ast.Expression.Equivalence;
import org.rascalmpl.ast.Expression.FieldProject;
import org.rascalmpl.ast.Expression.FieldUpdate;
import org.rascalmpl.ast.Expression.GreaterThan;
import org.rascalmpl.ast.Expression.GreaterThanOrEq;
import org.rascalmpl.ast.Expression.Guarded;
import org.rascalmpl.ast.Expression.IfDefinedOtherwise;
import org.rascalmpl.ast.Expression.Implication;
import org.rascalmpl.ast.Expression.In;
import org.rascalmpl.ast.Expression.Intersection;
import org.rascalmpl.ast.Expression.IsDefined;
import org.rascalmpl.ast.Expression.It;
import org.rascalmpl.ast.Expression.Join;
import org.rascalmpl.ast.Expression.LessThan;
import org.rascalmpl.ast.Expression.LessThanOrEq;
import org.rascalmpl.ast.Expression.Lexical;
import org.rascalmpl.ast.Expression.List;
import org.rascalmpl.ast.Expression.Literal;
import org.rascalmpl.ast.Expression.Match;
import org.rascalmpl.ast.Expression.Modulo;
import org.rascalmpl.ast.Expression.MultiVariable;
import org.rascalmpl.ast.Expression.Negation;
import org.rascalmpl.ast.Expression.Negative;
import org.rascalmpl.ast.Expression.NoMatch;
import org.rascalmpl.ast.Expression.NonEmptyBlock;
import org.rascalmpl.ast.Expression.NotIn;
import org.rascalmpl.ast.Expression.Or;
import org.rascalmpl.ast.Expression.Product;
import org.rascalmpl.ast.Expression.Range;
import org.rascalmpl.ast.Expression.Reducer;
import org.rascalmpl.ast.Expression.ReifiedType;
import org.rascalmpl.ast.Expression.ReifyType;
import org.rascalmpl.ast.Expression.Set;
import org.rascalmpl.ast.Expression.StepRange;
import org.rascalmpl.ast.Expression.Subscript;
import org.rascalmpl.ast.Expression.Subtraction;
import org.rascalmpl.ast.Expression.TransitiveClosure;
import org.rascalmpl.ast.Expression.TransitiveReflexiveClosure;
import org.rascalmpl.ast.Expression.Tuple;
import org.rascalmpl.ast.Expression.TypedVariable;
import org.rascalmpl.ast.Expression.TypedVariableBecomes;
import org.rascalmpl.ast.Expression.VariableBecomes;
import org.rascalmpl.ast.Expression.VoidClosure;
import org.rascalmpl.ast.FunctionDeclaration.Abstract;
import org.rascalmpl.ast.Header.Parameters;
import org.rascalmpl.ast.Import.Syntax;
import org.rascalmpl.ast.IntegerLiteral.DecimalIntegerLiteral;
import org.rascalmpl.ast.IntegerLiteral.HexIntegerLiteral;
import org.rascalmpl.ast.IntegerLiteral.OctalIntegerLiteral;
import org.rascalmpl.ast.Literal.Boolean;
import org.rascalmpl.ast.Literal.Integer;
import org.rascalmpl.ast.Literal.Location;
import org.rascalmpl.ast.Literal.Real;
import org.rascalmpl.ast.Literal.RegExp;
import org.rascalmpl.ast.LocalVariableDeclaration.Default;
import org.rascalmpl.ast.PathTail.Mid;
import org.rascalmpl.ast.PatternWithAction.Arbitrary;
import org.rascalmpl.ast.PatternWithAction.Replacing;
import org.rascalmpl.ast.ProtocolPart.Interpolated;
import org.rascalmpl.ast.ProtocolPart.NonInterpolated;
import org.rascalmpl.ast.ProtocolTail.Post;
import org.rascalmpl.ast.ShellCommand.Edit;
import org.rascalmpl.ast.ShellCommand.Help;
import org.rascalmpl.ast.ShellCommand.ListDeclarations;
import org.rascalmpl.ast.ShellCommand.Quit;
import org.rascalmpl.ast.ShellCommand.Unimport;
import org.rascalmpl.ast.Statement.Append;
import org.rascalmpl.ast.Statement.Assert;
import org.rascalmpl.ast.Statement.AssertWithMessage;
import org.rascalmpl.ast.Statement.Assignment;
import org.rascalmpl.ast.Statement.Break;
import org.rascalmpl.ast.Statement.Continue;
import org.rascalmpl.ast.Statement.DoWhile;
import org.rascalmpl.ast.Statement.EmptyStatement;
import org.rascalmpl.ast.Statement.Fail;
import org.rascalmpl.ast.Statement.For;
import org.rascalmpl.ast.Statement.GlobalDirective;
import org.rascalmpl.ast.Statement.IfThen;
import org.rascalmpl.ast.Statement.IfThenElse;
import org.rascalmpl.ast.Statement.Insert;
import org.rascalmpl.ast.Statement.Solve;
import org.rascalmpl.ast.Statement.Switch;
import org.rascalmpl.ast.Statement.Throw;
import org.rascalmpl.ast.Statement.Try;
import org.rascalmpl.ast.Statement.TryFinally;
import org.rascalmpl.ast.Statement.VariableDeclaration;
import org.rascalmpl.ast.Statement.While;
import org.rascalmpl.ast.Test.Labeled;
import org.rascalmpl.ast.Test.Unlabeled;
import org.rascalmpl.ast.Toplevel.GivenVisibility;
import org.rascalmpl.ast.Visit.DefaultStrategy;
import org.rascalmpl.ast.Visit.GivenStrategy;
import org.rascalmpl.interpreter.TraversalEvaluator.DIRECTION;
import org.rascalmpl.interpreter.TraversalEvaluator.FIXEDPOINT;
import org.rascalmpl.interpreter.TraversalEvaluator.PROGRESS;
import org.rascalmpl.interpreter.asserts.Ambiguous;
import org.rascalmpl.interpreter.asserts.ImplementationError;
import org.rascalmpl.interpreter.asserts.NotYetImplemented;
import org.rascalmpl.interpreter.control_exceptions.Failure;
import org.rascalmpl.interpreter.control_exceptions.InterruptException;
import org.rascalmpl.interpreter.control_exceptions.QuitException;
import org.rascalmpl.interpreter.control_exceptions.Return;
import org.rascalmpl.interpreter.env.Environment;
import org.rascalmpl.interpreter.env.GlobalEnvironment;
import org.rascalmpl.interpreter.env.ModuleEnvironment;
import org.rascalmpl.interpreter.env.RewriteRule;
import org.rascalmpl.interpreter.load.FromDefinedSdfSearchPathPathContributor;
import org.rascalmpl.interpreter.load.IRascalSearchPathContributor;
import org.rascalmpl.interpreter.load.ISdfSearchPathContributor;
import org.rascalmpl.interpreter.load.RascalURIResolver;
import org.rascalmpl.interpreter.load.SDFSearchPath;
import org.rascalmpl.interpreter.matching.IBooleanResult;
import org.rascalmpl.interpreter.matching.IMatchingResult;
import org.rascalmpl.interpreter.matching.NodePattern;
import org.rascalmpl.interpreter.result.AbstractFunction;
import org.rascalmpl.interpreter.result.BoolResult;
import org.rascalmpl.interpreter.result.JavaMethod;
import org.rascalmpl.interpreter.result.OverloadedFunctionResult;
import org.rascalmpl.interpreter.result.RascalFunction;
import org.rascalmpl.interpreter.result.Result;
import org.rascalmpl.interpreter.result.ResultFactory;
import org.rascalmpl.interpreter.staticErrors.AppendWithoutLoop;
import org.rascalmpl.interpreter.staticErrors.DateTimeParseError;
import org.rascalmpl.interpreter.staticErrors.ItOutsideOfReducer;
import org.rascalmpl.interpreter.staticErrors.JavaMethodLinkError;
import org.rascalmpl.interpreter.staticErrors.MissingModifierError;
import org.rascalmpl.interpreter.staticErrors.ModuleLoadError;
import org.rascalmpl.interpreter.staticErrors.ModuleNameMismatchError;
import org.rascalmpl.interpreter.staticErrors.NonVoidTypeRequired;
import org.rascalmpl.interpreter.staticErrors.RedeclaredVariableError;
import org.rascalmpl.interpreter.staticErrors.StaticError;
import org.rascalmpl.interpreter.staticErrors.SyntaxError;
import org.rascalmpl.interpreter.staticErrors.UndeclaredAnnotationError;
import org.rascalmpl.interpreter.staticErrors.UndeclaredFieldError;
import org.rascalmpl.interpreter.staticErrors.UndeclaredModuleError;
import org.rascalmpl.interpreter.staticErrors.UndeclaredVariableError;
import org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError;
import org.rascalmpl.interpreter.staticErrors.UnguardedFailError;
import org.rascalmpl.interpreter.staticErrors.UnguardedInsertError;
import org.rascalmpl.interpreter.staticErrors.UnguardedReturnError;
import org.rascalmpl.interpreter.staticErrors.UninitializedVariableError;
import org.rascalmpl.interpreter.staticErrors.UnsupportedOperationError;
import org.rascalmpl.interpreter.strategy.IStrategyContext;
import org.rascalmpl.interpreter.strategy.StrategyContextStack;
import org.rascalmpl.interpreter.types.FunctionType;
import org.rascalmpl.interpreter.types.NonTerminalType;
import org.rascalmpl.interpreter.types.RascalTypeFactory;
import org.rascalmpl.interpreter.utils.JavaBridge;
import org.rascalmpl.interpreter.utils.Names;
import org.rascalmpl.interpreter.utils.Profiler;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.interpreter.utils.Utils;
import org.rascalmpl.library.rascal.syntax.RascalRascal;
import org.rascalmpl.parser.ASTBuilder;
import org.rascalmpl.parser.ActionExecutor;
import org.rascalmpl.parser.IParserInfo;
import org.rascalmpl.parser.IRascalParser;
import org.rascalmpl.parser.LegacyRascalParser;
import org.rascalmpl.parser.NewRascalParser;
import org.rascalmpl.parser.ParserGenerator;
import org.rascalmpl.parser.sgll.IGLL;
import org.rascalmpl.uri.CWDURIResolver;
import org.rascalmpl.uri.ClassResourceInputStreamResolver;
import org.rascalmpl.uri.FileURIResolver;
import org.rascalmpl.uri.HttpURIResolver;
import org.rascalmpl.uri.URIResolverRegistry;
import org.rascalmpl.values.ValueFactoryFactory;
import org.rascalmpl.values.errors.SubjectAdapter;
import org.rascalmpl.values.errors.SummaryAdapter;
import org.rascalmpl.values.uptr.Factory;
import org.rascalmpl.values.uptr.ParsetreeAdapter;
import org.rascalmpl.values.uptr.ProductionAdapter;
import org.rascalmpl.values.uptr.SymbolAdapter;
import org.rascalmpl.values.uptr.TreeAdapter;

public class Evaluator extends org.rascalmpl.ast.NullASTVisitor<org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue>> implements org.rascalmpl.interpreter.IEvaluator<org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue>> {
	private org.eclipse.imp.pdb.facts.IValueFactory vf;
	private static final org.eclipse.imp.pdb.facts.type.TypeFactory tf = org.eclipse.imp.pdb.facts.type.TypeFactory.getInstance();
	protected org.rascalmpl.interpreter.env.Environment currentEnvt;
	private org.rascalmpl.interpreter.strategy.StrategyContextStack strategyContextStack;

	private final org.rascalmpl.interpreter.env.GlobalEnvironment heap;
	private boolean interrupt = false;

	private final org.rascalmpl.interpreter.utils.JavaBridge javaBridge;

	private org.rascalmpl.ast.AbstractAST currentAST; 	// used in runtime errormessages

	private static boolean doProfiling = false;
	private org.rascalmpl.interpreter.utils.Profiler profiler;
	
	private boolean saveParsedModules = false;

	private final org.rascalmpl.interpreter.TypeDeclarationEvaluator typeDeclarator;
	private org.rascalmpl.interpreter.IEvaluator<org.rascalmpl.interpreter.matching.IMatchingResult> patternEvaluator;

	private final java.util.List<java.lang.ClassLoader> classLoaders;
	private final org.rascalmpl.interpreter.env.ModuleEnvironment rootScope;
	private boolean concreteListsShouldBeSpliced;
	private final org.rascalmpl.parser.IRascalParser parser;

	private java.io.PrintWriter stderr;
	private java.io.PrintWriter stdout;

	private org.rascalmpl.interpreter.ITestResultListener testReporter;
	private java.util.Stack<org.rascalmpl.interpreter.Accumulator> accumulators = new java.util.Stack<org.rascalmpl.interpreter.Accumulator>();
	private final org.rascalmpl.interpreter.load.RascalURIResolver resolver;
	private final org.rascalmpl.interpreter.load.SDFSearchPath sdf;
	private final org.rascalmpl.parser.ASTBuilder builder;
	
	private final org.rascalmpl.uri.URIResolverRegistry resolverRegistry;

	public Evaluator(org.eclipse.imp.pdb.facts.IValueFactory f, java.io.PrintWriter stderr, java.io.PrintWriter stdout, org.rascalmpl.parser.IRascalParser parser, org.rascalmpl.interpreter.env.ModuleEnvironment scope, org.rascalmpl.interpreter.env.GlobalEnvironment heap) {
		this.__setVf(f);
		this.__setPatternEvaluator(new org.rascalmpl.interpreter.PatternEvaluator(this));
		this.strategyContextStack = new org.rascalmpl.interpreter.strategy.StrategyContextStack();
		this.heap = heap;
		this.typeDeclarator = new org.rascalmpl.interpreter.TypeDeclarationEvaluator(this);
		this.currentEnvt = scope;
		this.rootScope = scope;
		this.__getHeap().addModule(scope);
		this.classLoaders = new java.util.ArrayList<java.lang.ClassLoader>();
		this.javaBridge = new org.rascalmpl.interpreter.utils.JavaBridge(stderr, this.classLoaders, this.__getVf());
		this.resolver = new org.rascalmpl.interpreter.load.RascalURIResolver(this);
		this.sdf = new org.rascalmpl.interpreter.load.SDFSearchPath();
		this.parser = parser;
		this.stderr = stderr;
		this.__setStdout(stdout);
		this.builder = new org.rascalmpl.parser.ASTBuilder(ASTFactoryFactory.getASTFactory());
		this.resolverRegistry = new org.rascalmpl.uri.URIResolverRegistry();

		this.updateProperties();
		
		if (stderr == null) {
			throw new java.lang.NullPointerException();
		}
		if (stdout == null) {
			throw new java.lang.NullPointerException();
		}

		this.resolver.addPathContributor(new org.rascalmpl.interpreter.load.IRascalSearchPathContributor() {
			public void contributePaths(java.util.List<java.net.URI> l) {
				l.add(java.net.URI.create("cwd:///"));
				l.add(java.net.URI.create("stdlib:///org/rascalmpl/library"));
				l.add(java.net.URI.create("stdlib:///"));
				l.add(java.net.URI.create("stdlib:///org/rascalmpl/test/data"));

				java.lang.String property = java.lang.System.getProperty("rascal.path");

				if (property != null) {
					for (java.lang.String path : property.split(":")) {
						l.add(java.net.URI.create("file://" + path));
					}
				}
			}
			@Override
			public java.lang.String toString() {
				return "[current wd and stdlib]";
			}
		});

		// add current wd and sdf-library to search path for SDF modules
		this.__getSdf().addSdfSearchPathContributor(new org.rascalmpl.interpreter.load.ISdfSearchPathContributor() {
			public java.util.List<java.lang.String> contributePaths() {
				java.util.List<java.lang.String> result = new java.util.ArrayList<java.lang.String>();

				result.add(new java.io.File("/Users/jurgenv/Sources/Rascal/rascal/src/org/rascalmpl/library").getAbsolutePath());
				result.add(new java.io.File("/Users/mhills/Projects/rascal/build/rascal/src/org/rascalmpl/library").getAbsolutePath());
				result.add(new java.io.File("/Users/paulklint/software/source/roll/rascal/src/org/rascalmpl/library").getAbsolutePath());
				result.add(org.rascalmpl.interpreter.Configuration.getSdfLibraryPathProperty());
				result.add(new java.io.File(java.lang.System.getProperty("user.dir"), "src/org/rascalmpl/test/data").getAbsolutePath());
				// adds folders for Rascal grammar
				result.add(new java.io.File("/ufs/hills/project/rascal/build/rascal-grammar/spec").getAbsolutePath());
				return result;
			}
		});

		// adds folders using -Drascal.sdf.path=/colon-separated/path
		this.__getSdf().addSdfSearchPathContributor(new org.rascalmpl.interpreter.load.FromDefinedSdfSearchPathPathContributor());

		// load Java classes from the current jar (for the standard library)
		this.classLoaders.add(this.getClass().getClassLoader());

		// register some schemes
		org.rascalmpl.uri.FileURIResolver files = new org.rascalmpl.uri.FileURIResolver(); 
		this.resolverRegistry.registerInput(files.scheme(), files);
		this.resolverRegistry.registerOutput(files.scheme(), files);

		org.rascalmpl.uri.HttpURIResolver http = new org.rascalmpl.uri.HttpURIResolver();
		this.resolverRegistry.registerInput(http.scheme(), http);
		
		org.rascalmpl.uri.CWDURIResolver cwd = new org.rascalmpl.uri.CWDURIResolver();
		this.resolverRegistry.registerInput(cwd.scheme(), cwd);
		this.resolverRegistry.registerOutput(cwd.scheme(), cwd);
		
		org.rascalmpl.uri.ClassResourceInputStreamResolver library = new org.rascalmpl.uri.ClassResourceInputStreamResolver("stdlib", this.getClass());
		this.resolverRegistry.registerInput(library.scheme(), library);
		
		// Allow writing via stdlib scheme
		org.rascalmpl.uri.FileURIResolver stdlib = new org.rascalmpl.uri.FileURIResolver(); 
		this.resolverRegistry.registerOutput(library.scheme(), stdlib);

		this.resolverRegistry.registerInput(this.resolver.scheme(), this.resolver);
		this.resolverRegistry.registerOutput(this.resolver.scheme(), this.resolver);
	}  
	
	public org.rascalmpl.parser.IRascalParser __getParser() {
		return parser;
	}

	public void __setStdout(java.io.PrintWriter stdout) {
		this.stdout = stdout;
	}

	public java.io.PrintWriter __getStdout() {
		return stdout;
	}

	public org.rascalmpl.interpreter.TypeDeclarationEvaluator __getTypeDeclarator() {
		return typeDeclarator;
	}

	public void __setConcreteListsShouldBeSpliced(
			boolean concreteListsShouldBeSpliced) {
		this.concreteListsShouldBeSpliced = concreteListsShouldBeSpliced;
	}

	public boolean __getConcreteListsShouldBeSpliced() {
		return concreteListsShouldBeSpliced;
	}

	public void __setAccumulators(java.util.Stack<org.rascalmpl.interpreter.Accumulator> accumulators) {
		this.accumulators = accumulators;
	}

	public java.util.Stack<org.rascalmpl.interpreter.Accumulator> __getAccumulators() {
		return accumulators;
	}

	public void __setInterrupt(boolean interrupt) {
		this.interrupt = interrupt;
	}

	public boolean __getInterrupt() {
		return interrupt;
	}

	public org.rascalmpl.interpreter.utils.JavaBridge __getJavaBridge() {
		return javaBridge;
	}

	public static org.eclipse.imp.pdb.facts.type.TypeFactory __getTf() {
		return tf;
	}

	public org.rascalmpl.interpreter.env.ModuleEnvironment __getRootScope() {
		return rootScope;
	}

	public org.rascalmpl.interpreter.env.GlobalEnvironment __getHeap() {
		return heap;
	}

	public void __setPatternEvaluator(org.rascalmpl.interpreter.IEvaluator<org.rascalmpl.interpreter.matching.IMatchingResult> patternEvaluator) {
		this.patternEvaluator = patternEvaluator;
	}

	public org.rascalmpl.interpreter.IEvaluator<org.rascalmpl.interpreter.matching.IMatchingResult> __getPatternEvaluator() {
		return patternEvaluator;
	}

	public void __setVf(org.eclipse.imp.pdb.facts.IValueFactory vf) {
		this.vf = vf;
	}

	public org.eclipse.imp.pdb.facts.IValueFactory __getVf() {
		return vf;
	}

	public org.rascalmpl.interpreter.load.SDFSearchPath __getSdf() {
		return sdf;
	}

	public void interrupt() {
		this.__setInterrupt(true);
	}
	
	public boolean isInterrupted() {
		return this.__getInterrupt();
	}
	
	public java.io.PrintWriter getStdOut() {
		return this.__getStdout();
	}
	
	public java.io.PrintWriter getStdErr() {
		return this.stderr;
	}
	
	public void setTestResultListener(org.rascalmpl.interpreter.ITestResultListener l) {
		this.testReporter = l;
	}
	
	public org.rascalmpl.interpreter.load.SDFSearchPath getSDFSearchPath() {
		return this.__getSdf();
	}
	
	public org.rascalmpl.interpreter.utils.JavaBridge getJavaBridge(){
		return this.__getJavaBridge();
	}

	public org.rascalmpl.uri.URIResolverRegistry getResolverRegistry() {
		return this.resolverRegistry;
	}
	
	public org.rascalmpl.interpreter.load.RascalURIResolver getRascalResolver() {
		return this.resolver;
	}
	
	public org.eclipse.imp.pdb.facts.IValue call(java.lang.String name, org.eclipse.imp.pdb.facts.IValue...args) {
		org.rascalmpl.ast.QualifiedName qualifiedName = org.rascalmpl.interpreter.utils.Names.toQualifiedName(name);
		org.rascalmpl.interpreter.result.OverloadedFunctionResult func = (org.rascalmpl.interpreter.result.OverloadedFunctionResult) this.getCurrentEnvt().getVariable(qualifiedName);
		
		
		org.eclipse.imp.pdb.facts.type.Type[] types = new org.eclipse.imp.pdb.facts.type.Type[args.length];
		
		if (func == null) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("Function " + name + " is unknown");
		}
		
		int i = 0;
		for (org.eclipse.imp.pdb.facts.IValue v : args) {
			types[i++] = v.getType();
		}
		
		return func.call(types, args).getValue();
	}
	
	/**
	 * Parse an object string using the imported SDF modules from the current context.
	 */
	public org.eclipse.imp.pdb.facts.IConstructor parseObject(org.eclipse.imp.pdb.facts.IConstructor startSort, java.net.URI input) {
		try {
			return this.filterStart(startSort, this.__getParser().parseStream(this.__getSdf().getSdfSearchPath(), ((org.rascalmpl.interpreter.env.ModuleEnvironment) this.getCurrentEnvt().getRoot()).getSDFImports(), this.resolverRegistry.getInputStream(input)));
		} catch (java.io.IOException e) {
			throw org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.io(this.__getVf().string(e.getMessage()), this.getCurrentAST(), this.getStackTrace());
		} catch (org.rascalmpl.interpreter.staticErrors.SyntaxError e) {
			throw org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.parseError(e.getLocation(), this.getCurrentAST(), this.getStackTrace());
		}
	}
	
	/**
	 * Parse an object string using the imported SDF modules from the current context.
	 */
	public org.eclipse.imp.pdb.facts.IConstructor parseObject(org.eclipse.imp.pdb.facts.IConstructor startSort, java.lang.String input) {
		try {
			return this.filterStart(startSort, this.__getParser().parseString(this.__getSdf().getSdfSearchPath(), ((org.rascalmpl.interpreter.env.ModuleEnvironment) this.getCurrentEnvt().getRoot()).getSDFImports(), input));
		} catch (java.io.IOException e) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("unexpected io exception", e);
		} catch (org.rascalmpl.interpreter.staticErrors.SyntaxError e) {
			throw org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.parseError(e.getLocation(), this.getCurrentAST(), this.getStackTrace());
		}
	}
	
	public org.eclipse.imp.pdb.facts.IValue parseObjectExperimental(org.eclipse.imp.pdb.facts.IConstructor startSort, java.net.URI inputURI) throws IOException{
		System.err.println("Generating a parser");
		org.rascalmpl.parser.sgll.IGLL parser = this.getObjectParser();
		java.lang.String name = "";
		if (org.rascalmpl.values.uptr.SymbolAdapter.isCf(startSort)) {
			startSort = org.rascalmpl.values.uptr.SymbolAdapter.getSymbol(startSort);
		}
		if (org.rascalmpl.values.uptr.SymbolAdapter.isStart(startSort)) {
			name = "start__";
			startSort = org.rascalmpl.values.uptr.SymbolAdapter.getStart(startSort);
		}
		if (org.rascalmpl.values.uptr.SymbolAdapter.isSort(startSort)) {
			name += org.rascalmpl.values.uptr.SymbolAdapter.getName(startSort);
		}
		System.err.println("Calling the parser");
		org.eclipse.imp.pdb.facts.IConstructor forest = parser.parse(name, inputURI, this.resolver.getInputStream(inputURI));
		
		System.err.println("Executing actions");
		org.rascalmpl.parser.ActionExecutor exec = new org.rascalmpl.parser.ActionExecutor(this, (org.rascalmpl.parser.IParserInfo) parser);
		return exec.execute(forest);
	}
	
	public org.eclipse.imp.pdb.facts.IValue parseObjectExperimental(org.eclipse.imp.pdb.facts.IConstructor startSort, java.net.URI inputURI, java.lang.String sentence) {
		System.err.println("Generating a parser");
		org.rascalmpl.parser.sgll.IGLL parser = this.getObjectParser();
		java.lang.String name = "";
		if (org.rascalmpl.values.uptr.SymbolAdapter.isCf(startSort)) {
			startSort = org.rascalmpl.values.uptr.SymbolAdapter.getSymbol(startSort);
		}
		if (org.rascalmpl.values.uptr.SymbolAdapter.isStart(startSort)) {
			name = "start__";
			startSort = org.rascalmpl.values.uptr.SymbolAdapter.getStart(startSort);
		}
		if (org.rascalmpl.values.uptr.SymbolAdapter.isSort(startSort)) {
			name += org.rascalmpl.values.uptr.SymbolAdapter.getName(startSort);
		}
		System.err.println("Calling the parser");
		org.eclipse.imp.pdb.facts.IConstructor forest = parser.parse(name, inputURI, sentence);
		
		System.err.println("Executing actions");
		org.rascalmpl.parser.ActionExecutor exec = new org.rascalmpl.parser.ActionExecutor(this, (org.rascalmpl.parser.IParserInfo) parser);
		return exec.execute(forest);
	}

	private org.rascalmpl.parser.sgll.IGLL getObjectParser() {
		org.rascalmpl.parser.ParserGenerator pg = this.getParserGenerator();
		org.rascalmpl.interpreter.env.ModuleEnvironment currentModule = (org.rascalmpl.interpreter.env.ModuleEnvironment) this.getCurrentEnvt().getRoot();
		java.lang.Class<org.rascalmpl.parser.sgll.IGLL> parser = currentModule.getParser();
		
		if (parser == null) {
			java.lang.String parserName;
			if (this.__getRootScope() == currentModule) {
				parserName = "__Shell__";
			}
			else {
				parserName = currentModule.getName().replaceAll("::", ".");
			}

			parser = pg.getParser(this.getCurrentAST().getLocation(), parserName, currentModule.getProductions());
			currentModule.safeParser(parser);
		}
		
		try {
			return parser.newInstance();
		} catch (java.lang.InstantiationException e) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError(e.getMessage(), e);
		} catch (java.lang.IllegalAccessException e) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError(e.getMessage(), e);
		}
	}
	
	// for debugging/bootstrapping purposes TODO remove later
	public org.eclipse.imp.pdb.facts.IValue getGrammar(java.lang.String modname) {
		org.rascalmpl.interpreter.env.ModuleEnvironment env = this.getHeap().getModule(modname);
		if (env == null) {
			throw new org.rascalmpl.interpreter.staticErrors.UndeclaredModuleError(modname, this.getCurrentAST());
		}
		org.rascalmpl.parser.ParserGenerator pg = this.getParserGenerator();
		return pg.getGrammar(env.getProductions());
	}
	
	private org.rascalmpl.parser.ParserGenerator getParserGenerator() {
		if (this.parserGenerator == null) {
			this.parserGenerator = new org.rascalmpl.parser.ParserGenerator(this.__getStdout(), this.classLoaders, this.getValueFactory());
		}
		return this.parserGenerator;
	}

	private org.eclipse.imp.pdb.facts.IConstructor filterStart(org.eclipse.imp.pdb.facts.IConstructor startSort, org.eclipse.imp.pdb.facts.IConstructor ptree) {
		org.eclipse.imp.pdb.facts.IConstructor top = org.rascalmpl.values.uptr.ParsetreeAdapter.getTop(ptree);
		org.eclipse.imp.pdb.facts.IList args = org.rascalmpl.values.uptr.TreeAdapter.getArgs(top);
		org.eclipse.imp.pdb.facts.IConstructor tree = (org.eclipse.imp.pdb.facts.IConstructor) args.get(1);

		if (org.rascalmpl.values.uptr.TreeAdapter.isAppl(tree)) {
			org.eclipse.imp.pdb.facts.IConstructor prod = org.rascalmpl.values.uptr.TreeAdapter.getProduction(tree);
			org.eclipse.imp.pdb.facts.IConstructor rhs = org.rascalmpl.values.uptr.ProductionAdapter.getRhs(prod);

			if (!rhs.isEqual(startSort)) {
				throw org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.parseError(org.rascalmpl.values.uptr.TreeAdapter.getLocation(tree), null, null);
			}

			return ptree;
		}
		else if (org.rascalmpl.values.uptr.TreeAdapter.isAmb(tree)) {
			for (org.eclipse.imp.pdb.facts.IValue alt : org.rascalmpl.values.uptr.TreeAdapter.getAlternatives(tree)) {
				org.eclipse.imp.pdb.facts.IConstructor prod = org.rascalmpl.values.uptr.TreeAdapter.getProduction((org.eclipse.imp.pdb.facts.IConstructor) alt);
				org.eclipse.imp.pdb.facts.IConstructor rhs = org.rascalmpl.values.uptr.ProductionAdapter.getRhs(prod);

				if (rhs.isEqual(startSort)) {
					return ptree.set("top", top.set("args", args.put(1, alt)));
				}
			}

			throw org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.parseError(org.rascalmpl.values.uptr.TreeAdapter.getLocation(tree), null, null);
		}
		
		throw new org.rascalmpl.interpreter.asserts.ImplementationError("unexpected tree type: " + tree.getType());
	}

	private void checkPoint(org.rascalmpl.interpreter.env.Environment env) {
		env.checkPoint();
	}

	private void rollback(org.rascalmpl.interpreter.env.Environment env) {
		env.rollback();
	}

	private void commit(org.rascalmpl.interpreter.env.Environment env) {
		env.commit();
	}

	public void setCurrentAST(org.rascalmpl.ast.AbstractAST currentAST) {
		this.currentAST = currentAST;
	}

	public org.rascalmpl.ast.AbstractAST getCurrentAST() {
		return this.currentAST;
	}

	public void addRascalSearchPathContributor(org.rascalmpl.interpreter.load.IRascalSearchPathContributor contrib) {
		this.resolver.addPathContributor(contrib);
	}
	
	public void addRascalSearchPath(final java.net.URI uri) {
		this.resolver.addPathContributor(new org.rascalmpl.interpreter.load.IRascalSearchPathContributor() {
			public void contributePaths(java.util.List<java.net.URI> path) {
				path.add(0, uri);
			}
			
			@Override
			public java.lang.String toString() {
				return uri.toString();
			}
		});
	}
	
	public void addSdfSearchPathContributor(org.rascalmpl.interpreter.load.ISdfSearchPathContributor contrib) {
		this.__getSdf().addSdfSearchPathContributor(contrib);
	}

	public void addClassLoader(java.lang.ClassLoader loader) {
		// later loaders have precedence
		this.classLoaders.add(0, loader);
	}

	public java.lang.String getStackTrace() {
		java.lang.StringBuilder b = new java.lang.StringBuilder();
		org.rascalmpl.interpreter.env.Environment env = this.currentEnvt;
		while (env != null) {
			org.eclipse.imp.pdb.facts.ISourceLocation loc = env.getLocation();
			java.lang.String name = env.getName();
			if (name != null && loc != null) {
				java.net.URI uri = loc.getURI();
				b.append('\t');
				b.append(uri.getRawPath()+ ":" + loc.getBeginLine() + "," + loc.getBeginColumn() + ": " + name);
				b.append('\n');
			} else if (name != null) {
				b.append('\t');
				b.append("somewhere in: " + name);
				b.append('\n');
			}
			env = env.getCallerScope();
		}
		return b.toString();
	}

	/**
	 * Evaluate a statement
	 * @param stat
	 * @return
	 */
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(org.rascalmpl.ast.Statement stat) {
		this.__setInterrupt(false);
		try {
			if(Evaluator.doProfiling){
				this.profiler = new org.rascalmpl.interpreter.utils.Profiler(this);
				this.profiler.start();

			}
			this.currentAST = stat;
			try {
//				return stat.accept(this);
				return stat.__evaluate(this);
			}
			finally {
				if(Evaluator.doProfiling) {
					if (this.profiler != null) {
						this.profiler.pleaseStop();
						this.profiler.report();
					}
				}
			}
		} 
		catch (org.rascalmpl.interpreter.control_exceptions.Return e){
			throw new org.rascalmpl.interpreter.staticErrors.UnguardedReturnError(stat);
		}
		catch (org.rascalmpl.interpreter.control_exceptions.Failure e){
			throw new org.rascalmpl.interpreter.staticErrors.UnguardedFailError(stat);
		}
		catch (org.rascalmpl.interpreter.control_exceptions.Insert e){
			throw new org.rascalmpl.interpreter.staticErrors.UnguardedInsertError(stat);
		}
	}

	/**
	 * Evaluate an expression
	 * @param expr
	 * @return
	 */
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(org.rascalmpl.ast.Expression expr) {
		this.__setInterrupt(false);
		this.currentAST = expr;
		if(Evaluator.doProfiling){
			this.profiler = new org.rascalmpl.interpreter.utils.Profiler(this);
			this.profiler.start();

		}
		try {
//			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = expr.accept(this);
			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = expr.__evaluate(this);
			if(r != null){
				return r;
			}
		}
		finally {
			if(Evaluator.doProfiling){
				if (this.profiler != null) {
					this.profiler.pleaseStop();
					this.profiler.report();
				}
			}
		}

		throw new org.rascalmpl.interpreter.asserts.NotYetImplemented(expr.toString());
	}

	private java.util.Set<java.lang.String> getSDFImports() {
		return ((org.rascalmpl.interpreter.env.ModuleEnvironment) this.getCurrentEnvt().getRoot()).getSDFImports();
	}
	
	/**
	 * Parse and evaluate a command in the current execution environment
	 * @param command
	 * @return
	 */
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(java.lang.String command, java.net.URI location) {
		try {
			org.eclipse.imp.pdb.facts.IConstructor tree = this.__getParser().parseCommand(this.getSDFImports(), this.__getSdf().getSdfSearchPath(), location, command);
			
			if (tree.getConstructorType() == Factory.ParseTree_Summary) {
				throw this.parseError(tree, location);
			}
			
			org.rascalmpl.ast.Command stat = this.builder.buildCommand(tree);
			
			if (stat == null) {
				throw new org.rascalmpl.interpreter.asserts.ImplementationError("Disambiguation failed: it removed all alternatives");
			}
			
			return this.eval(stat);
		} catch (java.io.IOException e) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("something weird happened", e);
		}
	}
	
	public org.eclipse.imp.pdb.facts.IConstructor parseCommand(java.lang.String command, java.net.URI location) {
		org.eclipse.imp.pdb.facts.IConstructor tree;
		try {
			tree = this.__getParser().parseCommand(this.getSDFImports(), this.__getSdf().getSdfSearchPath(), location, command);
			
			if (this.__getParser() instanceof org.rascalmpl.parser.NewRascalParser) {
				// execute the parse actions
				// TODO: hide this inside the parser
				tree = new org.rascalmpl.parser.ActionExecutor(this, ((org.rascalmpl.parser.NewRascalParser) this.__getParser()).getInfo()).execute(tree);
			}
			if (tree.getConstructorType() == Factory.ParseTree_Summary) {
				throw this.parseError(tree, location);
			}
			
			return tree;
		} catch (java.io.IOException e) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("something weird happened", e);
		}
	}
	
	public org.eclipse.imp.pdb.facts.IConstructor parseCommandExperimental(java.lang.String command, java.net.URI location) {
		org.rascalmpl.parser.sgll.IGLL parser = new org.rascalmpl.library.rascal.syntax.RascalRascal();
		try {
			return parser.parse("Command", location, new java.io.ByteArrayInputStream(command.getBytes()));
		} catch (java.io.IOException e) {
			// TODO
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("TODO: " + e.getMessage());
		}
	}

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(org.rascalmpl.ast.Command command) {
		this.__setInterrupt(false);
		if (Evaluator.doProfiling){
			this.profiler = new org.rascalmpl.interpreter.utils.Profiler(this);
			this.profiler.start();

		}
		try {
//			return command.accept(this);
			return command.__evaluate(this);
		}
		finally {
			if(Evaluator.doProfiling){
				if (this.profiler != null) {
					this.profiler.pleaseStop();
					this.profiler.report();
				}
			}
		}
	}
	
//	protected void evalSDFModule(Default x) {
//		// TODO: find out what this is for
//		if (currentEnvt == rootScope) {
//			parser.addSdfImportForImportDefault(x);
//		}
////		super.evalSDFModule(x);
//	}
	
//	private IConstructor parseModule(String contents, ModuleEnvironment env) throws IOException{
//		URI uri = URI.create("stdin:///");
//		java.util.List<String> sdfSearchPath = sdf.getSdfSearchPath();
//		java.util.Set<String> sdfImports = parser.getSdfImports(sdfSearchPath, uri, contents.getBytes());
//
//		IConstructor tree = parser.parseModule(sdfSearchPath, sdfImports, uri, contents.getBytes(), env);
//		
//		if(tree.getConstructorType() == Factory.ParseTree_Summary){
//			throw parseError(tree, uri);
//		}
//		
//		return tree;
//	}

	
	/**
	 * Evaluate a declaration
	 * @param declaration
	 * @return
	 */
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(org.rascalmpl.ast.Declaration declaration) {
		this.__setInterrupt(false);
		this.currentAST = declaration;
//		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = declaration.accept(this);
		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = declaration.__evaluate(this);
		if(r != null){
			return r;
		}

		throw new org.rascalmpl.interpreter.asserts.NotYetImplemented(declaration.toString());
	}

	/**
	 * Evaluate an import
	 * @param imp
	 * @return
	 */
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> eval(org.rascalmpl.ast.Import imp) {
		this.__setInterrupt(false);
		this.currentAST = imp;
//		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = imp.accept(this);
		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r = imp.__evaluate(this);
		if(r != null){
			return r;
		}

		throw new org.rascalmpl.interpreter.asserts.ImplementationError("Not yet implemented: " + imp.getTree());
	}
	
	public void doImport(java.lang.String string) {
		this.eval("import " + string + ";", java.net.URI.create("import:///"));
	}


	public void reloadModule(java.lang.String name) {
		try {
			if (!this.__getHeap().existsModule(name)) {
				return; // ignore modules we don't know about
			}

			org.rascalmpl.interpreter.env.ModuleEnvironment mod = this.__getHeap().resetModule(name);

			org.rascalmpl.ast.Module module = this.loadModule(name, mod);

			if (module != null) {
				if (!this.getModuleName(module).equals(name)) {
					throw new org.rascalmpl.interpreter.staticErrors.ModuleNameMismatchError(this.getModuleName(module), name, this.getCurrentAST());
				}
//				module.accept(this);
				module.__evaluate(this);
			}
		}
		catch (java.io.IOException e) {
			throw new org.rascalmpl.interpreter.staticErrors.ModuleLoadError(name, e.getMessage(), this.getCurrentAST());
		}
	}
	
	/* First a number of general utility methods */

	/*
	 * Return an evaluation result that is already in normal form,
	 * i.e., all potential rules have already been applied to it.
	 */

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> normalizedResult(org.eclipse.imp.pdb.facts.type.Type t, org.eclipse.imp.pdb.facts.IValue v){
		java.util.Map<org.eclipse.imp.pdb.facts.type.Type, org.eclipse.imp.pdb.facts.type.Type> bindings = this.getCurrentEnvt().getTypeBindings();
		org.eclipse.imp.pdb.facts.type.Type instance;

		if (bindings.size() > 0) {
			instance = t.instantiate(bindings);
		}
		else {
			instance = t;
		}

		if (v != null) {
			this.checkType(v.getType(), instance);
		}
		return org.rascalmpl.interpreter.result.ResultFactory.makeResult(instance, v, this);
	}

	public void unwind(org.rascalmpl.interpreter.env.Environment old) {
		// TODO why not just replace the current env with the old one??
		while (this.getCurrentEnvt() != old) {
			this.setCurrentEnvt(this.getCurrentEnvt().getParent());
			this.getCurrentEnvt();
		}
	}

	public void pushEnv() {
		org.rascalmpl.interpreter.env.Environment env = new org.rascalmpl.interpreter.env.Environment(this.getCurrentEnvt(), this.getCurrentEnvt().getName());
		this.setCurrentEnvt(env);
	}

	public org.rascalmpl.interpreter.env.Environment pushEnv(org.rascalmpl.ast.Statement s) {
		/* use the same name as the current envt */
		org.rascalmpl.interpreter.env.Environment env = new org.rascalmpl.interpreter.env.Environment(this.getCurrentEnvt(), s.getLocation(), this.getCurrentEnvt().getName());
		this.setCurrentEnvt(env);
		return env;
	}


	private void checkType(org.eclipse.imp.pdb.facts.type.Type given, org.eclipse.imp.pdb.facts.type.Type expected) {
		if (expected instanceof org.rascalmpl.interpreter.types.FunctionType) {
			return;
		}
		if (!given.isSubtypeOf(expected)){
			throw new org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError(expected, given, this.getCurrentAST());
		}
	}

	public boolean mayOccurIn(org.eclipse.imp.pdb.facts.type.Type small, org.eclipse.imp.pdb.facts.type.Type large) {
		return this.mayOccurIn(small, large, new java.util.HashSet<org.eclipse.imp.pdb.facts.type.Type>());
	}

	boolean mayOccurIn(org.eclipse.imp.pdb.facts.type.Type small, org.eclipse.imp.pdb.facts.type.Type large, java.util.Set<org.eclipse.imp.pdb.facts.type.Type> seen){
		// TODO: this should probably be a visitor as well

		if(small.isVoidType())
			return true;
		if(large.isVoidType())
			return false;
		if(small.isValueType())
			return true;
		if(small.isSubtypeOf(large))
			return true;
		if(large.isListType() || large.isSetType())
			return this.mayOccurIn(small,large.getElementType(), seen);
		if(large.isMapType())
			return this.mayOccurIn(small, large.getKeyType(), seen) ||
			this.mayOccurIn(small, large.getValueType(), seen);
		if(large.isTupleType()){
			for(int i = 0; i < large.getArity(); i++){
				if(this.mayOccurIn(small, large.getFieldType(i), seen))
					return true;
			}
			return false;
		}

		if(large instanceof org.rascalmpl.interpreter.types.NonTerminalType && small instanceof org.rascalmpl.interpreter.types.NonTerminalType){
			//TODO: Until we have more precise info about the types in the concrete syntax
			// we just return true here.
			return true;
		}

		if(large.isConstructorType()){

			for(int i = 0; i < large.getArity(); i++){
				if(this.mayOccurIn(small, large.getFieldType(i), seen))
					return true;
			}
			return false;
		}
		if(large.isAbstractDataType()){
			if(small.isNodeType() && !small.isAbstractDataType())
				return true;
			if(small.isConstructorType() && small.getAbstractDataType().equivalent(large.getAbstractDataType()))
				return true;
			seen.add(large);
			for(org.eclipse.imp.pdb.facts.type.Type alt : this.getCurrentEnvt().lookupAlternatives(large)){				
				if(alt.isConstructorType()){
					for(int i = 0; i < alt.getArity(); i++){
						org.eclipse.imp.pdb.facts.type.Type fType = alt.getFieldType(i);
						if(seen.add(fType) && this.mayOccurIn(small, fType, seen))
							return true;
					}
				} else
					throw new org.rascalmpl.interpreter.asserts.ImplementationError("ADT");

			}
			return false;
		}
		return small.isSubtypeOf(large);
	}




	// Ambiguity ...................................................

	public void printHelpMessage(java.io.PrintWriter out) {
		out.println("Welcome to the Rascal command shell.");
		out.println();
		out.println("Shell commands:");
		out.println(":help                      Prints this message");
		out.println(":quit or EOF               Quits the shell");
		out.println(":declarations              Lists all visible rules, functions and variables");
		out.println(":set <option> <expression> Sets an option");
		out.println("e.g. profiling    true/false");
		out.println("     tracing      true/false");
		out.println("     saveBinaries true/false");
		out.println(":edit <modulename>         Opens an editor for that module");
		out.println(":modules                   Lists all imported modules");
		out.println(":test                      Runs all unit tests currently loaded");
		out.println(":unimport <modulename>     Undo an import");
		out.println(":undeclare <name>          Undeclares a variable or function introduced in the shell");
		out.println(":history                   Print the command history");
		out.println();
		out.println("Example rascal statements and declarations:");
		out.println("1 + 1;                     Expressions simply print their output and (static) type");
		out.println("int a;                     Declarations allocate a name in the current scope");
		out.println("a = 1;                     Assignments store a value in a (optionally previously declared) variable");
		out.println("int a = 1;                 Declaration with initialization");
		out.println("import IO;                 Importing a module makes its public members available");
		out.println("println(\"Hello World\")     Function calling");
		out.println();
		out.println("Please read the manual for further information");
		out.flush();
	}

	public void printVisibleDeclaredObjects(java.io.PrintWriter out) {
		java.util.List<java.util.Map.Entry<java.lang.String, org.rascalmpl.interpreter.result.OverloadedFunctionResult>> functions = this.getCurrentEnvt().getAllFunctions();
		java.util.Collections.sort(functions, new java.util.Comparator<java.util.Map.Entry<java.lang.String, org.rascalmpl.interpreter.result.OverloadedFunctionResult>>() {
			public int compare(java.util.Map.Entry<java.lang.String, org.rascalmpl.interpreter.result.OverloadedFunctionResult> o1,
					java.util.Map.Entry<java.lang.String, org.rascalmpl.interpreter.result.OverloadedFunctionResult> o2) {
				return o1.getKey().compareTo(o2.getKey());
			}
		});
		
		if (functions.size() != 0) {
			out.println("Functions:");

			for (java.util.Map.Entry<java.lang.String, org.rascalmpl.interpreter.result.OverloadedFunctionResult> cand : functions) {
				for (org.rascalmpl.interpreter.result.AbstractFunction func : cand.getValue().iterable()) {
					out.print('\t');
					out.println(func.getHeader());
				}
			}
		}
		

		java.util.List<org.rascalmpl.interpreter.env.RewriteRule> rules = this.getHeap().getRules();
		if (rules.size() != 0) {
			out.println("Rules:");
			for (org.rascalmpl.interpreter.env.RewriteRule rule : rules) {
				out.print('\t');
				out.println(rule.getRule().getPattern().toString());
			}
		}
		
		java.util.Map<java.lang.String, org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue>> variables = this.getCurrentEnvt().getVariables();
		if (variables.size() != 0) {
			out.println("Variables:");
			for (java.lang.String name : variables.keySet()) {
				out.print('\t');
				org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> value = variables.get(name);
				out.println(value.getType() + " " + name + " = " + value.getValue());
			}
		}
		
		out.flush();
	}
	
	// Modules -------------------------------------------------------------

	public void evalSDFModule(java.lang.String name, 
			org.rascalmpl.ast.Import.Default x) {
		this.loadParseTreeModule(x);
		this.getCurrentModuleEnvironment().addSDFImport(this.getUnescapedModuleName(x));
		org.rascalmpl.interpreter.env.ModuleEnvironment mod = this.__getHeap().getModule(name);
		if (mod == null) {
			mod = new org.rascalmpl.interpreter.env.ModuleEnvironment(name);
			this.__getHeap().addModule(mod);
		}
		
		try {
			((org.rascalmpl.parser.LegacyRascalParser) this.__getParser()).generateModuleParser(this.__getSdf().getSdfSearchPath(), this.getCurrentModuleEnvironment().getSDFImports(), mod);
		} catch (java.io.IOException e) {
			org.rascalmpl.interpreter.utils.RuntimeExceptionFactory.io(this.__getVf().string("IO exception while importing module " + x), x, this.getStackTrace());
		}
	}

	public void addImportToCurrentModule(
			org.rascalmpl.ast.Import.Default x, java.lang.String name) {
		org.rascalmpl.interpreter.env.ModuleEnvironment module = this.__getHeap().getModule(name);
		if (module == null) {
			throw new org.rascalmpl.interpreter.staticErrors.UndeclaredModuleError(name, x);
		}
		this.getCurrentModuleEnvironment().addImport(name, module);
	}

	public org.rascalmpl.interpreter.env.ModuleEnvironment getCurrentModuleEnvironment() {
		if (!(this.currentEnvt instanceof org.rascalmpl.interpreter.env.ModuleEnvironment)) {
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("Current env should be a module environment");
		}
		return ((org.rascalmpl.interpreter.env.ModuleEnvironment) this.currentEnvt);
	}

	public java.lang.String getUnescapedModuleName(
			org.rascalmpl.ast.Import.Default x) {
		return org.rascalmpl.interpreter.utils.Names.fullName(x.getModule().getName());
	}

	private void loadParseTreeModule(
			org.rascalmpl.ast.Import.Default x) {
		java.lang.String parseTreeModName = "ParseTree";
		if (!this.__getHeap().existsModule(parseTreeModName)) {
			this.evalRascalModule(x, parseTreeModName);
		}
		this.addImportToCurrentModule(x, parseTreeModName);
	}

	public boolean isSDFModule(java.lang.String name) {
		return this.__getSdf().isSdfModule(name);
	}

	private org.eclipse.imp.pdb.facts.IConstructor tryLoadBinary(java.lang.String name){
		java.io.InputStream inputStream = null;
		
		try {
			inputStream = this.resolver.getBinaryInputStream(java.net.URI.create("rascal:///" + name));
			if(inputStream == null) {
				return null;
			}

			org.eclipse.imp.pdb.facts.io.PBFReader pbfReader = new org.eclipse.imp.pdb.facts.io.PBFReader();
			return (org.eclipse.imp.pdb.facts.IConstructor) pbfReader.read(org.rascalmpl.values.ValueFactoryFactory.getValueFactory(), inputStream);
		}
		catch (java.io.IOException e) {
			return null;
		}
		finally {
			try {
				if (inputStream != null) {
					inputStream.close();
				}
			}
			catch (java.io.IOException ioex){
				throw new org.rascalmpl.interpreter.asserts.ImplementationError(ioex.getMessage(), ioex);
			}
		}
	}
	
	private void writeBinary(java.lang.String name, org.eclipse.imp.pdb.facts.IConstructor tree) throws IOException {
		java.io.OutputStream outputStream = null;
		org.eclipse.imp.pdb.facts.io.PBFWriter pbfWriter = new org.eclipse.imp.pdb.facts.io.PBFWriter();
		
		try{
			outputStream = new java.io.BufferedOutputStream(this.resolver.getBinaryOutputStream(java.net.URI.create("rascal:///" + name)));
			pbfWriter.write(tree, outputStream);
		}
		finally{
			if (outputStream != null) {
				outputStream.flush();
				outputStream.close();
			}
		}
	}
	
	/**
	 * Parse a module. Practical for implementing IDE features or features that use Rascal to implement Rascal.
	 * Parsing a module currently has the side effect of declaring non-terminal types in the given environment.
	 */
	public org.eclipse.imp.pdb.facts.IConstructor parseModule(java.net.URI location, org.rascalmpl.interpreter.env.ModuleEnvironment env) throws IOException {
		byte[] data;
		
		java.io.InputStream inputStream = null;
		try {
			inputStream = this.resolverRegistry.getInputStream(location);
			data = this.readModule(inputStream);
		}
		finally{
			if(inputStream != null){
				inputStream.close();
			}
		}

		java.net.URI resolved = this.resolver.resolve(location);
		if (resolved != null) {
			location = resolved;
		}
		
		return this.parseModule(data, location, env);
	}
	
	public org.eclipse.imp.pdb.facts.IConstructor parseModule(byte[] data, java.net.URI location, org.rascalmpl.interpreter.env.ModuleEnvironment env) throws IOException {
		java.util.List<java.lang.String> sdfSearchPath = this.__getSdf().getSdfSearchPath();
		java.util.Set<java.lang.String> sdfImports;

		// TODO: remove support for SDF2 imports
		if (this.__getParser() instanceof org.rascalmpl.parser.LegacyRascalParser) {
			sdfImports = ((org.rascalmpl.parser.LegacyRascalParser) this.__getParser()).getSdfImports(sdfSearchPath, location, data);
		}
		else {
			sdfImports = java.util.Collections.emptySet();
		}

		org.eclipse.imp.pdb.facts.IConstructor tree = this.__getParser().parseModule(sdfSearchPath, sdfImports, location, data, env);
		if(tree.getConstructorType() == Factory.ParseTree_Summary){
			throw this.parseError(tree, location);
		}
		return tree;
	}
	
	public org.eclipse.imp.pdb.facts.IConstructor parseModuleExperimental(java.io.InputStream stream, java.net.URI location) {
		org.rascalmpl.parser.sgll.IGLL parser = new org.rascalmpl.library.rascal.syntax.RascalRascal();
		try {
			return parser.parse("Module", location, stream);
		} catch (java.io.IOException e) {
			// TODO
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("TODO");
		}
	}
		
	private byte[] readModule(java.io.InputStream inputStream) throws IOException{
		byte[] buffer = new byte[8192];
		
		java.io.ByteArrayOutputStream inputStringData = new java.io.ByteArrayOutputStream();
		
		int bytesRead;
		while((bytesRead = inputStream.read(buffer)) != -1){
			inputStringData.write(buffer, 0, bytesRead);
		}
		
		return inputStringData.toByteArray();
	}
	
	protected org.rascalmpl.interpreter.staticErrors.SyntaxError parseError(org.eclipse.imp.pdb.facts.IConstructor tree, java.net.URI location){
		org.rascalmpl.values.errors.SummaryAdapter summary = new org.rascalmpl.values.errors.SummaryAdapter(tree);
		org.rascalmpl.values.errors.SubjectAdapter subject = summary.getInitialSubject();
		org.eclipse.imp.pdb.facts.IValueFactory vf = org.rascalmpl.values.ValueFactoryFactory.getValueFactory();
		
		if (subject != null) {
			org.eclipse.imp.pdb.facts.ISourceLocation loc = vf.sourceLocation(location, subject.getOffset(), subject.getLength(), subject.getBeginLine(), subject.getEndLine(), subject.getBeginColumn(), subject.getEndColumn());
			return new org.rascalmpl.interpreter.staticErrors.SyntaxError(subject.getDescription(), loc);
		}
		
		return new org.rascalmpl.interpreter.staticErrors.SyntaxError("unknown location, maybe you used a keyword as an identifier)", vf.sourceLocation(location, 0,1,1,1,0,1));
	}
	
	
	
	private org.rascalmpl.ast.Module loadModule(java.lang.String name, org.rascalmpl.interpreter.env.ModuleEnvironment env) throws IOException {
		if(this.isSDFModule(name)){
			return null;
		}
		
		try{
			org.eclipse.imp.pdb.facts.IConstructor tree = null;
			
			if (!this.saveParsedModules && !(this.__getParser() instanceof org.rascalmpl.parser.NewRascalParser)) {
				tree = this.tryLoadBinary(name);
			}
			
			if (tree == null) {
				tree = this.parseModule(java.net.URI.create("rascal:///" + name), env);
			}
			
			if (this.saveParsedModules && !(this.__getParser() instanceof org.rascalmpl.parser.NewRascalParser)) {
				this.writeBinary(name, tree);
			}
			
			org.rascalmpl.parser.ASTBuilder astBuilder = new org.rascalmpl.parser.ASTBuilder(ASTFactoryFactory.getASTFactory());
			org.rascalmpl.ast.Module moduleAst = astBuilder.buildModule(tree);
			
			if (moduleAst == null) {
				throw new org.rascalmpl.interpreter.asserts.ImplementationError("After this, all ambiguous ast's have been filtered in " + name, astBuilder.getLastSuccessLocation());
			}
			return moduleAst;
		}catch (org.eclipse.imp.pdb.facts.exceptions.FactTypeUseException e){
			throw new org.rascalmpl.interpreter.asserts.ImplementationError("Unexpected PDB typecheck exception", e);
		}
	}
	
	public org.rascalmpl.ast.Module evalRascalModule(org.rascalmpl.ast.AbstractAST x,
			java.lang.String name) {
		org.rascalmpl.interpreter.env.ModuleEnvironment env = this.__getHeap().getModule(name);
		if (env == null) {
			env = new org.rascalmpl.interpreter.env.ModuleEnvironment(name);
			this.__getHeap().addModule(env);
		}
		try {
			org.rascalmpl.ast.Module module = this.loadModule(name, env);
	
			if (module != null) {
				if (!this.getModuleName(module).equals(name)) {
					throw new org.rascalmpl.interpreter.staticErrors.ModuleNameMismatchError(this.getModuleName(module), name, x);
				}
				this.__getHeap().setModuleURI(name, module.getLocation().getURI());
//				module.accept(this);
				module.__evaluate(this);
				return module;
			}
		}
		catch (org.rascalmpl.interpreter.staticErrors.StaticError e) {
			this.__getHeap().removeModule(env);
			throw e;
		}
		catch (org.rascalmpl.interpreter.control_exceptions.Throw e) {
			this.__getHeap().removeModule(env);
			throw e;
		} 
		catch (java.io.IOException e) {
			throw new org.rascalmpl.interpreter.staticErrors.ModuleLoadError(name, e.getMessage(), x);
		}

		throw new org.rascalmpl.interpreter.asserts.ImplementationError("Unexpected error while parsing module " + name + " and building an AST for it ", x.getLocation());
	}

	public java.lang.String getModuleName(
			org.rascalmpl.ast.Module module) {
		java.lang.String name = module.getHeader().getName().toString();
		if (name.startsWith("\\")) {
			name = name.substring(1);
		}
		return name;
	}

	public void visitImports(java.util.List<org.rascalmpl.ast.Import> imports) {
		for (org.rascalmpl.ast.Import i : imports) {
//			i.accept(this);
			i.__evaluate(this);
		}
	}

	public org.eclipse.imp.pdb.facts.type.Type evalType(org.rascalmpl.ast.Type type) {
		return new org.rascalmpl.interpreter.TypeEvaluator(this.getCurrentEnvt(), this.__getHeap()).eval(type);
	}

	public boolean hasJavaModifier(org.rascalmpl.ast.FunctionDeclaration func) {
		java.util.List<org.rascalmpl.ast.FunctionModifier> mods = func.getSignature().getModifiers().getModifiers();
		for (org.rascalmpl.ast.FunctionModifier m : mods) {
			if (m.isJava()) {
				return true;
			}
		}

		return false;
	}

	public boolean isWildCard(java.lang.String fieldName){
		return fieldName.equals("_");
	}

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> evalStatementTry(org.rascalmpl.ast.Statement body, java.util.List<org.rascalmpl.ast.Catch> handlers, org.rascalmpl.ast.Statement finallyBody){
		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> res = org.rascalmpl.interpreter.result.ResultFactory.nothing();

		try {
//			res = body.accept(this);
			res = body.__evaluate(this);
		} catch (org.rascalmpl.interpreter.control_exceptions.Throw e){
			org.eclipse.imp.pdb.facts.IValue eValue = e.getException();

			for (org.rascalmpl.ast.Catch c : handlers){
				if(c.isDefault()){
//					res = c.getBody().accept(this);
					res = c.getBody().__evaluate(this);
					break;
				} 

				// TODO: Throw should contain Result<IValue> instead of IValue
				if(this.matchAndEval(org.rascalmpl.interpreter.result.ResultFactory.makeResult(eValue.getType(), eValue, this), c.getPattern(), c.getBody())){
					break;
				}
			}
		}
		finally {
			if (finallyBody != null) {
//				finallyBody.accept(this);
				finallyBody.__evaluate(this);
			}
		}
		return res;
	}

	public org.rascalmpl.interpreter.matching.IBooleanResult makeBooleanResult(org.rascalmpl.ast.Expression pat){
		if (pat instanceof org.rascalmpl.ast.Expression.Ambiguity) {
			// TODO: wrong exception here.
			throw new org.rascalmpl.interpreter.asserts.Ambiguous((org.eclipse.imp.pdb.facts.IConstructor) pat.getTree());
		}

		org.rascalmpl.interpreter.BooleanEvaluator pe = new org.rascalmpl.interpreter.BooleanEvaluator(this);
//		return pat.accept(pe);
		return pat.__evaluate(pe);
	}

	// Expressions -----------------------------------------------------------

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> evalBooleanExpression(org.rascalmpl.ast.Expression x) {
		org.rascalmpl.interpreter.matching.IBooleanResult mp = this.makeBooleanResult(x);
		mp.init();
		while(mp.hasNext()){
			if (this.__getInterrupt()) throw new org.rascalmpl.interpreter.control_exceptions.InterruptException(this.getStackTrace());
			if(mp.next()) {
				return org.rascalmpl.interpreter.result.ResultFactory.bool(true, this);
			}
		}
		return org.rascalmpl.interpreter.result.ResultFactory.bool(false, this);
	}

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> createVisitedDateTime(java.lang.String datePart, java.lang.String timePart,
			org.rascalmpl.ast.DateAndTime.Lexical x) {
		java.lang.String isoDate = datePart;
		if (-1 == datePart.indexOf("-")) {
			isoDate = datePart.substring(0,4) + "-" + datePart.substring(4,6) + "-" + 
			          datePart.substring(6);
		}
		java.lang.String isoTime = timePart;
		if (-1 == timePart.indexOf(":")) {			
			isoTime = timePart.substring(0, 2) + ":" + timePart.substring(2,4) + ":" +
					  timePart.substring(4);
		}
		java.lang.String isoDateTime = isoDate + "T" + isoTime;
		try {
			org.joda.time.DateTime dateAndTime = org.joda.time.format.ISODateTimeFormat.dateTimeParser().parseDateTime(isoDateTime);
			int hourOffset = dateAndTime.getZone().getOffset(dateAndTime.getMillis())/3600000;
			int minuteOffset = (dateAndTime.getZone().getOffset(dateAndTime.getMillis())/60000) % 60;		
			return org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().dateTimeType(),
					this.__getVf().datetime(dateAndTime.getYear(), dateAndTime.getMonthOfYear(), 
							dateAndTime.getDayOfMonth(), dateAndTime.getHourOfDay(), 
							dateAndTime.getMinuteOfHour(), dateAndTime.getSecondOfMinute(),
							dateAndTime.getMillisOfSecond(), hourOffset, minuteOffset), this);
		} catch (java.lang.IllegalArgumentException iae) {
			throw new org.rascalmpl.interpreter.staticErrors.DateTimeParseError("$" + datePart + "T" + timePart, x.getLocation());
		}
	}

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> createVisitedDate(java.lang.String datePart,
			org.rascalmpl.ast.JustDate.Lexical x) {
		java.lang.String isoDate = datePart;
		if (-1 == datePart.indexOf("-")) {
			isoDate = datePart.substring(0,4) + "-" + datePart.substring(4,6) + "-" + 
			          datePart.substring(6);
		}
		try {
			org.joda.time.DateTime justDate = org.joda.time.format.ISODateTimeFormat.dateParser().parseDateTime(isoDate);
			return org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().dateTimeType(),
					this.__getVf().date(justDate.getYear(), justDate.getMonthOfYear(), 
							justDate.getDayOfMonth()), this);
		} catch (java.lang.IllegalArgumentException iae) {
			throw new org.rascalmpl.interpreter.staticErrors.DateTimeParseError("$" + datePart, x.getLocation());
		}			
	}

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> createVisitedTime(java.lang.String timePart,
			org.rascalmpl.ast.JustTime.Lexical x) {
		java.lang.String isoTime = timePart;
		if (-1 == timePart.indexOf(":")) {			
			isoTime = timePart.substring(0, 2) + ":" + timePart.substring(2,4) + ":" +
					  timePart.substring(4);
		}
		try {
			org.joda.time.DateTime justTime = org.joda.time.format.ISODateTimeFormat.timeParser().parseDateTime(isoTime);
			int hourOffset = justTime.getZone().getOffset(justTime.getMillis())/3600000;
			int minuteOffset = (justTime.getZone().getOffset(justTime.getMillis())/60000) % 60;		
			return org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().dateTimeType(),
					this.__getVf().time(justTime.getHourOfDay(), justTime.getMinuteOfHour(), justTime.getSecondOfMinute(),
							justTime.getMillisOfSecond(), hourOffset, minuteOffset), this);
		} catch (java.lang.IllegalArgumentException iae) {
			throw new org.rascalmpl.interpreter.staticErrors.DateTimeParseError("$T" + timePart, x.getLocation());
		}						
	}


	public boolean matchAndEval(org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> subject, org.rascalmpl.ast.Expression pat, org.rascalmpl.ast.Statement stat){
		boolean debug = false;
		org.rascalmpl.interpreter.env.Environment old = this.getCurrentEnvt();
		this.pushEnv();

		try {
//			org.rascalmpl.interpreter.matching.IMatchingResult mp = pat.accept(this.__getPatternEvaluator());
			org.rascalmpl.interpreter.matching.IMatchingResult mp = pat.__evaluate(this.__getPatternEvaluator());
			mp.initMatch(subject);
			if(debug)System.err.println("matchAndEval: subject=" + subject + ", pat=" + pat);
			while(mp.hasNext()){
				this.pushEnv();
				if (this.__getInterrupt()) throw new org.rascalmpl.interpreter.control_exceptions.InterruptException(this.getStackTrace());
				if(debug)System.err.println("matchAndEval: mp.hasNext()==true");
				if(mp.next()){
					if(debug)System.err.println("matchAndEval: mp.next()==true");
					try {
						this.checkPoint(this.getCurrentEnvt());
						if(debug)System.err.println(stat.toString());
						try {
//							stat.accept(this);
							stat.__evaluate(this);
						} catch (org.rascalmpl.interpreter.control_exceptions.Insert e){
							// Make sure that the match pattern is set
							if(e.getMatchPattern() == null) {
								e.setMatchPattern(mp);
							}
							throw e;
						}
						this.commit(this.getCurrentEnvt());
						return true;
					} catch (org.rascalmpl.interpreter.control_exceptions.Failure e){
						if(debug) System.err.println("failure occurred");
						this.rollback(this.getCurrentEnvt());
//						 unwind(old); // can not clean up because you don't know how far to roll back
					}
				}
			}
		} finally {
			if(debug)System.err.println("Unwind to old env");
			this.unwind(old);
		}
		return false;
	}

	boolean matchEvalAndReplace(org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> subject, 
			org.rascalmpl.ast.Expression pat, 
			java.util.List<org.rascalmpl.ast.Expression> conditions,
			org.rascalmpl.ast.Expression replacementExpr){
		org.rascalmpl.interpreter.env.Environment old = this.getCurrentEnvt();
		try {
//			org.rascalmpl.interpreter.matching.IMatchingResult mp = pat.accept(this.__getPatternEvaluator());
			org.rascalmpl.interpreter.matching.IMatchingResult mp = pat.__evaluate(this.__getPatternEvaluator());
			mp.initMatch(subject);

			while (mp.hasNext()){
				if (this.__getInterrupt()) throw new org.rascalmpl.interpreter.control_exceptions.InterruptException(this.getStackTrace());
				if(mp.next()){
					try {
						boolean trueConditions = true;
						for(org.rascalmpl.ast.Expression cond : conditions){
//							if(!cond.accept(this).isTrue()){
							if(!cond.__evaluate(this).isTrue()){
								trueConditions = false;
								break;
							}
						}
						if(trueConditions){
//							throw new org.rascalmpl.interpreter.control_exceptions.Insert(replacementExpr.accept(this), mp);		
							throw new org.rascalmpl.interpreter.control_exceptions.Insert(replacementExpr.__evaluate(this), mp);		
						}
					} catch (org.rascalmpl.interpreter.control_exceptions.Failure e){
						System.err.println("failure occurred");
					}
				}
			}
		} finally {
			this.unwind(old);
		}
		return false;
	}

	private abstract class ComprehensionWriter {
		protected org.eclipse.imp.pdb.facts.type.Type elementType1;
		protected org.eclipse.imp.pdb.facts.type.Type elementType2;
		protected org.eclipse.imp.pdb.facts.type.Type resultType;
		protected java.util.List<org.rascalmpl.ast.Expression> resultExprs;
		protected org.eclipse.imp.pdb.facts.IWriter writer;
		protected org.rascalmpl.interpreter.Evaluator ev;

		ComprehensionWriter(
				java.util.List<org.rascalmpl.ast.Expression> resultExprs,
				org.rascalmpl.interpreter.Evaluator ev){
			this.ev = ev;
			this.resultExprs = resultExprs;
			this.writer = null;
		}

		public void check(org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r, org.eclipse.imp.pdb.facts.type.Type t, java.lang.String kind, org.rascalmpl.ast.Expression expr){
			if(!r.getType().isSubtypeOf(t)){
				throw new org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError(t, r.getType() ,
						expr);
			}
		}

		public org.rascalmpl.interpreter.IEvaluatorContext getContext(org.rascalmpl.ast.AbstractAST ast) {
			Evaluator.this.setCurrentAST(ast);
			return Evaluator.this;
		}

		public abstract void append();


		public abstract org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> done();
	}

	public class ListComprehensionWriter extends
	org.rascalmpl.interpreter.Evaluator.ComprehensionWriter {

		private boolean splicing[];
		private org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> rawElements[];
		
		@SuppressWarnings("unchecked")
		public
		ListComprehensionWriter(
				java.util.List<org.rascalmpl.ast.Expression> resultExprs,
				org.rascalmpl.interpreter.Evaluator ev) {
			super(resultExprs, ev);
			this.splicing = new boolean[resultExprs.size()];
			this.rawElements = new org.rascalmpl.interpreter.result.Result[resultExprs.size()];
		}

		@Override
		public void append() {
			// the first time we need to find out the type of the elements first, and whether or not to splice them, and evaluate them
			if(this.writer == null) {
				int k = 0;
				this.elementType1 = org.rascalmpl.interpreter.Evaluator.__getTf().voidType();
				
				for(org.rascalmpl.ast.Expression resExpr : this.resultExprs){
					//this.rawElements[k] = resExpr.accept(this.ev);
					this.rawElements[k] = resExpr.__evaluate(this.ev);
					org.eclipse.imp.pdb.facts.type.Type elementType = this.rawElements[k].getType();
					
					if (elementType.isListType() && !resExpr.isList()){
						elementType = elementType.getElementType();
						this.splicing[k] = true;
					} 
					else {
						this.splicing[k] = false;
					}
					this.elementType1 = this.elementType1.lub(elementType);
					k++;
				}
				
				this.resultType = org.rascalmpl.interpreter.Evaluator.__getTf().listType(this.elementType1);		
				this.writer = this.resultType.writer(Evaluator.this.__getVf());
			}
			// the second time we only need to evaluate and add the elements
			else {
				int k = 0;
				for (org.rascalmpl.ast.Expression resExpr : this.resultExprs) {
//					this.rawElements[k++] = resExpr.accept(this.ev);
					this.rawElements[k++] = resExpr.__evaluate(this.ev);
				}
			}
			
			// here we finally add the elements
			int k = 0;
			for (org.rascalmpl.ast.Expression resExpr : this.resultExprs) {
				if(this.splicing[k]){
					/*
					 * Splice elements of the value of the result expression in the result list
					 */
					if (!this.rawElements[k].getType().getElementType().isSubtypeOf(this.elementType1)) {
						throw new org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError(this.elementType1, this.rawElements[k].getType().getElementType(), resExpr);
					}
					
					for(org.eclipse.imp.pdb.facts.IValue val : ((org.eclipse.imp.pdb.facts.IList) this.rawElements[k].getValue())){
						((org.eclipse.imp.pdb.facts.IListWriter) this.writer).append(val);
					}
				} else {
					this.check(this.rawElements[k], this.elementType1, "list", resExpr);
					((org.eclipse.imp.pdb.facts.IListWriter) this.writer).append(this.rawElements[k].getValue());
				}
				k++;
			}
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> done() {
			return (this.writer == null) ? org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().listType(org.rascalmpl.interpreter.Evaluator.__getTf().voidType()), Evaluator.this.__getVf().list(), this.getContext(this.resultExprs.get(0))) : 
				org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().listType(this.elementType1), this.writer.done(), this.getContext(this.resultExprs.get(0)));
		}
	}

	public class SetComprehensionWriter extends
	org.rascalmpl.interpreter.Evaluator.ComprehensionWriter {
		private boolean splicing[];
		private org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> rawElements[];
		
		@SuppressWarnings("unchecked")
		public
		SetComprehensionWriter(
				java.util.List<org.rascalmpl.ast.Expression> resultExprs,
				org.rascalmpl.interpreter.Evaluator ev) {
			super(resultExprs, ev);
			this.splicing = new boolean[resultExprs.size()];
			this.rawElements = new org.rascalmpl.interpreter.result.Result[resultExprs.size()];
		}

		@Override
		public void append() {
			// the first time we need to find out the type of the elements first, and whether or not to splice them, and evaluate them
			if(this.writer == null) {
				int k = 0;
				this.elementType1 = org.rascalmpl.interpreter.Evaluator.__getTf().voidType();
				
				for(org.rascalmpl.ast.Expression resExpr : this.resultExprs){
//					this.rawElements[k] = resExpr.accept(this.ev);
					this.rawElements[k] = resExpr.__evaluate(this.ev);
					org.eclipse.imp.pdb.facts.type.Type elementType = this.rawElements[k].getType();
				
					if (elementType.isSetType() && !resExpr.isSet()){
						elementType = elementType.getElementType();
						this.splicing[k] = true;
					} 
					else {
						this.splicing[k] = false;
					}
					this.elementType1 = this.elementType1.lub(elementType);
					k++;
				}
				
				this.resultType = org.rascalmpl.interpreter.Evaluator.__getTf().setType(this.elementType1);		
				this.writer = this.resultType.writer(Evaluator.this.__getVf());
			}
			// the second time we only need to evaluate and add the elements
			else {
				int k = 0;
				for (org.rascalmpl.ast.Expression resExpr : this.resultExprs) {
//					this.rawElements[k++] = resExpr.accept(this.ev);
					this.rawElements[k++] = resExpr.__evaluate(this.ev);
				}
			}
			
			// here we finally add the elements
			int k = 0;
			for (org.rascalmpl.ast.Expression resExpr : this.resultExprs) {
				if(this.splicing[k]){
					/*
					 * Splice elements of the value of the result expression in the result list
					 */
					if (!this.rawElements[k].getType().getElementType().isSubtypeOf(this.elementType1)) {
						throw new org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError(this.elementType1, this.rawElements[k].getType().getElementType(), resExpr);
					}
					
					for(org.eclipse.imp.pdb.facts.IValue val : ((org.eclipse.imp.pdb.facts.ISet) this.rawElements[k].getValue())){
						((org.eclipse.imp.pdb.facts.ISetWriter) this.writer).insert(val);
					}
				} else {
					this.check(this.rawElements[k], this.elementType1, "list", resExpr);
					((org.eclipse.imp.pdb.facts.ISetWriter) this.writer).insert(this.rawElements[k].getValue());
				}
				k++;
			}
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> done() {
			return (this.writer == null) ? org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().setType(org.rascalmpl.interpreter.Evaluator.__getTf().voidType()), Evaluator.this.__getVf().set(), this.getContext(this.resultExprs.get(0))) : 
				org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().setType(this.elementType1), this.writer.done(), this.getContext(this.resultExprs.get(0)));
		}
	}

	public class MapComprehensionWriter extends
	org.rascalmpl.interpreter.Evaluator.ComprehensionWriter {

		public MapComprehensionWriter(
				java.util.List<org.rascalmpl.ast.Expression> resultExprs,
				org.rascalmpl.interpreter.Evaluator ev) {
			super(resultExprs, ev);
			if(resultExprs.size() != 2)
				throw new org.rascalmpl.interpreter.asserts.ImplementationError("Map comprehensions needs two result expressions");
		}

		@Override
		public void append() {
//			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r1 = this.resultExprs.get(0).accept(this.ev);
//			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r2 = this.resultExprs.get(1).accept(this.ev);
			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r1 = this.resultExprs.get(0).__evaluate(this.ev);
			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> r2 = this.resultExprs.get(1).__evaluate(this.ev);
			if (this.writer == null) {
				this.elementType1 = r1.getType();
				this.elementType2 = r2.getType();
				this.resultType = org.rascalmpl.interpreter.Evaluator.__getTf().mapType(this.elementType1, this.elementType2);
				this.writer = this.resultType.writer(Evaluator.this.__getVf());
			}
			this.check(r1, this.elementType1, "map", this.resultExprs.get(0));
			this.check(r2, this.elementType2, "map", this.resultExprs.get(1));
			((org.eclipse.imp.pdb.facts.IMapWriter) this.writer).put(r1.getValue(), r2.getValue());
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> done() {
			return (this.writer == null) ? 
					org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().mapType(org.rascalmpl.interpreter.Evaluator.__getTf().voidType(), org.rascalmpl.interpreter.Evaluator.__getTf().voidType()), Evaluator.this.__getVf().map(org.rascalmpl.interpreter.Evaluator.__getTf().voidType(), org.rascalmpl.interpreter.Evaluator.__getTf().voidType()), this.getContext(this.resultExprs.get(0)))
					: org.rascalmpl.interpreter.result.ResultFactory.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf().mapType(this.elementType1, this.elementType2), this.writer.done(), this.getContext(this.resultExprs.get(0)));
		}
	}


	public static final org.rascalmpl.ast.Name IT = new org.rascalmpl.ast.Name.Lexical(null, "<it>");
	private org.rascalmpl.parser.ParserGenerator parserGenerator;
	
	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> evalReducer(org.rascalmpl.ast.Expression init, org.rascalmpl.ast.Expression result, java.util.List<org.rascalmpl.ast.Expression> generators) {
		int size = generators.size();
		org.rascalmpl.interpreter.matching.IBooleanResult[] gens = new org.rascalmpl.interpreter.matching.IBooleanResult[size];
		org.rascalmpl.interpreter.env.Environment[] olds = new org.rascalmpl.interpreter.env.Environment[size];
		org.rascalmpl.interpreter.env.Environment old = this.getCurrentEnvt();
		int i = 0;
		
//		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> it = init.accept(this);
		org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> it = init.__evaluate(this);

		try {
			gens[0] = this.makeBooleanResult(generators.get(0));
			gens[0].init();
			olds[0] = this.getCurrentEnvt();
			this.pushEnv();

			while (i >= 0 && i < size) {
				if (this.__getInterrupt()) throw new org.rascalmpl.interpreter.control_exceptions.InterruptException(this.getStackTrace());
				if (gens[i].hasNext() && gens[i].next()) {
					if(i == size - 1){
						this.getCurrentEnvt().storeVariable(Evaluator.IT, it);
//						it = result.accept(this);
						it = result.__evaluate(this);
						this.unwind(olds[i]);
						this.pushEnv();
					} 
					else {
						i++;
						gens[i] = this.makeBooleanResult(generators.get(i));
						gens[i].init();
						olds[i] = this.getCurrentEnvt();
						this.pushEnv();
					}
				} else {
					this.unwind(olds[i]);
					i--;
				}
			}
		}
		finally {
			this.unwind(old);
		}
		return it;
	
	}

	/*
	 * The common comprehension evaluator
	 */

	public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> evalComprehension(java.util.List<org.rascalmpl.ast.Expression> generators, 
			org.rascalmpl.interpreter.Evaluator.ComprehensionWriter w){
		int size = generators.size();
		org.rascalmpl.interpreter.matching.IBooleanResult[] gens = new org.rascalmpl.interpreter.matching.IBooleanResult[size];
		org.rascalmpl.interpreter.env.Environment[] olds = new org.rascalmpl.interpreter.env.Environment[size];
		org.rascalmpl.interpreter.env.Environment old = this.getCurrentEnvt();
		int i = 0;

		try {
			gens[0] = this.makeBooleanResult(generators.get(0));
			gens[0].init();
			olds[0] = this.getCurrentEnvt();
			this.pushEnv();

			while (i >= 0 && i < size) {
				if (this.__getInterrupt()) throw new org.rascalmpl.interpreter.control_exceptions.InterruptException(this.getStackTrace());
				if (gens[i].hasNext() && gens[i].next()) {
					if(i == size - 1){
						w.append();
						this.unwind(olds[i]);
						this.pushEnv();
					} 
					else {
						i++;
						gens[i] = this.makeBooleanResult(generators.get(i));
						gens[i].init();
						olds[i] = this.getCurrentEnvt();
						this.pushEnv();
					}
				} else {
					this.unwind(olds[i]);
					i--;
				}
			}
		}
		finally {
			this.unwind(old);
		}
		return w.done();
	}

	public void updateProperties(){
		java.lang.String profiling = java.lang.System.getProperty("rascal.config.profiling");
		if(profiling != null) {
			Evaluator.doProfiling = profiling.equals("true");
		}
		else {
			Evaluator.doProfiling = false;
		}

		java.lang.String tracing = java.lang.System.getProperty("rascal.config.tracing");
		if(tracing != null) {
			org.rascalmpl.interpreter.result.AbstractFunction.setCallTracing(tracing.equals("true"));
		}
		else {
			org.rascalmpl.interpreter.result.AbstractFunction.setCallTracing(false);
		}
		
		java.lang.String sound = java.lang.System.getProperty("rascal.config.soundTracing");
		if (sound != null) {
			org.rascalmpl.interpreter.result.AbstractFunction.setSoundCallTracing(sound.equals("true"));
		}
		else {
			org.rascalmpl.interpreter.result.AbstractFunction.setSoundCallTracing(false);
		}
		
		java.lang.String binaryWriting = java.lang.System.getProperty("rascal.config.saveBinaries");
	    if (binaryWriting != null) {
	    	this.saveParsedModules = binaryWriting.equals("true");
	    }
	    else {
	    	this.saveParsedModules = false;
	    }
	}

	public java.util.Stack<org.rascalmpl.interpreter.env.Environment> getCallStack() {
		java.util.Stack<org.rascalmpl.interpreter.env.Environment> stack = new java.util.Stack<org.rascalmpl.interpreter.env.Environment>();
		org.rascalmpl.interpreter.env.Environment env = this.currentEnvt;
		while (env != null) {
			stack.add(0, env);
			env = env.getCallerScope();
		}
		return stack;
	}

	public org.rascalmpl.interpreter.env.Environment getCurrentEnvt() {
		return this.currentEnvt;
	}

	public void setCurrentEnvt(org.rascalmpl.interpreter.env.Environment env) {
		this.currentEnvt = env;
	}

	public org.rascalmpl.interpreter.Evaluator getEvaluator() {
		return this;
	}

	public org.rascalmpl.interpreter.env.GlobalEnvironment getHeap() {
		return this.__getHeap();
	}

	public boolean runTests(){
		final boolean[] allOk = new boolean[] { true };
		final org.rascalmpl.interpreter.ITestResultListener l = this.testReporter != null ? this.testReporter : new org.rascalmpl.interpreter.DefaultTestResultListener(this.__getStdout());
		
		new org.rascalmpl.interpreter.TestEvaluator(this, new org.rascalmpl.interpreter.ITestResultListener() {
			public void report(boolean successful, java.lang.String test, org.eclipse.imp.pdb.facts.ISourceLocation loc, java.lang.Throwable t) {
				if (!successful) allOk[0] = false;
				l.report(successful, test, loc, t);
			}
			
			public void report(boolean successful, java.lang.String test, org.eclipse.imp.pdb.facts.ISourceLocation loc) {
				if (!successful) allOk[0] = false;
				l.report(successful, test, loc);
			}

			public void done() {l.done();}
			public void start(int count) {l.start(count);}
		}).test();
		return allOk[0];
	}

	public org.eclipse.imp.pdb.facts.IValueFactory getValueFactory() {
		return this.__getVf();
	}

	public void setIValueFactory(
			org.eclipse.imp.pdb.facts.IValueFactory factory) {
		this.__setVf(factory);
	}

	public org.rascalmpl.interpreter.strategy.IStrategyContext getStrategyContext(){
		return this.strategyContextStack.getCurrentContext();
	}

	public void pushStrategyContext(org.rascalmpl.interpreter.strategy.IStrategyContext strategyContext){
		this.strategyContextStack.pushContext(strategyContext);
	}

	public void popStrategyContext(){
		this.strategyContextStack.popContext();
	}

	public void setStdErr(java.io.PrintWriter printWriter) {
		this.stderr = printWriter;
	}  
	
	public void setStdOut(java.io.PrintWriter printWriter) {
		this.__setStdout(printWriter);
	}

	public void setAccumulators(org.rascalmpl.interpreter.Accumulator accu) {
		this.__getAccumulators().push(accu);
	}

	public java.util.Stack<org.rascalmpl.interpreter.Accumulator> getAccumulators() {
		return this.__getAccumulators();
	}

	public void setAccumulators(java.util.Stack<org.rascalmpl.interpreter.Accumulator> accumulators) {
		this.__setAccumulators(accumulators);
	}
	
	public void appendToString(org.eclipse.imp.pdb.facts.IValue value, java.lang.StringBuilder b)
	{
		if (value.getType() == Factory.Tree) {
			b.append(org.rascalmpl.values.uptr.TreeAdapter.yield((org.eclipse.imp.pdb.facts.IConstructor) value));
		}
		else if (value.getType().isStringType()) {
			b.append(((org.eclipse.imp.pdb.facts.IString) value).getValue());
		}
		else {
			b.append(value.toString());
		}
	}


	

	
}
