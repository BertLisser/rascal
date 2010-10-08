package org.rascalmpl.interpreter.matching;

import java.util.Iterator;

import org.eclipse.imp.pdb.facts.IValue;
import org.rascalmpl.ast.Expression;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.result.Result;
import org.rascalmpl.interpreter.result.ResultFactory;

public class EnumeratorResult extends BasicBooleanResult {
	private IMatchingResult pat;
	private Iterator<?> iterator;
	private Expression expression;
	private boolean firstTime;    // TODO: can probably be removed.

	/*
	 * Constructor for a standard enumerator
	 */
	
	public EnumeratorResult(IEvaluatorContext ctx, IMatchingResult matchPattern, Expression expression){
		super(ctx, expression);
		this.pat = matchPattern;
		this.expression = expression;
	}
	
	@Override
	public void init() {
		super.init();
		firstTime = true;
	}
	
	@Override
	public boolean hasNext(){
		if (firstTime) {
			hasNext = true;
			return true;
		}
		if (hasNext) {
			boolean hn = pat.hasNext() || iterator.hasNext();
			if(!hn){
				hasNext = false;
			}
			return hn;
		}
		return false;
	}

	@Override
	public boolean next() {
		if (firstTime) {
			firstTime = false;
//			Result<IValue> result = expression.accept(ctx.getEvaluator());
			Result<IValue> result = expression.__evaluate(ctx.getEvaluator());
			iterator = IteratorFactory.make(ctx, pat, result, true);
		}
		/*
		 * First, explore alternatives that remain to be matched by the current pattern
		 */
		while(pat.hasNext()){
			if(pat.next()){
				return true;
			}
		}
		/*
		 * Next, fetch a new data element (if any) and create a new pattern.
		 */
		while(iterator.hasNext()){
			IValue v = (IValue) iterator.next();
			
			// TODO: extract the proper static element type that will be generated
			pat.initMatch(ResultFactory.makeResult(v.getType(), v, ctx));
			while(pat.hasNext()){
				if(pat.next()){
					return true;						
				}	
			}
		}
		hasNext = false;
		return false;
	}
}

