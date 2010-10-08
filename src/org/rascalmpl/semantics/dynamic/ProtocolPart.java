package org.rascalmpl.semantics.dynamic;

public abstract class ProtocolPart extends org.rascalmpl.ast.ProtocolPart {

	static public class NonInterpolated extends
			org.rascalmpl.ast.ProtocolPart.NonInterpolated {

		public NonInterpolated(org.eclipse.imp.pdb.facts.INode __param1,
				org.rascalmpl.ast.ProtocolChars __param2) {
			super(__param1, __param2);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> __evaluate(
				org.rascalmpl.interpreter.Evaluator __eval) {

			return this.getProtocolChars().accept(__eval);

		}

	}

	static public class Interpolated extends
			org.rascalmpl.ast.ProtocolPart.Interpolated {

		public Interpolated(org.eclipse.imp.pdb.facts.INode __param1,
				org.rascalmpl.ast.PreProtocolChars __param2,
				org.rascalmpl.ast.Expression __param3,
				org.rascalmpl.ast.ProtocolTail __param4) {
			super(__param1, __param2, __param3, __param4);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> __evaluate(
				org.rascalmpl.interpreter.Evaluator __eval) {

			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> pre = this
					.getPre().accept(__eval);
			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> expr = this
					.getExpression().accept(__eval);
			org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> tail = this
					.getTail().accept(__eval);
			java.lang.StringBuilder result = new java.lang.StringBuilder();

			result.append(((org.eclipse.imp.pdb.facts.IString) pre.getValue())
					.getValue());
			__eval.appendToString(expr.getValue(), result);
			result.append(((org.eclipse.imp.pdb.facts.IString) tail.getValue())
					.getValue());

			return org.rascalmpl.interpreter.result.ResultFactory.makeResult(
					org.rascalmpl.interpreter.Evaluator.__getTf().stringType(),
					__eval.__getVf().string(result.toString()), __eval);

		}

	}

	static public class Ambiguity extends
			org.rascalmpl.ast.ProtocolPart.Ambiguity {

		public Ambiguity(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.ProtocolPart> __param2) {
			super(__param1, __param2);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

	}
}