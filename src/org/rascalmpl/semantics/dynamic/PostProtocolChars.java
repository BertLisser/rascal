package org.rascalmpl.semantics.dynamic;

public abstract class PostProtocolChars extends
		org.rascalmpl.ast.PostProtocolChars {

	static public class Ambiguity extends
			org.rascalmpl.ast.PostProtocolChars.Ambiguity {

		public Ambiguity(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.PostProtocolChars> __param2) {
			super(__param1, __param2);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

	}

	static public class Lexical extends
			org.rascalmpl.ast.PostProtocolChars.Lexical {

		public Lexical(org.eclipse.imp.pdb.facts.INode __param1,
				java.lang.String __param2) {
			super(__param1, __param2);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.interpreter.result.Result<org.eclipse.imp.pdb.facts.IValue> __evaluate(
				org.rascalmpl.interpreter.Evaluator __eval) {

			java.lang.String str = this.getString();
			return org.rascalmpl.interpreter.result.ResultFactory
					.makeResult(org.rascalmpl.interpreter.Evaluator.__getTf()
							.stringType(), __eval.__getVf().string(
							str.substring(1, str.length() - 3)), __eval);

		}

	}
}