package org.rascalmpl.semantics.dynamic;

import org.rascalmpl.ast.ASTFactoryFactory;

public abstract class StringTemplate extends org.rascalmpl.ast.StringTemplate {

	static public class DoWhile extends
			org.rascalmpl.ast.StringTemplate.DoWhile {

		public DoWhile(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.Statement> __param2,
				org.rascalmpl.ast.StringMiddle __param3,
				java.util.List<org.rascalmpl.ast.Statement> __param4,
				org.rascalmpl.ast.Expression __param5) {
			super(__param1, __param2, __param3, __param4, __param5);
		}

		@Override
		public org.rascalmpl.ast.Statement __evaluate(
				org.rascalmpl.interpreter.StringTemplateConverter.Visitor __eval) {

			org.rascalmpl.ast.ASTFactory factory = ASTFactoryFactory.getASTFactory();
			
			org.rascalmpl.ast.Statement body = this.getBody().__evaluate(__eval);
			return factory.makeStatementDoWhile(this.getTree(),
					factory.makeLabelEmpty(this.getTree()),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStats(), body, this.getPostStats()),
					this.getCondition());

		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

	}

	static public class While extends org.rascalmpl.ast.StringTemplate.While {

		public While(org.eclipse.imp.pdb.facts.INode __param1,
				org.rascalmpl.ast.Expression __param2,
				java.util.List<org.rascalmpl.ast.Statement> __param3,
				org.rascalmpl.ast.StringMiddle __param4,
				java.util.List<org.rascalmpl.ast.Statement> __param5) {
			super(__param1, __param2, __param3, __param4, __param5);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.ast.Statement __evaluate(
				org.rascalmpl.interpreter.StringTemplateConverter.Visitor __eval) {

			org.rascalmpl.ast.ASTFactory factory = ASTFactoryFactory.getASTFactory();

			org.rascalmpl.ast.Statement body = this.getBody().__evaluate(__eval);
			return factory.makeStatementWhile(this.getTree(),
					factory.makeLabelEmpty(this.getTree()),
					java.util.Collections.singletonList(this.getCondition()),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStats(), body, this.getPostStats()));

		}

	}

	static public class IfThenElse extends
			org.rascalmpl.ast.StringTemplate.IfThenElse {

		public IfThenElse(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.Expression> __param2,
				java.util.List<org.rascalmpl.ast.Statement> __param3,
				org.rascalmpl.ast.StringMiddle __param4,
				java.util.List<org.rascalmpl.ast.Statement> __param5,
				java.util.List<org.rascalmpl.ast.Statement> __param6,
				org.rascalmpl.ast.StringMiddle __param7,
				java.util.List<org.rascalmpl.ast.Statement> __param8) {
			super(__param1, __param2, __param3, __param4, __param5, __param6,
					__param7, __param8);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.ast.Statement __evaluate(
				org.rascalmpl.interpreter.StringTemplateConverter.Visitor __eval) {

			org.rascalmpl.ast.ASTFactory factory = ASTFactoryFactory.getASTFactory();

			org.rascalmpl.ast.Statement t = this.getThenString().__evaluate(__eval);
			org.rascalmpl.ast.Statement e = this.getElseString().__evaluate(__eval);
			return factory.makeStatementIfThenElse(this.getTree(),
					factory.makeLabelEmpty(this.getTree()), this
							.getConditions(),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStatsThen(), t, this
									.getPostStatsThen()),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStatsElse(), e, this
									.getPostStatsElse()));

		}

	}

	static public class Ambiguity extends
			org.rascalmpl.ast.StringTemplate.Ambiguity {

		public Ambiguity(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.StringTemplate> __param2) {
			super(__param1, __param2);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

	}

	static public class For extends org.rascalmpl.ast.StringTemplate.For {

		public For(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.Expression> __param2,
				java.util.List<org.rascalmpl.ast.Statement> __param3,
				org.rascalmpl.ast.StringMiddle __param4,
				java.util.List<org.rascalmpl.ast.Statement> __param5) {
			super(__param1, __param2, __param3, __param4, __param5);
		}

		@Override
		public org.rascalmpl.ast.Statement __evaluate(
				org.rascalmpl.interpreter.StringTemplateConverter.Visitor __eval) {

			org.rascalmpl.ast.ASTFactory factory = ASTFactoryFactory.getASTFactory();

			org.rascalmpl.ast.Statement body = this.getBody().__evaluate(__eval);
			return factory.makeStatementFor(this.getTree(),
					factory.makeLabelEmpty(this.getTree()), this
							.getGenerators(),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStats(), body, this.getPostStats()));

		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

	}

	static public class IfThen extends org.rascalmpl.ast.StringTemplate.IfThen {

		public IfThen(org.eclipse.imp.pdb.facts.INode __param1,
				java.util.List<org.rascalmpl.ast.Expression> __param2,
				java.util.List<org.rascalmpl.ast.Statement> __param3,
				org.rascalmpl.ast.StringMiddle __param4,
				java.util.List<org.rascalmpl.ast.Statement> __param5) {
			super(__param1, __param2, __param3, __param4, __param5);
		}

		@Override
		public <T> T __evaluate(org.rascalmpl.ast.NullASTVisitor<T> __eval) {
			return null;
		}

		@Override
		public org.rascalmpl.ast.Statement __evaluate(
				org.rascalmpl.interpreter.StringTemplateConverter.Visitor __eval) {

			org.rascalmpl.ast.ASTFactory factory = ASTFactoryFactory.getASTFactory();

			org.rascalmpl.ast.Statement body = this.getBody().__evaluate(__eval);
			return factory.makeStatementIfThen(this.getTree(),
					factory.makeLabelEmpty(this.getTree()), this
							.getConditions(),
					org.rascalmpl.interpreter.StringTemplateConverter.Visitor
							.combinePreBodyPost(this.getTree(), this
									.getPreStats(), body, this.getPostStats()),
					null);

		}

	}
}