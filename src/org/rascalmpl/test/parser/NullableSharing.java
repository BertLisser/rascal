package org.rascalmpl.test.parser;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.io.StandardTextReader;
import org.rascalmpl.parser.gtd.SGTDBF;
import org.rascalmpl.parser.gtd.stack.AbstractStackNode;
import org.rascalmpl.parser.gtd.stack.EpsilonStackNode;
import org.rascalmpl.parser.gtd.stack.NonTerminalStackNode;
import org.rascalmpl.values.ValueFactoryFactory;
import org.rascalmpl.values.uptr.Factory;

/*
* S ::= N N
* N ::= A
* A ::= epsilon
*/
public class NullableSharing extends SGTDBF implements IParserTest{
	private final static IConstructor SYMBOL_START_S = VF.constructor(Factory.Symbol_Sort, VF.string("S"));
	private final static IConstructor SYMBOL_A = VF.constructor(Factory.Symbol_Sort, VF.string("A"));
	private final static IConstructor SYMBOL_N = VF.constructor(Factory.Symbol_Sort, VF.string("N"));
	private final static IConstructor SYMBOL_empty = VF.constructor(Factory.Symbol_Empty);
	
	private final static IConstructor PROD_S_NN = VF.constructor(Factory.Production_Default, VF.list(SYMBOL_N, SYMBOL_N), SYMBOL_START_S, VF.constructor(Factory.Attributes_NoAttrs));
	private final static IConstructor PROD_N_A = VF.constructor(Factory.Production_Default, VF.list(SYMBOL_A), SYMBOL_N, VF.constructor(Factory.Attributes_NoAttrs));
	private final static IConstructor PROD_A_empty = VF.constructor(Factory.Production_Default, VF.list(SYMBOL_empty), SYMBOL_A, VF.constructor(Factory.Attributes_NoAttrs));
	
	private final static AbstractStackNode NONTERMINAL_START_S = new NonTerminalStackNode(AbstractStackNode.START_SYMBOL_ID, 0, "S");
	private final static AbstractStackNode NONTERMINAL_A0 = new NonTerminalStackNode(0, 0, "A");
	private final static AbstractStackNode NONTERMINAL_N1 = new NonTerminalStackNode(1, 0, "N");
	private final static AbstractStackNode NONTERMINAL_N2 = new NonTerminalStackNode(2, 1, "N");
	private final static AbstractStackNode EPSILON3 = new EpsilonStackNode(3, 0);
	
	public NullableSharing(){
		super();
	}
	
	public void S(){
		expect(PROD_S_NN, NONTERMINAL_N1, NONTERMINAL_N2);
	}
	
	public void A(){
		expect(PROD_A_empty, EPSILON3);
	}
	
	public void N(){
		expect(PROD_N_A, NONTERMINAL_A0);
	}
	
	public IConstructor executeParser(){
		return parse(NONTERMINAL_START_S, null, "".toCharArray());
	}
	
	public IValue getExpectedResult() throws IOException{
		String expectedInput = "appl(prod([sort(\"N\"),sort(\"N\")],sort(\"S\"),\\no-attrs()),[appl(prod([sort(\"A\")],sort(\"N\"),\\no-attrs()),[appl(prod([empty()],sort(\"A\"),\\no-attrs()),[])]),appl(prod([sort(\"A\")],sort(\"N\"),\\no-attrs()),[appl(prod([empty()],sort(\"A\"),\\no-attrs()),[])])])";
		return new StandardTextReader().read(ValueFactoryFactory.getValueFactory(), Factory.uptr, Factory.Tree, new ByteArrayInputStream(expectedInput.getBytes()));
	}
	
	public static void main(String[] args){
		NullableSharing ns = new NullableSharing();
		IConstructor result = ns.parse(NONTERMINAL_START_S, null, "".toCharArray());
		System.out.println(result);
		
		System.out.println("S(N(A()),N(A())) <- good");
	}
}
