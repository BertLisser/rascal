package org.rascalmpl.parser.gtd.result.struct;

import org.rascalmpl.parser.gtd.result.AbstractNode;
import org.rascalmpl.parser.gtd.util.ArrayList;

public class Link{
	public final ArrayList<Link> prefixes;
	public final AbstractNode node;
	
	public Link(ArrayList<Link> prefixes, AbstractNode node){
		super();
		
		this.prefixes = prefixes;
		this.node = node;
	}
}