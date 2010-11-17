package org.rascalmpl.library.vis.graph.spring;

import org.eclipse.imp.pdb.facts.IString;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;
import org.rascalmpl.library.vis.Figure;
import org.rascalmpl.library.vis.FigurePApplet;
import org.rascalmpl.library.vis.PropertyManager;
import org.rascalmpl.library.vis.graph.spring.SpringGraph;

import processing.core.PApplet;

/**
 * A GraphEdge is created for each "edge" constructor that occurs in a graph.
 * 
 * @author paulk
 *
 */
public class SpringGraphEdge extends Figure {
	private SpringGraphNode from;
	private SpringGraphNode to;
	private boolean inverted = false;
	private static boolean debug = false;
	
	public SpringGraphEdge(SpringGraph G, FigurePApplet fpa, PropertyManager properties, IString fromName, IString toName, IEvaluatorContext ctx) {
		super(fpa, properties, ctx);
		this.from = G.getRegistered(fromName.getValue());
		
		if(getFrom() == null){
			throw RuntimeExceptionFactory.figureException("No node with id property + \"" + fromName.getValue() + "\"",
					fromName, ctx.getCurrentAST(), ctx.getStackTrace());
		}
		to = G.getRegistered(toName.getValue());
		if(to == null){
			throw RuntimeExceptionFactory.figureException("No node with id property + \"" + toName.getValue() + "\"", toName, ctx.getCurrentAST(), ctx.getStackTrace());
		}
		
		if(debug)System.err.println("edge: " + fromName.getValue() + " -> " + toName.getValue());
	}
	

	SpringGraphNode getFrom() {
		return inverted ? to : from;
	}

	SpringGraphNode getTo() {
		return inverted? from : to;
	}

	void invert(){
		inverted = true;
	}
	
	boolean isInverted(){
		return inverted;
	}
	
	void relax(SpringGraph G){
		float vx = to.xdistance(getFrom());
		float vy = to.ydistance(getFrom());
		
		float dlen = PApplet.mag(vx, vy);
		dlen = (dlen == 0) ? .0001f : dlen;

		//float attract = G.attract(dlen);
		float attract = dlen * dlen / G.springConstant;
		float dx = (vx / dlen) * attract;
		float dy = (vy / dlen) * attract;

		to.dispx += -dx;
		to.dispy += -dy;
		getFrom().dispx += dx;
		getFrom().dispy += dy;
		
		if(debug)System.err.printf("edge: %s -> %s: dx=%f, dy=%f\n", getFrom().name, to.name, dx, dy);
	}

	@Override
	public
	void draw(float left, float top) {
		applyProperties();
		if(debug) System.err.println("edge: (" + getFrom().name + ": " + getFrom().x + "," + getFrom().y + ") -> (" + 
				to.name + ": " + to.x + "," + to.y + ")");

		fpa.line(left + getFrom().figX(), top + getFrom().figY(), 
				left + getTo().figX(), top + getTo().figY());
	}

	@Override
	public
	void bbox() {
		// TODO Auto-generated method stub
		
	}

}