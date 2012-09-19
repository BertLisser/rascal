/*******************************************************************************
 * Copyright (c) 2009-2011 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI
 *   * Anya Helene Bagge - anya@ii.uib.no (Univ. Bergen)
 *   * Paul Klint - Paul.Klint@cwi.nl - CWI
 *   * Arnold Lankamp - Arnold.Lankamp@cwi.nl
*******************************************************************************/
package org.rascalmpl.interpreter.staticErrors;

import org.eclipse.imp.pdb.facts.ISourceLocation;
import org.rascalmpl.ast.AbstractAST;

/**
 * A static error represents all errors that are detected by the interpreter
 * in 'static type check' mode. These errors can NOT be caught in Rascal code.
 * In the future, they may be thrown by an actual type checker.
 */
public abstract class StaticError extends RuntimeException {
	private static final long serialVersionUID = 2730379050952564623L;
	private ISourceLocation loc;
	
	public StaticError(String message, ISourceLocation loc) {
		super(message);
		if(loc != null)
			addStackTraceElement(loc);
		this.loc = loc;
	}
	
	public StaticError(String message, ISourceLocation loc, Throwable cause) {
		super(message, cause);
		if(loc != null)
			addStackTraceElement(loc);
		this.loc = loc;
	}

	public StaticError(String message, AbstractAST ast) {
		this(message, ast != null ? ast.getLocation() : null);
	}
	
	public StaticError(String message, AbstractAST ast, Throwable cause) {
		this(message, ast != null ? ast.getLocation() : null, cause);
	}
	
	public ISourceLocation getLocation() {
		return loc;
	}
	
	public void setLocation(ISourceLocation loc) {
		String mod = loc.getURI().getPath().replaceAll("^.*/", "").replaceAll("\\..*$", "");
		getStackTrace()[0] = new StackTraceElement(mod, "?", loc.getURI().getPath(), loc.getBeginLine());

		this.loc = loc;
	}

	private void addStackTraceElement(ISourceLocation loc) {
		StackTraceElement[] oldStackTrace = getStackTrace();
		StackTraceElement[] stackTrace = new StackTraceElement[oldStackTrace.length+1];
		int i = 0;
		
		String mod = loc.getURI().getPath().replaceAll("^.*/", "").replaceAll("\\..*$", "");
		stackTrace[i++] = new StackTraceElement(mod, "?", loc.getURI().getPath(), loc.getBeginLine());

		for(StackTraceElement elt : oldStackTrace)
			stackTrace[i++] = elt;
		setStackTrace(stackTrace);
	}
}
