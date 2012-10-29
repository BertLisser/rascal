/*******************************************************************************
 * Copyright (c) 2009-2012 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *   * Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI
 *   * Tijs van der Storm - Tijs.van.der.Storm@cwi.nl
 *   * Paul Klint - Paul.Klint@cwi.nl - CWI
 *   * Mark Hills - Mark.Hills@cwi.nl (CWI)
 *   * Arnold Lankamp - Arnold.Lankamp@cwi.nl
 *   * Michael Steindorfer - Michael.Steindorfer@cwi.nl - CWI
 *******************************************************************************/
package org.rascalmpl.ast;


import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IValue;
import org.rascalmpl.interpreter.IEvaluator;
import org.rascalmpl.interpreter.asserts.Ambiguous;
import org.rascalmpl.interpreter.env.Environment;
import org.rascalmpl.interpreter.result.Result;

public abstract class Tags extends AbstractAST {
  public Tags(IConstructor node) {
    super();
  }

  
  public boolean hasTags() {
    return false;
  }

  public java.util.List<org.rascalmpl.ast.Tag> getTags() {
    throw new UnsupportedOperationException();
  }

  static public class Ambiguity extends Tags {
    private final java.util.List<org.rascalmpl.ast.Tags> alternatives;
    private final IConstructor node;
           
    public Ambiguity(IConstructor node, java.util.List<org.rascalmpl.ast.Tags> alternatives) {
      super(node);
      this.node = node;
      this.alternatives = java.util.Collections.unmodifiableList(alternatives);
    }
    
    @Override
    public IConstructor getTree() {
      return node;
    }
  
    @Override
    public AbstractAST findNode(int offset) {
      return null;
    }
  
    @Override
    public Result<IValue> interpret(IEvaluator<Result<IValue>> __eval) {
      throw new Ambiguous(src);
    }
      
    @Override
    public org.eclipse.imp.pdb.facts.type.Type typeOf(Environment env) {
      throw new Ambiguous(src);
    }
    
    public java.util.List<org.rascalmpl.ast.Tags> getAlternatives() {
      return alternatives;
    }
    
    public <T> T accept(IASTVisitor<T> v) {
    	return v.visitTagsAmbiguity(this);
    }
  }

  

  
  public boolean isDefault() {
    return false;
  }

  static public class Default extends Tags {
    // Production: sig("Default",[arg("java.util.List\<org.rascalmpl.ast.Tag\>","tags")])
  
    
    private final java.util.List<org.rascalmpl.ast.Tag> tags;
  
    public Default(IConstructor node , java.util.List<org.rascalmpl.ast.Tag> tags) {
      super(node);
      
      this.tags = tags;
    }
  
    @Override
    public boolean isDefault() { 
      return true; 
    }
  
    @Override
    public <T> T accept(IASTVisitor<T> visitor) {
      return visitor.visitTagsDefault(this);
    }
  
    
    @Override
    public java.util.List<org.rascalmpl.ast.Tag> getTags() {
      return this.tags;
    }
  
    @Override
    public boolean hasTags() {
      return true;
    }	
  }
}