@license{
  Copyright (c) 2009-2011 CWI
  All rights reserved. This program and the accompanying materials
  are made available under the terms of the Eclipse Public License v1.0
  which accompanies this distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html
}
@contributor{Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI}
@bootstrapParser
module lang::rascal::grammar::definition::Modules

import lang::rascal::\syntax::RascalRascal;
import lang::rascal::grammar::definition::Productions;
import lang::rascal::grammar::definition::Layout;
import lang::rascal::grammar::definition::Literals;
import lang::rascal::grammar::definition::Names;
import Grammar;
import Set;

@doc{Converts internal module representation of Rascal interpreter to single grammar definition}
public Grammar modules2grammar(str main, map[str name, tuple[set[str] imports, set[str] extends, set[SyntaxDefinition] defs] \mod] mods) {
  // note that we ignore extends here because they're resolved by the interpreter at the moment by 
  // cloning definitions into the module that extends.
  def = \definition(main, (m:\module(m, 
                                    mods[m].imports, 
                                    mods[m].extends, 
                                    syntax2grammar(mods[m].defs)
                                    ) 
                          | m <- mods));
  return fuse(layouts(resolve(def)));
}

@doc{Converts concrete syntax definitions and fuses them into one single grammar definition}     
public Grammar modules2grammar(str main, set[Module] modules) {
  return fuse(layouts(resolve(modules2definition(main, modules))));
}

@doc{Converts concrete syntax definitions to abstract grammar definitions}
public GrammarDefinition modules2definition(str main, set[Module] modules) {
  return \definition(main, (\mod.name:\mod | m <- modules, \mod := module2grammar(m)));
}

@doc{
  Combines a set of modules into one big Grammar, projecting only the rules that
  are visible locally, or via import and extend.
}
public Grammar fuse(GrammarDefinition def) {
  result = grammar({},());
  todo = {def.main};
  done = {};
  
  while (todo != {}) {
    <name,todo> = takeOneFrom(todo);
    \mod = def.modules[name];
    done += name; 
    result = (compose(result, \mod.grammar) | compose(it, def.modules[i].grammar) | i <- \mod.imports + \mod.extends);
    todo += (\mod.extends - done);
  }
  
  return result;
}
 


public GrammarModule module2grammar(Module \mod) {
  <name, imps, exts> = getModuleMetaInf(\mod);
  return \module(name, imps, exts, syntax2grammar(collect(\mod)));
} 

public tuple[str, set[str], set[str]] getModuleMetaInf(\mod) {
  // TODO: implement module type parameters
  // Tags tags "module" QualifiedName name ModuleParameters params Import* imports
  switch (\mod) {
    case (Module) `<Tags _> module <QualifiedName name> <ModuleParameters _> <Import* is> <Body _>` :
    return <deslash("<name>"), { "<i>" | (Import) `import <QualifiedName  i>;` <- is } 
                    , { "<i>" | (Import) `extend <QualifiedName  i>;` <- is }>; 
    case (Module) `<Tags _> module <QualifiedName name> <Import* is> <Body _>`:
    return <deslash("<name>"), { "<i>" | (Import) `import <QualifiedName  i>;` <- is } 
                    , { "<i>" | (Import) `extend <QualifiedName  i>;` <- is }>; 
  }
  
  throw "unexpected module syntax <\mod>";
} 
 
str deslash(str input) {
  return visit(input) {
    case /\\/ => ""
  }
}

public Grammar imports2grammar(set[Import] imports) {
  return syntax2grammar({ s | (Import) `<SyntaxDefinition s>` <- imports});
}
 
private set[SyntaxDefinition] collect(Module \mod) {
  set[SyntaxDefinition] result = {};
  
  top-down-break visit (\mod) {
    case SyntaxDefinition s : result += s; 
    case Body b => b
  }
  return result;
}  
