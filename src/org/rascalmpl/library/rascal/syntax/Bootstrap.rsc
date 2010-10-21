@bootstrapParser
module rascal::syntax::Bootstrap

import rascal::syntax::Grammar2Rascal;
import rascal::syntax::Definition;
import rascal::syntax::Grammar;
import rascal::syntax::Generator;
import rascal::syntax::RascalRascal; // for new parser
import ParseTree;
import IO;
import ValueIO;

private str package = "org.rascalmpl.library.rascal.syntax";
private loc folder = |project://RascalLibrary/src/rascal/syntax|;
private str grammarName = "RascalRascal";
private str rootName = "RascalRascal";
private str objectName = "ObjectRascalRascal";
private str metaName = "MetaRascalRascal";

public void bootstrap() {
  gr = getRascalGrammar();
  bootRootParser(gr);
  bootObjectParser(gr);
  bootMetaParser(gr);
}

public Grammar getRascalGrammar() {
  println("parsing the rascal definition of rascal");
  Module \module = parse(#Module, folder + "/<grammarName>.rsc");
  println("imploding the syntax definition and normalizing and desugaring it");
  return module2grammar(\module);
}

public void bootRootParser(Grammar gr) {
  println("generating root parser");
  str source = generateRootParser(package,rootName, gr);
  println("writing rascal root parser");
  writeFile(folder + "/<rootName>.java", source);
}

public void bootObjectParser(Grammar gr) {
  println("generating rascal object parser");
  source = generateObjectParser(package, objectName, gr);
  println("writing rascal object parser");
  writeFile(folder + "/<objectName>.java", source);
}

public void bootMetaParser(Grammar gr) {
  println("generating assimilated rascal for rascal parser");
  gr.rules[sort("RascalReservedKeywords")] = {};
  source = generateMetaParser(package, metaName, objectName, gr);
  println("writing assimilated parser");
  writeFile(folder + "/<metaName>.java", source);
}