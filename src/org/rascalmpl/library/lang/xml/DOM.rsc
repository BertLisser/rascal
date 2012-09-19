@license{
  Copyright (c) 2009-2011 CWI
  All rights reserved. This program and the accompanying materials
  are made available under the terms of the Eclipse Public License v1.0
  which accompanies this distribution, and is available at
  http://www.eclipse.org/legal/epl-v10.html
}
@contributor{Tijs van der Storm - Tijs.van.der.Storm@cwi.nl}
@contributor{Mark Hills - Mark.Hills@cwi.nl (CWI)}
@contributor{Paul Klint - Paul.Klint@cwi.nl (CWI)}

@doc{
Synopsis: Functions for reading and writing XML files.
Description:
[XML](http://en.wikipedia.org/wiki/XML) is a widely used markup language for encoding and exchanging documents.

The Document Object Model [DOM](http://en.wikipedia.org/wiki/Document_Object_Model) is a cross-platform and language-independent
way of representing and manipulating HTML, XHTML and XML documents.

* An optional header line consisting of field names separated by comma's.
* One or more lines consisting of values separated by comma's.

The following functions are provided:
//<toc Rascal/Library/lang/xml 1>
}

module lang::xml::DOM

@doc{
Synopsis: Datatypes for representing an instance of the DOM.
}

data Node 
     = document(Node root)
     | attribute(Namespace namespace, str name, str text)
	 | element(Namespace namespace, str name, list[Node] children)
	 | charData(str text)
	 | cdata(str text)
	 | comment(str text)
	 | pi(str target, str text)
	 | entityRef(str name)
	 | charRef(int code)
	 ;  
		  
data Namespace 
     = namespace(str prefix, str uri)
     | none()
     ;

@doc{
Synopsis: Auxiliary constructor for XML attribute without namespace.
}
public Node attribute(str name, str text) = attribute(none(), name, text);

@doc{
Synopsis: Auxiliary constructor for XML element without namespace.
}
public Node element(str name, list[Node] kids) = element(none(), name, kids);


@doc{
Synopsis: Parse an XML document and return a DOM instance.

Description:

Examples:
Read the sample note file, parse it, and construct a DOM instance.
<screen>
import IO;
import lang::xml::DOM;
N = readFile(|courses:///Rascal/Libraries/lang/xml/note.xml|);
parseXMLDOM(N);
</screen>
The DOM instance contains every single character (including spaces and newlines)
as they appear in the source file.
As expected, the result is of type [xml/DOM/Node].
}
@javaClass{org.rascalmpl.library.lang.xml.DOM}
public java Node parseXMLDOM(str src);

@doc{
Synopsis: Parse an XML document and trim it (remove layout).

Examples:

Read the sample note file, parse it, and construct a DOM instance (using `parseXMLDOMTrim`).
<screen>
import IO;
import lang::xml::DOM;
N = readFile(|courses:///Rascal/Libraries/lang/xml/note.xml|);
parseXMLDOMTrim(N);
</screen>
All whitespace characters have been removed and do not occur in the trimmed DOM instance.
Compare this with the output of [parseXMLDOM].
}
@javaClass{org.rascalmpl.library.lang.xml.DOM}
public java Node parseXMLDOMTrim(str src);

@doc{
Synopsis: Convert a DOM instance to a raw XML string.

Examples:
Read the sample note file, parse it, construct a DOM instance, and convert it to a string:
<screen>
import IO;
import lang::xml::DOM;
F = readFile(|courses:///Rascal/Libraries/lang/xml/note.xml|);
println(F);
S = xmlRaw(parseXMLDOM(F));
println(S);
</screen>
Apart from an extra XML header, the original source file `F` and the output `S` of `xmlRaw` are identical.
}
@javaClass{org.rascalmpl.library.lang.xml.DOM}
public java str xmlRaw(Node x);

@doc{
Synopsis: Convert a DOM instance to a compact XML string (with minimal white space).

Examples:
Read the sample note file, parse it, construct a DOM instance, and convert it to a string:
<screen>
import IO;
import lang::xml::DOM;
F = readFile(|courses:///Rascal/Libraries/lang/xml/note.xml|);
println(F);
S = xmlCompact(parseXMLDOM(F));
println(S);
</screen>
The output `S` of `xmlCompact` is a version of the original source file `F` with all white space removed.
}
@javaClass{org.rascalmpl.library.lang.xml.DOM}
public java str xmlCompact(Node x);

@doc{
Synopsis: Convert a DOM instance to a pretty printed XML string.

Examples:
Read the sample note file, parse it, construct a DOM instance, and convert it to a string:
<screen>
import IO;
import lang::xml::DOM;
F = readFile(|courses:///Rascal/Libraries/lang/xml/note.xml|);
println(F);
S = xmlPretty(parseXMLDOM(F));
println(S);
</screen>
The output `S` of `xmlPretty` is a pretty printed version of the original source file `F`.
Observe that the elements inside `<note> ... </note>` are indented.
}
@javaClass{org.rascalmpl.library.lang.xml.DOM}
public java str xmlPretty(Node x);




