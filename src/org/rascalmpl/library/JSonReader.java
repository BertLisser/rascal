/*******************************************************************************
 * Copyright (c) INRIA-LORIA and CWI 2006-2009 
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Jurgen Vinju (jurgenv@cwi.nl) - initial API and implementation

 *******************************************************************************/
package org.rascalmpl.library;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.ITuple;
import org.eclipse.imp.pdb.facts.IListWriter;
import org.eclipse.imp.pdb.facts.IMap;
import org.eclipse.imp.pdb.facts.IMapWriter;
import org.eclipse.imp.pdb.facts.ISetWriter;
import org.eclipse.imp.pdb.facts.IString;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.exceptions.FactParseError;
import org.eclipse.imp.pdb.facts.exceptions.IllegalOperationException;
import org.eclipse.imp.pdb.facts.io.AbstractBinaryReader;
import org.eclipse.imp.pdb.facts.type.Type;
import org.eclipse.imp.pdb.facts.type.TypeFactory;
import org.eclipse.imp.pdb.facts.type.TypeStore;

// TODO: add support for values of type Value, for this we need overloading resolving
public class JSonReader extends AbstractBinaryReader {
	private IValueFactory vf;
	private TypeFactory tf = TypeFactory.getInstance();
	private TypeStore ts;

	private IString nameKey, argKey;

	final boolean debug = true;

	public IValue read(IValueFactory factory, TypeStore store, Type type,
			InputStream stream) throws FactParseError, IOException {
		this.vf = factory;
		this.ts = store;
		nameKey = (IString) tf.stringType().make(vf, "name");
		argKey = (IString) tf.stringType().make(vf, "args");
		int firstToken;
		do {
			firstToken = stream.read();
			if (firstToken == -1) {
				throw new IOException("Premature EOF.");
			}
		} while (Character.isWhitespace((char) firstToken));

		char typeByte = (char) firstToken;
		if (Character.isLetterOrDigit(typeByte) || typeByte == '_'
				|| typeByte == '[' || typeByte == '{') {
			SharingStream sreader = new SharingStream(stream);
			sreader.last_char = typeByte;
			IValue result = parse(sreader, type);
			System.err.println("PARSED:"+result);
			if (type.isAbstractDataType()) {
				result = buildTerm((IMap) result, type);
			} else
				result = buildTerm(result, type);
			return result;
		} else {
			throw new RuntimeException("nyi");
		}
	}

	// TODO add support for anonymous constructors (is already done for the
	// parseNumber case)
	private IValue parse(SharingStream reader, Type expected)
			throws IOException {
		IValue result;
		int start, end;
		System.err.println("Parse:" + expected + " " + reader.getLastChar());
		start = reader.getPosition();
		switch (reader.getLastChar()) {
		case -1:
			throw new FactParseError("premature EOF encountered.", start);
		case '{':
			result = parseMap(reader, expected);
			break;
		case '[':
			if (expected.isTupleType()) {
				result = parseTuple(reader, expected);
			} else
				result = parseList(reader, expected);
			break;
		case '"':
			result = parseString(reader, expected);
			break;
		case '-':
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			result = parseNumber(reader, expected);
			break;
		default:
			throw new FactParseError("Illegal symbol", reader.getPosition());
		}

		if (reader.getLastChar() == '{') {
			result = parseAnnotations(reader, result);
		}

		end = reader.getPosition();
		reader.storeNextTerm(result, end - start);

		return result;
	}

	private IValue parseAnnotations(SharingStream reader, IValue result)
			throws IOException {
		if (reader.readSkippingWS() == '}') {
			reader.readSkippingWS();
		} else {
			result = parseAnnos(reader, result);
			if (reader.getLastChar() != '}') {
				throw new FactParseError("'}' expected", reader.getPosition());
			}
		}
		return result;
	}

	private IValue parseTuple(SharingStream reader, Type expected)
			throws IOException {
		final int c = reader.readSkippingWS();
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		IValue[] a = new IValue[expected.getArity()];
		Type[] b = new Type[expected.getArity()];
		Iterator<Type> it = expected.iterator();
		int i = 0;
		if (it.hasNext()) {
			Type typ = it.next();
			IValue term = parse(reader, typ);
			a[i] = term;
			b[i] = term.getType();
			System.err.println("ParseTuple:"+a[i]+" "+b[i]+" "+i);
			i++;
		}
		while (reader.getLastChar() == ',' && it.hasNext()) {
			reader.readSkippingWS();
			IValue term = parse(reader, it.next());
			a[i] = term;
			b[i] = term.getType();
			System.err.println("ParseTuple:"+a[i]+" "+b[i]+" "+i);
			i++;
		}
		IValue result = tf.tupleType(b).make(vf, a);
		System.err.println("result="+result);
		if (reader.getLastChar() != ']') {
			throw new FactParseError("expected ']' but got '"
					+ (char) reader.getLastChar() + "'", reader.getPosition());
		}
		reader.readSkippingWS();
		return result;
	}

	private IValue[] parseEntry(SharingStream reader, Type mapType)
			throws IOException {
		IValue[] array = new IValue[2];
		array[0] = parse(reader, mapType.getKeyType());
		if (reader.getLastChar() == ':') {
			reader.readSkippingWS();
			array[1] = parse(reader, mapType.getValueType());
		} else
			throw new FactParseError("In map ':' expected",
					reader.getPosition());
		return array;
	}

	private IValue parseMap(SharingStream reader, Type expected)
			throws IOException {
		final int c = reader.readSkippingWS();
		if (!expected.isMapType())
			expected = tf.mapType(tf.stringType(), tf.valueType());
		IMapWriter w = expected.writer(vf);
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		IValue[] term = parseEntry(reader, expected);
		w.put(term[0], term[1]);
		while (reader.getLastChar() == ',') {
			reader.readSkippingWS();
			term = parseEntry(reader, expected);
			w.put(term[0], term[1]);

		}
		if (reader.getLastChar() != '}') {
			throw new FactParseError("expected '}' but got '"
					+ (char) reader.getLastChar() + "'", reader.getPosition());
		}
		reader.readSkippingWS();
		return w.done();
	}

	private IValue parseString(SharingStream reader, Type expected)
			throws IOException {
		int c;
		IValue result;
		String str = parseStringLiteral(reader);

		// note that we interpret all strings as strings, not possible function
		// names.
		// this deviates from the ATerm library.

		result = expected.make(vf, str);
		c = reader.readSkippingWS(); /* " */
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		return result;
	}

	private IValue parseList(SharingStream reader, Type expected)
			throws IOException {
		IValue result;
		final int c = reader.readSkippingWS();
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		if (c == ']') {
			reader.readSkippingWS();
			if (expected.isListType()) {
				result = expected.make(vf);
			} else if (expected.isValueType()) {
				result = tf.listType(tf.valueType()).make(vf);
			} else {
				throw new FactParseError("Did not expect a list, rather a "
						+ expected, reader.getPosition());
			}
		} else {
			result = parseTerms(reader, expected);
			if (reader.getLastChar() != ']' && reader.getLastChar() != '}') {
				throw new FactParseError("expected ']' or '}' but got '"
						+ (char) reader.getLastChar() + "'",
						reader.getPosition());
			}
			reader.readSkippingWS();
		}
		return result;
	}

	private IValue parseAnnos(SharingStream reader, IValue result)
			throws IOException {
		result = parseAnno(reader, result);
		while (reader.getLastChar() == ',') {
			reader.readSkippingWS();
			result = parseAnno(reader, result);
		}

		return result;
	}

	private IValue parseAnno(SharingStream reader, IValue result)
			throws IOException {
		if (reader.getLastChar() == '[') {
			int c = reader.readSkippingWS();

			if (c == '"') {
				String key = parseStringLiteral(reader);
				Type annoType = ts.getAnnotationType(result.getType(), key);

				if (reader.readSkippingWS() == ',') {
					reader.readSkippingWS();
					IValue value = parse(reader, annoType);

					if (result.getType().isConstructorType()
							|| result.getType().isAbstractDataType()) {
						result = ((IConstructor) result).setAnnotation(key,
								value);
					}

					if (reader.getLastChar() != ']') {
						throw new FactParseError("expected a ] but got a "
								+ reader.getLastChar(), reader.getPosition());
					}

					reader.readSkippingWS();
					return result;
				}

				throw new FactParseError(
						"expected a comma before the value of the annotation",
						reader.getPosition());
			}

			throw new FactParseError("expected a label for an annotation",
					reader.getPosition());
		}

		// no annotations
		return result;
	}

	private IValue parseNumber(SharingStream reader, Type expected)
			throws IOException {
		StringBuilder str = new StringBuilder();
		IValue result;
		do {
			str.append((char) reader.getLastChar());
		} while (Character.isDigit(reader.read()));

		if (reader.getLastChar() != '.' && reader.getLastChar() != 'e'
				&& reader.getLastChar() != 'E' && reader.getLastChar() != 'l'
				&& reader.getLastChar() != 'L') {
			int val;
			try {
				val = Integer.parseInt(str.toString());
			} catch (NumberFormatException e) {
				throw new FactParseError("malformed int:" + str,
						reader.getPosition());
			}

			result = expected.make(vf, ts, val);
		} else if (reader.getLastChar() == 'l' || reader.getLastChar() == 'L') {
			reader.read();
			throw new FactParseError("No support for longs",
					reader.getPosition());
		} else {
			if (reader.getLastChar() == '.') {
				str.append('.');
				reader.read();
				if (!Character.isDigit(reader.getLastChar()))
					throw new FactParseError("digit expected",
							reader.getPosition());
				do {
					str.append((char) reader.getLastChar());
				} while (Character.isDigit(reader.read()));
			}
			if (reader.getLastChar() == 'e' || reader.getLastChar() == 'E') {
				str.append((char) reader.getLastChar());
				reader.read();
				if (reader.getLastChar() == '-' || reader.getLastChar() == '+') {
					str.append((char) reader.getLastChar());
					reader.read();
				}
				if (!Character.isDigit(reader.getLastChar()))
					throw new FactParseError("digit expected!",
							reader.getPosition());
				do {
					str.append((char) reader.getLastChar());
				} while (Character.isDigit(reader.read()));
			}
			double val;
			try {
				val = Double.valueOf(str.toString()).doubleValue();
				result = expected.make(vf, ts, val);
			} catch (NumberFormatException e) {
				throw new FactParseError("malformed real",
						reader.getPosition(), e);
			}
		}
		reader.skipWS();
		return result;
	}

	private String parseStringLiteral(SharingStream reader) throws IOException {
		boolean escaped;
		StringBuilder str = new StringBuilder();
		do {
			escaped = false;
			if (reader.read() == '\\') {
				reader.read();
				escaped = true;
			}
			int lastChar = reader.getLastChar();
			if (lastChar == -1)
				throw new IOException("Premature EOF.");
			if (escaped) {
				switch (lastChar) {
				case 'n':
					str.append('\n');
					break;
				case 't':
					str.append('\t');
					break;
				case 'b':
					str.append('\b');
					break;
				case 'r':
					str.append('\r');
					break;
				case 'f':
					str.append('\f');
					break;
				case '\\':
					str.append('\\');
					break;
				case '\'':
					str.append('\'');
					break;
				case '\"':
					str.append('\"');
					break;
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
					str.append(reader.readOct());
					break;
				default:
					str.append('\\').append((char) lastChar);
				}
			} else if (lastChar != '\"') {
				str.append((char) lastChar);
			}
		} while (escaped || reader.getLastChar() != '"');

		return str.toString();
	}

	private IValue buildTerm(IList t, Type type) {
		IValue[] a = new IValue[t.length()];
		Type[] b = new Type[t.length()];
		System.err.println("buildTermList");
		for (int i = 0; i < t.length(); i++) {
			System.err.println(t.get(i));
			a[i] = buildTerm(t.get(i), t.getElementType());
			b[i] = a[i].getType();
			System.err.println("R:" + a[i] + " " + b[i]);
		}
		if (type.isTupleType())
			return tf.tupleType(b).make(vf, a);
		return (tf.listType(t.isEmpty() ? t.getElementType() : b[0])
				.make(vf, a));
	}

	private IValue buildTerm(ITuple t, Type type) {
		IValue[] a = new IValue[t.arity()];
		Type[] b = new Type[t.arity()];
		System.err.println("buildTermTyple:"+type);
		for (int i = 0; i < t.arity(); i++) {
			a[i] = buildTerm(t.get(i), type.getFieldType(i));
			b[i] = a[i].getType();
			System.err.println("buildTermTyple:"+a[i]+" "+b[i]);
		}
		return tf.tupleType(b).make(vf, a);
	}

	private IValue buildTerm(IMap t, Type type) {
		IList rs = (IList) tf.listType(tf.valueType()).make(vf);
		final Iterator<IValue> args = ((IList) t.get(argKey)).iterator();

		final String funname = ((IString) t.get(nameKey)).getValue();
		while (args.hasNext()) {
			IValue arg = (IValue) args.next();
			arg = buildTerm(arg, type);
			rs = rs.append(arg);
		}
		IValue[] a = new IValue[rs.length()];
		Type[] b = new Type[rs.length()];
		int i = 0;
		for (IValue c : rs) {
			a[i] = c;
			b[i] = c.getType();
			i++;
		}
		if (funname.equals("tuple")) {
			return tf.tupleType(b).make(vf, a);
		}
		if (funname.equals("set")) {
			return tf.setType(b.length > 0 ? b[0] : tf.valueType()).make(vf, a);
		}
		Type types = tf.tupleType(b);
		if (debug)
			System.err.println("lookupFirstConstructor:" + funname + " "
					+ types);
		/* Local data types also searched - Monday */
		Type node = ts.lookupConstructor(type, funname, types);
		if (node == null)
			node = ts.lookupFirstConstructor(funname, types);
		System.err.println("node=" + node);
		if (node.isAliasType())
			node = node.getAliased();
		return node.make(vf, a);
	}

	private IValue buildTerm(IValue t, Type type) {
		System.err.println("BuildTerm:"+t+" "+type);
		if (t instanceof IMap)
			return buildTerm((IMap) t, type);
		if (t instanceof IList)
			return buildTerm((IList) t, type);
		if (t instanceof ITuple)
			return buildTerm((ITuple) t, type);
		return t;
	}

	private IValue parseTerms(SharingStream reader, Type expected)
			throws IOException {
		Type base = expected;
		Type elementType = getElementType(expected);
		IValue[] terms = parseTermsArray(reader, elementType);
		if (base.isListType() || base.isValueType()) {
			IListWriter w = expected.writer(vf);
			for (int i = terms.length - 1; i >= 0; i--) {
				w.insert(terms[i]);
			}
			return w.done();
		} else if (base.isSetType() || base.isRelationType()) {
			ISetWriter w = expected.writer(vf);
			w.insert(terms);
			return w.done();
		}
		throw new FactParseError("Unexpected type " + expected,
				reader.getPosition());
	}

	private Type getElementType(Type expected) {
		Type base = expected;
		if (base.isTupleType()) {
			return base;
		} else if (base.isRelationType()) {
			return base.getFieldTypes();
		} else if (base.isListType()) {
			return base.getElementType();
		} else if (base.isSetType()) {
			return base.getElementType();
		} else if (base.isMapType()) {
			return base;
			// return tf.tupleType(base.getKeyType(), base.getValueType());
		} else if (base.isAbstractDataType()) {
			return tf.tupleType(tf.stringType(), tf.valueType());
		} else if (base.isValueType()) {
			return base;
		} else {
			throw new IllegalOperationException("getElementType", expected);
		}
	}

	private IValue[] parseTermsArray(SharingStream reader, Type elementType)
			throws IOException {
		List<IValue> list = new ArrayList<IValue>(2);
		IValue term = parse(reader, elementType);
		list.add(term);
		while (reader.getLastChar() == ',') {
			reader.getLastChar();
			reader.readSkippingWS();
			term = parse(reader, elementType);
			list.add(term);
		}
		IValue[] array = new IValue[list.size()];
		ListIterator<IValue> iter = list.listIterator();
		int index = 0;
		while (iter.hasNext()) {
			array[index++] = iter.next();
		}
		return array;
	}

	class SharingStream {
		private static final int INITIAL_TABLE_SIZE = 2048;
		private static final int TABLE_INCREMENT = 4096;

		private static final int INITIAL_BUFFER_SIZE = 1024;

		private InputStream reader;

		int last_char;
		private int pos;

		private int nr_terms;
		private IValue[] table;

		private byte[] buffer;
		private int limit;
		private int bufferPos;

		public SharingStream(InputStream reader) {
			this(reader, INITIAL_BUFFER_SIZE);
		}

		public SharingStream(InputStream stream, int bufferSize) {
			this.reader = stream;
			last_char = -1;
			pos = 0;

			if (bufferSize < INITIAL_BUFFER_SIZE)
				buffer = new byte[bufferSize];
			else
				buffer = new byte[INITIAL_BUFFER_SIZE];
			limit = -1;
			bufferPos = -1;
		}

		public void initializeSharing() {
			table = new IValue[INITIAL_TABLE_SIZE];
			nr_terms = 0;
		}

		public void storeNextTerm(IValue t, int size) {
			if (table == null) {
				return;
			}

			if (nr_terms == table.length) {
				IValue[] new_table = new IValue[table.length + TABLE_INCREMENT];
				System.arraycopy(table, 0, new_table, 0, table.length);
				table = new_table;
			}

			table[nr_terms++] = t;
		}

		public IValue getTerm(int index) {
			if (index < 0 || index >= nr_terms) {
				throw new RuntimeException("illegal index");
			}
			return table[index];
		}

		public int read() throws IOException {
			if (bufferPos == limit) {
				limit = reader.read(buffer);
				bufferPos = 0;
			}

			if (limit == -1) {
				last_char = -1;
			} else {
				last_char = buffer[bufferPos++];
				pos++;
			}

			return last_char;
		}

		public int readSkippingWS() throws IOException {
			do {
				last_char = read();
			} while (Character.isWhitespace(last_char));

			return last_char;

		}

		public int skipWS() throws IOException {
			while (Character.isWhitespace(last_char)) {
				last_char = read();
			}

			return last_char;
		}

		public int readOct() throws IOException {
			int val = Character.digit(last_char, 8);
			val += Character.digit(read(), 8);

			if (val < 0) {
				throw new FactParseError("octal must have 3 octdigits.",
						getPosition());
			}

			val += Character.digit(read(), 8);

			if (val < 0) {
				throw new FactParseError("octal must have 3 octdigits",
						getPosition());
			}

			return val;
		}

		public int getLastChar() {
			return last_char;
		}

		public int getPosition() {
			return pos;
		}
	}
}
