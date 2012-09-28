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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.IRelationWriter;
import org.eclipse.imp.pdb.facts.ISet;
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
		if (debug)
			System.err.println("read1:" + type);
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
				|| typeByte == '[' || typeByte == '{' || typeByte == '-'
				|| typeByte == '.' || typeByte == '"') {
			JSonStream sreader = new JSonStream(stream);
			sreader.last_char = typeByte;
			if (debug)
				System.err.println("read2:" + type);
			IValue result = parse(sreader, type);
			if (debug)
				System.err.println("PARSED:" + result);
			if (type.isAbstractDataType()) {
				result = buildTerm((IMap) result, type);
			} else
				result = buildTerm(result, type);
			return result;
		} else {
			throw new RuntimeException("nyi");
		}
	}

	private IValue parse(JSonStream reader, Type expected) throws IOException {
		IValue result;
		int start;
		if (debug)
			System.err
					.println("Parse:" + expected + " " + reader.getLastChar());
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
		case 't':
		case 'f':
			result = parseBoolean(reader, expected);
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

		// end = reader.getPosition();
		// reader.storeNextTerm(result, end - start);

		return result;
	}

	private IValue parseAnnotations(JSonStream reader, IValue result)
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

	private IValue parseTuple(JSonStream reader, Type expected)
			throws IOException {
		if (debug)
			System.err.println("ParseTuple:" + expected);
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
			if (debug)
				System.err.println("ParseTuple:" + a[i] + " " + b[i] + " " + i);
			i++;
		}
		while (reader.getLastChar() == ',' && it.hasNext()) {
			reader.readSkippingWS();
			IValue term = parse(reader, it.next());
			a[i] = term;
			b[i] = term.getType();
			if (debug)
				System.err.println("ParseTuple:" + a[i] + " " + b[i] + " " + i);
			i++;
		}
		IValue result = tf.tupleType(b).make(vf, a);
		if (debug)
			System.err.println("result=" + result);
		if (reader.getLastChar() != ']') {
			throw new FactParseError("expected ']' but got '"
					+ (char) reader.getLastChar() + "'", reader.getPosition());
		}
		reader.readSkippingWS();
		return result;
	}

	private ITuple parseEntry(JSonStream reader, Type mapType)
			throws IOException {
		IValue[] array = new IValue[2];
		Type t = tf.tupleType(mapType.getKeyType(), mapType.getValueType());
		if (debug)
			System.err.println("ParseEntry:" + mapType.getKeyType() + " "
					+ mapType.getValueType());
		array[0] = parse(reader, mapType.getKeyType());
		if (debug)
			System.err.println("ParseEntry2:" + array[0]);
		if (reader.getLastChar() == ':') {
			reader.readSkippingWS();
			if (debug)
				System.err.println("ParseEntry3:" + mapType.getKeyType() + " "
						+ mapType.getValueType());
			array[1] = parse(reader, mapType.getValueType());
			if (debug)
				System.err.println("ParseEntry4:" + array[1]);
		} else
			throw new FactParseError("In map ':' expected",
					reader.getPosition());
		return (ITuple) t.make(vf, array[0], array[1]);
	}

	private IValue parseMap(JSonStream reader, Type expected)
			throws IOException {
		final int c = reader.readSkippingWS();
		if (debug)
			System.err.println("ParseMap1:" + expected);
		if (!expected.isMapType())
			expected = tf.mapType(tf.stringType(), tf.valueType());
		if (debug)
			System.err.println("ParseMap2:" + expected);
		IMapWriter w = expected.writer(vf);
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		if (reader.getLastChar() == '}') {
			reader.readSkippingWS();
			return w.done();
		}
		ITuple term = parseEntry(reader, expected);
		w.put(term.get(0), term.get(1));
		while (reader.getLastChar() == ',') {
			reader.readSkippingWS();
			term = parseEntry(reader, expected);
			w.put(term.get(0), term.get(1));
		}
		if (reader.getLastChar() != '}') {
			throw new FactParseError("expected '}' but got '"
					+ (char) reader.getLastChar() + "'", reader.getPosition());
		}
		reader.readSkippingWS();
		return w.done();
	}

	private IValue parseString(JSonStream reader, Type expected)
			throws IOException {
		int c;
		IValue result;
		String str = parseStringLiteral(reader);
		result = expected.make(vf, str);
		c = reader.readSkippingWS(); /* " */
		// if (c == -1) {
		// throw new FactParseError("premature EOF encountered.",
		// reader.getPosition());
		// }
		return result;
	}

	private IValue parseBoolean(JSonStream reader, Type expected)
			throws IOException {
		int c;
		IValue result;
		String str = parseBooleanLiteral(reader);
		if (!str.equalsIgnoreCase("true") && !str.equalsIgnoreCase("false"))
			throw new FactParseError("true or false expected but found:" + str
					+ ".", reader.getPosition());
		result = expected.make(vf, str.equalsIgnoreCase("true") ? true : false);
		c = reader.readSkippingWS(); /* e */
		// if (c == -1) {
		// throw new FactParseError("premature EOF encountered.",
		// reader.getPosition());
		// }
		return result;
	}

	private IValue parseList(JSonStream reader, Type expected)
			throws IOException {
		IValue result;
		if (debug)
			System.err.println("ParseList:" + expected);
		final int c = reader.readSkippingWS();
		if (c == -1) {
			throw new FactParseError("premature EOF encountered.",
					reader.getPosition());
		}
		if (c == ']') {
			reader.readSkippingWS();
			if (expected.isListType() || expected.isSetType()
					|| expected.isRelationType() || expected.isTupleType()) {
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

	private IValue parseAnnos(JSonStream reader, IValue result)
			throws IOException {
		result = parseAnno(reader, result);
		while (reader.getLastChar() == ',') {
			reader.readSkippingWS();
			result = parseAnno(reader, result);
		}

		return result;
	}

	private IValue parseAnno(JSonStream reader, IValue result)
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

	private IValue parseNumber(JSonStream reader, Type expected)
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
			result = !expected.isValueType() ? expected.make(vf, ts, val) : tf
					.integerType().make(vf, ts, val);
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
				result = !expected.isValueType() ? expected.make(vf, ts, val)
						: tf.realType().make(vf, ts, val);
			} catch (NumberFormatException e) {
				throw new FactParseError("malformed real",
						reader.getPosition(), e);
			}
		}
		reader.skipWS();
		return result;
	}

	private String parseBooleanLiteral(JSonStream reader) throws IOException {
		StringBuilder str = new StringBuilder();
		str.append((char) reader.getLastChar());
		do {
			reader.read();
			int lastChar = reader.getLastChar();
			str.append((char) lastChar);
			if (lastChar == -1)
				throw new IOException("Premature EOF.");

		} while (reader.getLastChar() != 'e');
		// str.append('e'); /* true or false */
		return str.toString();
	}

	private String parseStringLiteral(JSonStream reader) throws IOException {
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
		if (debug)
			System.err.println("buildTermList");
		for (int i = 0; i < t.length(); i++) {
			if (debug)
				System.err.println(t.get(i));
			a[i] = buildTerm(t.get(i), t.getElementType());
			b[i] = a[i].getType();
			if (debug)
				System.err.println("R:" + a[i] + " " + b[i]);
		}
		if (type.isTupleType())
			return tf.tupleType(b).make(vf, a);
		if (type.isSetType())
			return (tf.setType(t.isEmpty() ? t.getElementType() : b[0]).make(
					vf, a));
		return (tf.listType(t.isEmpty() ? t.getElementType() : b[0])
				.make(vf, a));
	}

	private IValue buildTerm(ISet t, Type type) {
		if (debug)
			System.err.println("buildTermSet" + " " + t.size());
		IValue[] a = new IValue[t.size()];
		Type[] b = new Type[t.size()];
		Iterator<IValue> it = t.iterator();
		for (int i = 0; i < t.size(); i++) {
			a[i] = buildTerm(it.next(), type.isValueType() ? t.getElementType()
					: type.getElementType());
			b[i] = a[i].getType();
		}
		return (tf.setType(t.isEmpty() ? t.getElementType() : b[0]).make(vf, a));
	}

	private IValue buildTerm(ITuple t, Type type) {
		IValue[] a = new IValue[t.arity()];
		Type[] b = new Type[t.arity()];
		for (int i = 0; i < t.arity(); i++) {
			a[i] = buildTerm(t.get(i), type.isValueType() ? t.get(i).getType()
					: type.getFieldType(i));
			b[i] = a[i].getType();
		}
		return tf.tupleType(b).make(vf, a);
	}

	private IValue _buildTerm(IMap t, Type type) {
		IValue[] a1 = new IValue[t.size()];
		Type[] b1 = new Type[t.size()];
		IValue[] a2 = new IValue[t.size()];
		Type[] b2 = new Type[t.size()];
		Iterator<IValue> it = t.iterator();
		for (int i = 0; i < t.size(); i++) {
			a1[i] = buildTerm(it.next(), type.getKeyType());
			b1[i] = a1[i].getType();
			a2[i] = buildTerm(t.get(a1[i]), type.getValueType());
			b2[i] = a2[i].getType();
		}
		IMapWriter w = tf.mapType(a1.length == 0 ? t.getKeyType() : b1[0],
				a1.length == 0 ? t.getValueType() : b2[0]).writer(vf);
		for (int i = 0; i < t.size(); i++) {
			w.put(a1[i], a2[i]);
		}
		return w.done();
	}

	private IValue buildTerm(IMap t, Type type) {
		IValue key = t.get(nameKey);
		if (key == null)
			if (type.isMapType())
				return _buildTerm(t, type);
			else
				return t;
		final String funname = ((IString) key).getValue();
		IList rs = (IList) tf.listType(tf.valueType()).make(vf);
		final Iterator<IValue> args = ((IList) t.get(argKey)).iterator();
		while (args.hasNext()) {
			IValue arg = (IValue) args.next();
			arg = buildTerm(arg, type);
			rs = rs.append(arg);
		}
		IValue[] a = new IValue[rs.length()];
		Type[] b = new Type[rs.length()];
		{
			int i = 0;
			for (IValue c : rs) {
				a[i] = c;
				b[i] = c.getType();
				i++;
			}
		}
		if (funname.equals("tuple")) {
			return tf.tupleType(b).make(vf, a);
		}
		if (funname.equals("set")) {
			return tf.setType(b.length > 0 ? b[0] : tf.valueType()).make(vf, a);
		}
		if (funname.equals("map")) {
			System.err.println("buildTerm:" + b[0].getFieldType(0) + " "
					+ b[0].getFieldType(1));
			IMapWriter w = tf.mapType(
					a.length == 0 ? tf.valueType() : b[0].getFieldType(0),
					a.length == 0 ? tf.valueType() : b[0].getFieldType(1))
					.writer(vf);
			System.err.println("buildTerm:size " + t.size());
			for (int i = 0; i < a.length; i++) {
				System.err.println("buildTerm:" + a[i] + " " + i);
				w.put(((ITuple) a[i]).get(0), ((ITuple) a[i]).get(1));
			}
			return w.done();
		}
		Type types = tf.tupleType(b);
		// if (debug)
		System.err.println("lookupFirstConstructor:" + funname + " " + types
				+ " " + type);
		/* Local data types also searched - Monday */
		Type node = null;
		if (type.isAbstractDataType())
			node = ts.lookupConstructor(type, funname, types);
		if (node == null)
			node = ts.lookupFirstConstructor(funname, types);
		System.err.println("node=" + node);
		if (node.isAliasType())
			node = node.getAliased();
		return node.make(vf, a);
	}

	private IValue buildTerm(IValue t, Type type) {
		// System.err.println("BuildTerm:" + t + " " + type);
		if (t instanceof IMap)
			return buildTerm((IMap) t, type);
		if (t instanceof IList)
			return buildTerm((IList) t, type);
		if (t instanceof ISet)
			return buildTerm((ISet) t, type);
		if (t instanceof ITuple)
			return buildTerm((ITuple) t, type);
		return t;
	}

	private IValue parseTerms(JSonStream reader, Type expected)
			throws IOException {
		Type base = expected;
		Type elementType = getElementType(expected);
		IValue[] terms = parseTermsArray(reader, elementType);
		if (debug)
			System.err.println("ParseTerms2:" + base + " " + elementType);
		if (base.isListType() || base.isValueType()) {
			IListWriter w = expected.writer(vf);
			for (int i = terms.length - 1; i >= 0; i--) {
				w.insert(terms[i]);
			}
			return w.done();
		} else if (base.isRelationType()) {
			IRelationWriter w = expected.writer(vf);
			w.insert(terms);
			return w.done();

		} else if (base.isSetType()) {
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

	private IValue[] parseTermsArray(JSonStream reader, Type elementType)
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

	class JSonStream {

		private static final int INITIAL_BUFFER_SIZE = 1024;

		private BufferedReader reader;

		int last_char;
		private int pos;

		private char[] buffer;
		private int limit;
		private int bufferPos;

		public JSonStream(InputStream reader) {
			this(reader, INITIAL_BUFFER_SIZE);
		}

		public JSonStream(InputStream stream, int bufferSize) {
			this.reader = new BufferedReader(new InputStreamReader(stream));
			last_char = -1;
			pos = 0;

			if (bufferSize < INITIAL_BUFFER_SIZE)
				buffer = new char[bufferSize];
			else
				buffer = new char[INITIAL_BUFFER_SIZE];
			limit = -1;
			bufferPos = -1;
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
