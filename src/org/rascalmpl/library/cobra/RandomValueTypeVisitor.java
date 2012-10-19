/*******************************************************************************
 * Copyright (c) 2009-2012 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Wietse Venema - wietsevenema@gmail.com - CWI
 *******************************************************************************/
package org.rascalmpl.library.cobra;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Random;

import org.apache.commons.lang.RandomStringUtils;
import org.eclipse.imp.pdb.facts.IConstructor;
import org.eclipse.imp.pdb.facts.IList;
import org.eclipse.imp.pdb.facts.IListWriter;
import org.eclipse.imp.pdb.facts.IMap;
import org.eclipse.imp.pdb.facts.IMapWriter;
import org.eclipse.imp.pdb.facts.ISet;
import org.eclipse.imp.pdb.facts.ISetWriter;
import org.eclipse.imp.pdb.facts.ISourceLocation;
import org.eclipse.imp.pdb.facts.IString;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.impl.ExternalValue;
import org.eclipse.imp.pdb.facts.type.ExternalType;
import org.eclipse.imp.pdb.facts.type.ITypeVisitor;
import org.eclipse.imp.pdb.facts.type.Type;
import org.eclipse.imp.pdb.facts.type.TypeFactory;
import org.rascalmpl.interpreter.control_exceptions.ContinueException;
import org.rascalmpl.interpreter.control_exceptions.Throw;
import org.rascalmpl.interpreter.env.ModuleEnvironment;
import org.rascalmpl.interpreter.result.ICallableValue;
import org.rascalmpl.interpreter.result.Result;
import org.rascalmpl.interpreter.staticErrors.NotEnumerableError;

public class RandomValueTypeVisitor implements ITypeVisitor<IValue> {

	private static final Random stRandom = new Random();

	private final IValueFactory vf;
	private final ModuleEnvironment rootEnv;
	private final int maxDepth;
	private final HashMap<Type, ICallableValue> generators;

	class FunctionValue extends ExternalValue {
		FunctionValue(ExternalType t) {
			super(t);
		}
	}

	public RandomValueTypeVisitor(IValueFactory vf, ModuleEnvironment rootEnv,
			int maxDepth, HashMap<Type, ICallableValue> generators) {
		this.vf = vf;
		this.rootEnv = rootEnv;
		this.maxDepth = maxDepth;
		this.generators = generators;
	}

	private IValue callGenerator(Type type, int depthLimit) {
		if (depthLimit < 0) {
			return null;
		}
		TypeFactory tf = TypeFactory.getInstance();

		ICallableValue generator = generators.get(type);
		Result<IValue> result = generator.call(new Type[] { tf.integerType() },
				new IValue[] { vf.integer(depthLimit) });
		return result.getValue();
	}

	private RandomValueTypeVisitor descend() {
		RandomValueTypeVisitor visitor = new RandomValueTypeVisitor(vf,
				rootEnv, maxDepth - 1, generators);
		return visitor;
	}

	public IValue generate(Type t) {
		if (generators.containsKey(t)) {
			return callGenerator(t, maxDepth);
		} else {
			return t.accept(this);
		}
	}

	private IValue genSet(Type type) {
		ISetWriter writer = type.writer(vf);

		if (maxDepth <= 0 || (stRandom.nextInt(2) == 0)) {
			return writer.done();
		} else {
			RandomValueTypeVisitor visitor = descend();
			ISet set = (ISet) visitor.generate(type);

			IValue element = null;
			int recursionGuard = 0; // Domain of set can be small.
			while ((element == null || set.contains(element))
					&& recursionGuard < 1000) {
				recursionGuard += 1;
				element = visitor.generate(type.getElementType());
			}

			writer.insertAll(set);

			if (element != null) {
				writer.insert(element);
			}
			return writer.done();
		}
	}

	@Override
	public IValue visitAbstractData(Type type) {
		LinkedList<Type> alternatives = new LinkedList<Type>();
		alternatives.addAll(this.rootEnv.lookupAlternatives(type));
		Collections.shuffle(alternatives);
		for (Type pick : alternatives) {
			IConstructor result = (IConstructor) this.generate(pick);
			if (result != null) {
				RandomValueTypeVisitor visitor = descend();
				Map<String, Type> annotations = rootEnv.getStore()
						.getAnnotations(type);
				for (Map.Entry<String, Type> entry : annotations.entrySet()) {
					IValue value = visitor.generate(entry.getValue());
					if (value == null) {
						return null;

					}
					result = result.setAnnotation(entry.getKey(), value);
				}

				return result;

			}
		}
		return null;
	}

	@Override
	public IValue visitAlias(Type type) {
		// Het is niet mogelijk om een circulaire alias te maken dus dit kost
		// geen diepte.
		return this.generate(type.getAliased());
	}

	@Override
	public IValue visitBool(Type boolType) {
		return vf.bool(stRandom.nextBoolean());
	}

	@Override
	public IValue visitConstructor(Type type) {
		/*
		 * Following the common definition of depth of tree, the depth of an
		 * algebraic datatype with zero arguments is 0 and the depth of an
		 * alternative with more than 0 arguments is defined as the maximum
		 * depth of the list of arguments plus 1.
		 */

		if (type.getArity() == 0) { // Diepte 0 dus we mogen altijd opleveren.
			return vf.constructor(type);
		} else if (this.maxDepth <= 0) {
			return null;
		}

		RandomValueTypeVisitor visitor = descend();

		LinkedList<IValue> values = new LinkedList<IValue>();
		for (int i = 0; i < type.getArity(); i++) {
			Type fieldType = type.getFieldType(i);
			IValue argument = visitor.generate(fieldType);
			if (argument == null) {
				return null;
				/*
				 * Het is onmogelijk om de constructor te bouwen als ��n
				 * argument null is.
				 */
			}
			values.add(argument);
		}
		return vf.constructor(type, values.toArray(new IValue[values.size()]));

	}

	@Override
	public IValue visitDateTime(Type type) {
		long maxDate = vf.date(3000, 12, 31).getInstant();
		long n = stRandom.nextLong();
		if (n < 0)
			n = -n;
		return vf.datetime(n % maxDate);
	}

	@Override
	public IValue visitExternal(Type externalType) {
		// IValue r = vf.node(externalType.toString());
		// System.err.println("QQQ:" + r);
		// return r;
		// throw new Throw(vf.string("Can't handle ExternalType."),
		// (ISourceLocation) null, null);
		throw new ContinueException("Can't handle ExternalType.");
	}

	@Override
	public IValue visitInteger(Type type) {
		return vf.integer(stRandom.nextInt());
	}

	@Override
	public IValue visitList(Type type) {
		IListWriter writer = type.writer(vf);

		if (maxDepth <= 0 || (stRandom.nextInt(2) == 0)) {
			return writer.done();
		} else {
			RandomValueTypeVisitor visitor = descend();
			IValue element = visitor.generate(type.getElementType());
			if (element != null) {
				writer.append(element);
			}
			writer.appendAll((IList) visitor.generate(type));
			return writer.done();
		}

	}

	@Override
	public IValue visitMap(Type type) {
		IMapWriter writer = type.writer(vf);

		if (maxDepth <= 0 || (stRandom.nextInt(2) == 0)) {
			return writer.done();
		} else {

			RandomValueTypeVisitor visitor = descend();
			IValue key = visitor.generate(type.getKeyType());
			IValue value = visitor.generate(type.getValueType());

			if (key != null && value != null) {
				writer.put(key, value);
			}

			writer.putAll((IMap) visitor.generate(type));
			return writer.done();
		}
	}

	@Override
	public IValue visitNode(Type type) {
		throw new Throw(vf.string("Can't handle Node."),
				(ISourceLocation) null, null);
	}

	@Override
	public IValue visitNumber(Type type) {
		switch (stRandom.nextInt(3)) {
		case 0:
			return this.visitInteger(type);
		case 1:
			return this.visitReal(type);
		default:
			return this.visitRational(type);
		}
	}

	@Override
	public IValue visitParameter(Type parameterType) {
		// FIXME Type parameters binden aan echte type van actual value in call.
		return this.generate(parameterType.getBound());
	}

	@Override
	public IValue visitRational(Type type) {
		return vf.rational(stRandom.nextInt(), stRandom.nextInt());
	}

	@Override
	public IValue visitReal(Type type) {
		return vf.real(stRandom.nextDouble());
	}

	@Override
	public IValue visitRelationType(Type type) {
		return genSet(type);
	}

	@Override
	public IValue visitSet(Type type) {
		return genSet(type);
	}

	@Override
	public IValue visitSourceLocation(Type type) {
		return vf.sourceLocation("tmp:///");
	}

	@Override
	public IValue visitString(Type type) {
		if (maxDepth <= 0 || (stRandom.nextInt(2) == 0)) {
			return vf.string("");
		} else {
			RandomValueTypeVisitor visitor = descend();
			IString str = (IString) visitor.generate(type);
			return str.concat(vf.string(RandomStringUtils.random(1)));
		}
	}

	@Override
	public IValue visitTuple(Type type) {
		RandomValueTypeVisitor visitor = descend();

		IValue[] elems = new IValue[type.getArity()];
		for (int i = 0; i < type.getArity(); i++) {
			Type fieldType = type.getFieldType(i);
			IValue element = visitor.generate(fieldType);
			if (element == null) {
				return null;
			}
			elems[i] = visitor.generate(fieldType);
		}
		return vf.tuple(elems);
	}

	@Override
	public IValue visitValue(Type type) {
		RandomType rt = new RandomType();
		return this.generate(rt.getType(maxDepth));
	}

	@Override
	public IValue visitVoid(Type type) {
		throw new Throw(vf.string("void has no values."),
				(ISourceLocation) null, null);
	}

}
