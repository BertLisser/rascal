/*******************************************************************************
 * Copyright (c) 2009-2011 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI
 *   * Mark Hills - Mark.Hills@cwi.nl (CWI)
 *   * Arnold Lankamp - Arnold.Lankamp@cwi.nl
 *******************************************************************************/
package org.rascalmpl.interpreter.result;

import static org.rascalmpl.interpreter.result.ResultFactory.bool;
import static org.rascalmpl.interpreter.result.ResultFactory.makeResult;

import java.util.Iterator;

import org.eclipse.imp.pdb.facts.IDateTime;
import org.eclipse.imp.pdb.facts.IInteger;
import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.type.Type;
import org.eclipse.imp.pdb.facts.type.TypeStore;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.staticErrors.InvalidComparisonError;
import org.rascalmpl.interpreter.staticErrors.UndeclaredFieldError;
import org.rascalmpl.interpreter.staticErrors.UnexpectedTypeError;
import org.rascalmpl.interpreter.staticErrors.UnsupportedOperationError;
import org.rascalmpl.interpreter.utils.RuntimeExceptionFactory;

public class DateTimeResult extends ElementResult<IDateTime> {

	public DateTimeResult(Type type, IDateTime value, IEvaluatorContext ctx) {
		super(type, value, ctx);
	}

	public DateTimeResult(Type type, IDateTime value,
			Iterator<Result<IValue>> iter, IEvaluatorContext ctx) {
		super(type, value, iter, ctx);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> equals(Result<V> that) {
		return that.equalToDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> equalToDateTime(DateTimeResult that) {
		return that.equalityBoolean(this);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> nonEquals(
			Result<V> that) {
		return that.nonEqualToDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> nonEqualToDateTime(
			DateTimeResult that) {
		return that.nonEqualityBoolean(this);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> compare(Result<V> that) {
		return that.compareDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> compareDateTime(DateTimeResult that) {
		System.err.println("Compare:" + that.value.getInstant() + " "
				+ this.value.getInstant());
		checkDateTimeComparison(that);
		return makeIntegerResult(that.value.getInstant() == this.value
				.getInstant() ? 0 : (that.value.getInstant() < this.value
				.getInstant() ? -1 : 1));
	}

	@Override
	public <U extends IValue> Result<U> fieldAccess(String name, TypeStore store) {
		IValueFactory vf = getValueFactory();
		IDateTime dt = getValue();

		if (name.equals("year")) {
			if (!dt.isTime()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(dt.getYear()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the year on a time value",
					ctx.getCurrentAST());
		} else if (name.equals("month")) {
			if (!dt.isTime()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getMonthOfYear()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the month on a time value",
					ctx.getCurrentAST());
		} else if (name.equals("day")) {
			if (!dt.isTime()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getDayOfMonth()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the day on a time value",
					ctx.getCurrentAST());
		} else if (name.equals("hour")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getHourOfDay()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the hour on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("minute")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getMinuteOfHour()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the minute on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("second")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getSecondOfMinute()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the second on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("millisecond")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getMillisecondsOfSecond()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the millisecond on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("timezoneOffsetHours")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getTimezoneOffsetHours()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the timezone offset hours on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("timezoneOffsetMinutes")) {
			if (!dt.isDate()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getTimezoneOffsetMinutes()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the timezone offset minutes on a date value",
					ctx.getCurrentAST());
		} else if (name.equals("century")) {
			if (!dt.isTime()) {
				return makeResult(getTypeFactory().integerType(),
						vf.integer(getValue().getCentury()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the century on a time value",
					ctx.getCurrentAST());
		} else if (name.equals("isDate")) {
			return makeResult(getTypeFactory().boolType(),
					vf.bool(getValue().isDate()), ctx);
		} else if (name.equals("isTime")) {
			return makeResult(getTypeFactory().boolType(),
					vf.bool(getValue().isTime()), ctx);
		} else if (name.equals("isDateTime")) {
			return makeResult(getTypeFactory().boolType(),
					vf.bool(getValue().isDateTime()), ctx);
		} else if (name.equals("justDate")) {
			if (!dt.isTime()) {
				return makeResult(
						getTypeFactory().dateTimeType(),
						vf.date(dt.getYear(), dt.getMonthOfYear(),
								dt.getDayOfMonth()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the date component of a time value",
					ctx.getCurrentAST());
		} else if (name.equals("justTime")) {
			if (!dt.isDate()) {
				return makeResult(
						getTypeFactory().dateTimeType(),
						vf.time(dt.getHourOfDay(), dt.getMinuteOfHour(),
								dt.getSecondOfMinute(),
								dt.getMillisecondsOfSecond(),
								dt.getTimezoneOffsetHours(),
								dt.getTimezoneOffsetMinutes()), ctx);
			}
			throw new UnsupportedOperationError(
					"Can not retrieve the time component of a date value",
					ctx.getCurrentAST());
		}
		throw new UndeclaredFieldError(name, getTypeFactory().dateTimeType(),
				ctx.getCurrentAST());
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> fieldUpdate(
			String name, Result<V> repl, TypeStore store) {

		Type replType = repl.getType();
		IValue replValue = repl.getValue();
		IDateTime dt = getValue();

		// Individual fields
		int year = dt.getYear();
		int month = dt.getMonthOfYear();
		int day = dt.getDayOfMonth();
		int hour = dt.getHourOfDay();
		int minute = dt.getMinuteOfHour();
		int second = dt.getSecondOfMinute();
		int milli = dt.getMillisecondsOfSecond();
		int tzOffsetHour = dt.getTimezoneOffsetHours();
		int tzOffsetMin = dt.getTimezoneOffsetMinutes();

		try {
			if (name.equals("year")) {
				if (dt.isTime()) {
					throw new UnsupportedOperationError(
							"Can not update the year on a time value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				year = ((IInteger) replValue).intValue();
			} else if (name.equals("month")) {
				if (dt.isTime()) {
					throw new UnsupportedOperationError(
							"Can not update the month on a time value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				month = ((IInteger) replValue).intValue();
			} else if (name.equals("day")) {
				if (dt.isTime()) {
					throw new UnsupportedOperationError(
							"Can not update the day on a time value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				day = ((IInteger) replValue).intValue();
			} else if (name.equals("hour")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the hour on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				hour = ((IInteger) replValue).intValue();
			} else if (name.equals("minute")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the minute on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				minute = ((IInteger) replValue).intValue();
			} else if (name.equals("second")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the second on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				second = ((IInteger) replValue).intValue();
			} else if (name.equals("millisecond")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the millisecond on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				milli = ((IInteger) replValue).intValue();
			} else if (name.equals("timezoneOffsetHours")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the timezone offset hours on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				tzOffsetHour = ((IInteger) replValue).intValue();
			} else if (name.equals("timezoneOffsetMinutes")) {
				if (dt.isDate()) {
					throw new UnsupportedOperationError(
							"Can not update the timezone offset minutes on a date value",
							ctx.getCurrentAST());
				}
				if (!replType.isIntegerType()) {
					throw new UnexpectedTypeError(getTypeFactory()
							.integerType(), replType, ctx.getCurrentAST());
				}
				tzOffsetMin = ((IInteger) replValue).intValue();
			} else {
				throw new UndeclaredFieldError(name, getTypeFactory()
						.dateTimeType(), ctx.getCurrentAST());
			}

			IDateTime newdt = null;
			if (dt.isDate()) {
				newdt = getValueFactory().date(year, month, day);
			} else if (dt.isTime()) {
				newdt = getValueFactory().time(hour, minute, second, milli,
						tzOffsetHour, tzOffsetMin);
			} else {
				newdt = getValueFactory().datetime(year, month, day, hour,
						minute, second, milli, tzOffsetHour, tzOffsetMin);
			}

			return makeResult(getType(), newdt, ctx);
		} catch (IllegalArgumentException e) {
			throw RuntimeExceptionFactory.illegalArgument(ctx.getCurrentAST(),
					null);
		}
	}

	private void checkDateTimeComparison(DateTimeResult that) {
		if (that.getValue().isDate()) {
			if (this.getValue().isTime()) {
				throw new InvalidComparisonError("date", "time",
						ctx.getCurrentAST());
			} else if (this.getValue().isDateTime()) {
				throw new InvalidComparisonError("date", "datetime",
						ctx.getCurrentAST());
			}
		} else if (that.getValue().isTime()) {
			if (this.getValue().isDate()) {
				throw new InvalidComparisonError("time", "date",
						ctx.getCurrentAST());
			} else if (this.getValue().isDateTime()) {
				throw new InvalidComparisonError("time", "datetime",
						ctx.getCurrentAST());
			}
		} else {
			if (this.getValue().isDate()) {
				throw new InvalidComparisonError("datetime", "date",
						ctx.getCurrentAST());
			} else if (this.getValue().isTime()) {
				throw new InvalidComparisonError("datetime", "time",
						ctx.getCurrentAST());
			}
		}
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> greaterThan(
			Result<V> that) {
		return that.greaterThanDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> greaterThanDateTime(
			DateTimeResult that) {
		checkDateTimeComparison(that);
		return bool(that.value.getInstant() > this.value.getInstant(), ctx);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> greaterThanOrEqual(
			Result<V> that) {
		return that.greaterThanOrEqualDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> greaterThanOrEqualDateTime(
			DateTimeResult that) {
		checkDateTimeComparison(that);
		return bool(that.value.getInstant() >= this.value.getInstant(), ctx);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> lessThan(
			Result<V> that) {
		return that.lessThanDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> lessThanDateTime(DateTimeResult that) {
		checkDateTimeComparison(that);
		return bool(that.value.getInstant() < this.value.getInstant(), ctx);
	}

	@Override
	public <U extends IValue, V extends IValue> Result<U> lessThanOrEqual(
			Result<V> that) {
		return that.lessThanOrEqualDateTime(this);
	}

	@Override
	protected <U extends IValue> Result<U> lessThanOrEqualDateTime(
			DateTimeResult that) {
		checkDateTimeComparison(that);
		return bool(that.value.getInstant() <= this.value.getInstant(), ctx);
	}

}
