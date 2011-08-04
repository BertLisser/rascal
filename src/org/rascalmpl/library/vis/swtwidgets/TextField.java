/*******************************************************************************
 * Copyright (c) 2009-2011 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Bert Lisser - Bert.Lisser@cwi.nl (CWI)
 *   * Paul Klint - Paul.Klint@cwi.nl - CWI
 *******************************************************************************/
package org.rascalmpl.library.vis.swtwidgets;

import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.imp.pdb.facts.IValueFactory;
import org.eclipse.imp.pdb.facts.type.TypeFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.rascalmpl.interpreter.IEvaluatorContext;
import org.rascalmpl.interpreter.result.Result;
import org.rascalmpl.library.vis.Figure;
import org.rascalmpl.library.vis.FigureApplet;
import org.rascalmpl.library.vis.FigureColorUtils;
import org.rascalmpl.library.vis.graphics.GraphicsContext;
import org.rascalmpl.library.vis.properties.PropertyManager;
import org.rascalmpl.library.vis.swt.IFigureApplet;
import org.rascalmpl.library.vis.swt.IFigureConstructionEnv;
import org.rascalmpl.library.vis.swt.SWTFontsAndColors;
import org.rascalmpl.values.ValueFactoryFactory;

public class TextField extends SWTWidgetFigureWithValidationAndCallBack<Text> {

	private final Color falseColor;
	

	public TextField(IFigureConstructionEnv env, String text, IValue cb, IValue validate, PropertyManager properties) {
		super(env, cb, validate, properties);
		falseColor = SWTFontsAndColors.getRgbColor(FigureColorUtils.colorNames.get("red").intValue());
		widget = makeWidget(env.getSWTParent(), env,text);
		widget.setVisible(false);
	}

	@Override
	public void draw(GraphicsContext gc) {
		super.draw(gc);
		widget.setForeground(validated ? SWTFontsAndColors.getRgbColor(getFontColorProperty()) : falseColor);
	}

	Text makeWidget(Composite comp, IFigureConstructionEnv env,String text) {
		Text textfield = new Text(comp, SWT.SINGLE | SWT.BORDER);
		
		textfield.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				doValidate();
			}
		});
		textfield.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
				doCallback();
			}
		});
		textfield.setText(text);
		return textfield;
	}

	@Override
	Result<IValue> executeValidate() {
		return cbenv.executeRascalCallBackSingleArgument(validate, TypeFactory.getInstance().stringType(), ValueFactoryFactory.getValueFactory().string(widget.getText()));
	}

	@Override
	void executeCallback() {
		cbenv.executeRascalCallBackSingleArgument(callback, TypeFactory.getInstance().stringType(), ValueFactoryFactory.getValueFactory().string(widget.getText()));
		
	}

}
