/*******************************************************************************
 * Copyright (c) 2009-2011 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Atze van der Ploeg - ploeg@cwi.nl (CWI)
 *******************************************************************************/

package org.rascalmpl.library.vis.figure.interaction.swtwidgets;

import org.eclipse.imp.pdb.facts.IValue;
import org.eclipse.swt.widgets.Control;
import org.rascalmpl.library.vis.properties.PropertyManager;
import org.rascalmpl.library.vis.swt.ICallbackEnv;
import org.rascalmpl.library.vis.swt.IFigureConstructionEnv;

public abstract class SWTWidgetFigureWithSingleCallBack<WidgetType extends Control> extends SWTWidgetFigure<WidgetType>{

	IValue callback;
	ICallbackEnv cbenv;
	
	SWTWidgetFigureWithSingleCallBack(IFigureConstructionEnv env, IValue callback, PropertyManager properties){
		super(env,properties);
		this.cbenv = env.getCallBackEnv();
		this.callback = callback;
	}

	void doCallback(){
		cbenv.runOutsideUIThread(new Runnable() {
			@Override
			public void run() {
				executeCallback();
				cbenv.signalRecompute();
			}
		});
	}
	
	abstract void executeCallback();
	
}
