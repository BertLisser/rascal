/*******************************************************************************
 * Copyright (c) 2009-2011 CWI
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:

 *   * Jurgen J. Vinju - Jurgen.Vinju@cwi.nl - CWI
*******************************************************************************/
package org.rascalmpl.uri;

import java.net.URI;

public class HomeURIResolver extends FileURIResolver {
	@Override
	public String scheme() {
		return "home";
	}
	
	@Override
	protected String getPath(URI uri) {
		String path = super.getPath(uri);
		return System.getProperty("user.home") + (path.startsWith("/") ? path : ("/" + path));
	}
}
