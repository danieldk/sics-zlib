/*
 * A simple zlib module for Sicstus Prolog
 *
 * Copyright (c) 2009 Daniel de Kok
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

:- module(zlib,[ zlib_compress/2,
		 zlib_uncompress/3 ]).

foreign_resource(zlib,[ zlib_compress,
			zlib_uncompress ]).

foreign(zlib_compress,c,zlib_compress(+term,[-term])).
foreign(zlib_uncompress,c,zlib_uncompress(+term,+integer,[-term])).

:- load_foreign_resource(zlib).
