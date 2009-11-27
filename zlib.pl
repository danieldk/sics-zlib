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
		 zlib_compress/3,
		 zlib_compress_term/3,
		 zlib_compress_term/4,
		 zlib_uncompress/3,
		 zlib_uncompress_term/3 ]).

:- use_module(library(fastrw),[ fast_buf_read/2,
				fast_buf_write/3 ]).

foreign_resource(zlib,[ zlib_compress,
			zlib_compress_buf_list,
			zlib_compress_default,
			zlib_free_buffer,
			zlib_uncompress,
			zlib_uncompress_list_buf ]).

foreign(zlib_compress,c,zlib_compress(+term,+integer,[-term])).
foreign(zlib_compress_buf_list,c,zlib_compress_buf_list(+address(char),+integer,
						   +integer,[-term])).
foreign(zlib_compress_default,c,zlib_compress(+term,[-term])).
foreign(zlib_free_buffer,c,zlib_free_buffer(+address)).
foreign(zlib_uncompress,c,zlib_uncompress(+term,+integer,[-term])).
foreign(zlib_uncompress_list_buf,c,zlib_uncompress_list_buf(+term,+integer,
							[-address])).

:- load_foreign_resource(zlib).

zlib_compress_term(Term,Compressed,Len) :-
    zlib_compress_term(Term,-1,Compressed,Len).

zlib_compress_term(Term,Level,Compressed,Len) :-
    call_residue(copy_term(Term,TermCopy),Cons),
    numbervars(TermCopy/Cons,0,_),
    fast_buf_write(TermCopy/Cons,Len,Addr),
    zlib_compress_buf_list(Addr,Len,Level,Compressed).

zlib_uncompress_term(Compressed,Len,Term) :-
    zlib_uncompress_list_buf(Compressed,Len,Addr),
    fast_buf_read(Term/Cons,Addr),
    zlib_free_buffer(Addr),
    call_constraints(Cons).

call_constraints([]).
call_constraints([_-Call|Rest]) :-
    call(Call),
    call_constraints(Rest).