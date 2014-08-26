/*  Part of ClioPatria SeRQL and SPARQL server

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, University of Amsterdam,
		   VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cliopatria_pengines, []).
:- use_module(library(pengines), []).
%:- use_module(pengine_sandbox:library(semweb/rdf_db)).
%:- use_module(pengine_sandbox:library(semweb/rdfs)).
:- use_module(library(sandbox), []).
:- use_module(cliopatria(entailment/rdf)).
:- use_module(library(semweb/rdf_litindex)).

/** <module> Provide pengines Prolog and JavaScript API

This module makes the ClioPatria RDF  API available through the pengines
package. Pengines provide an ideal mechanism for accessing the data from
web applications through JavaScript or from other Prolog processes using
pengine_rpc/3.

@see http://www.swi-prolog.org/pldoc/package/pengines.html
*/

sandbox:safe_primitive(rdf_entailment:rdf(_,_,_)).
sandbox:safe_primitive(rdf_litindex:rdf_find_literals(_,_)).

