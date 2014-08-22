:- module(test_query,
    [
        find_unique_interactions/1,
        find_interactions/7,
        find_atc/2
    ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/mimetype)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_host)).
:- use_module(library(semweb/rdf_db)).

%:- http_handler(testquery, ask, []).

% Get results from list.
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

find_unique_interactions(X) :-
    read(Input),
    setof(
        [Interaksjonsmekanisme,Konsekvens,Relevans,Handtering],
        find_interactions(Input,Name1,Name2,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering),
        Result),
    takeout(X,Result,_).

find_interactions(Input,Name1,Name2,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering) :-
    takeout(Substance_1,Input,R),
    takeout(Substance_2,R,_),
    rdf(_,interaksjon,I),
    rdf(I,_,literal(substring(Substance_1),_)),
    rdf(I,_,literal(substring(Substance_2),_)),
    rdf(I,interaksjonsmekanisme,Interaksjonsmekanisme),
    rdf(I,kliniskKonsekvens,Konsekvens),
    rdf(I,relevans,Relevans),
    rdf(I,handtering,Handtering),
    find_atc(Name1,Name2).

find_atc(Name1,Name2) :-
    rdf(_,interaksjon,I),
    rdf(I,id,literal('ID_3278CC6E-A001-477A-B228-9F8A012763E8')),
    rdf(I,substansgruppe,O),
    takeout(Name1,O,Name2)
    %rdf(X,substans,S),
    %rdf(Y,substans,S2),
    %rdf(S,atc,A),
    %rdf(S2,atc,A2),
    %rdf(A,'DN',Name1),
    %rdf(A2,'DN',Name2),
    %rdf(A,'V',ATC1),
    %rdf(A2,'V',ATC2)
    .

reply_decorated_file(Alias, _Request) :-
    absolute_file_name(Alias, Page, [access(read)]),
    load_html_file(Page, DOM),
    contains_term(element(title, _, Title), DOM),
    contains_term(element(body, _, Body), DOM),
    Style = element(style, _, _),
    findall(Style, sub_term(Style, DOM), Styles),
    append(Styles, Body, Content),
    reply_html_page(cliopatria(html_file),
    title(Title), Content).