:- module(test_query,
    [
        find_unique_interactions/1,
        interaksjoner/5,
        legemiddelmerkevare/3,
        legemiddelvirkestoff/3,
        legemiddeldose/3,
        legemiddelpakning/3,
        medforbmatr/5,
        virkestoff/3,
        synonymer/2
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
:- use_module(library(http/http_parameters)).
:- use_module(library(semweb/rdf_db)).
:- use_module(cliopatria(components/basics)).

:- http_handler(cliopatria('testquery'), query_form, []).
:- http_handler(cliopatria('evaluate_query'), evaluate_query, []).

query_form(_Request) :-
    reply_html_page(cliopatria(default),
    title('Specify a query'),
    [\query_form([])
    ]).

query_form(Options) -->
    html([ form([ class(query),
    name(query),
    action(evaluate_query),
    method('GET')],
        [
        h3([ 'Query']),
            table([ class(query)],
                [
                    tr([ td(colspan(5),
                    textarea(name(query), ''))
                        ]),
                    tr([
                        td(align(right),
                        [ input([ type(reset),
                        value('Clear')
                        ]),
                        input([ type(submit),
                        value('Go!')
                            ])
                        ])
                    ])
                ])
        ])
    ]).

evaluate_query(Request) :-
    http_parameters(Request,
                    [ query(Q, [optional(true)])
                    ]),
    reply_html_page(cliopatria(default),
    title('Results'),
    [h1(['Results: ', Q])]
    ).

query_results -->
    html([h1(['Hullo'])]).

% Get results from list.
takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

find_unique_interactions(X) :-
    read(Input),
    setof(
        [Interaksjonsmekanisme,Konsekvens,Relevans,Handtering],
        interaksjoner(Input,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering),
        Result),
    takeout(X,Result,_).

interaksjoner(Input,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering) :-
    takeout(Substance_1,Input,R),
    takeout(Substance_2,R,_),
    rdf(_,interaksjon,I,fest),
    rdf(I,_,literal(substring(Substance_1),_),fest),
    rdf(I,_,literal(substring(Substance_2),_),fest),
    rdf(I,interaksjonsmekanisme,Interaksjonsmekanisme,fest),
    rdf(I,kliniskKonsekvens,Konsekvens,fest),
    rdf(I,relevans,Relevans,fest),
    rdf(I,handtering,Handtering,fest).

legemiddelmerkevare(Input,ATC,Navn) :-
    takeout(Substance,Input,_),
    rdf(_,legemiddelMerkevare,L,fest),
    rdf(L,_,literal(substring(Substance),_),festv),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

legemiddelvirkestoff(Input,ATC,Navn) :-
    takeout(Substance,Input,_),
    rdf(_,legemiddelVirkestoff,L,fest),
    rdf(L,_,literal(substring(Substance),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

legemiddeldose(Input,ATC,Navn) :-
    takeout(Substance,Input,_),
    rdf(_,legemiddeldose,L,fest),
    rdf(L,_,literal(substring(Substance),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

legemiddelpakning(Input,ATC,Navn) :-
    takeout(Substance,Input,_),
    rdf(_,legemiddelpakning,L,fest),
    rdf(L,_,literal(substring(Substance),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

medforbmatr(Input,Navn,Adr,LNavn,Tlf) :-
    takeout(Substance,Input,_),
    rdf(_,medForbMatr,L,fest),
    rdf(L,_,literal(substring(Substance),_),fest),
    rdf(L,navn,Navn,fest),
    rdf(L,leverandor,LEV,fest);(
        rdf(LEV,adresse,Adr,fest),
        rdf(LEV,navn,LNavn,fest),
        rdf(LEV,tlf,Tlf,fest))
    .

virkestoff(Input,Navn,VAL) :-
    takeout(Substance,Input,_),
    rdf(_,virkestoff,V,fest),
    rdf(V,_,literal(substring(Substance),_),fest),
    rdf(V,navn,Navn,fest),
    rdf(A,_,literal(substring(Substance),_),atc),
    rdf(A,rdfs:seeAlso,VAL).

synonymer(Input,Synonyms) :-
    takeout(X,Input,_),
    rdf(L,_,literal(substring(X),_),icd10no),
    rdf(L, 'http://research.idi.ntnu.no/hilab/ehr/ontologies/icd10no.owl#synonym', Synonyms).

%["root","api/resources?r=http://www.legemiddelverket.no/Legemiddelsoek%23",ATC_code]
