:- module(test_query,
    [
        interaksjoner/5,
        legemiddelmerkevare/5,
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
    [\query_form
    ]).

query_form -->
    html([ form([ class(query),
    name(query),
    action(evaluate_query),
    method('GET')],
        [
        h3([ 'Medication Helper']),
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
    title('Resultater'),
    [h1(['Resultater for spørring: ', Q]),
        h3('Relevante stikkord: '),
        \stikkord(Q),
        h3('Interaksjoner: '),
        \interaksjoner(Q),
        h3('Legemiddelmerkevarer: '),
        \merkevarer(Q),
        h3('LegemiddelVirkestoffer: '),
        \legevirkestoff(Q),
        h3('LegemiddelDoser: '),
        \dose(Q),
        h3('LegemiddelPakninger: '),
        \pakning(Q),
        h3('Handelsvarer: '),
        \handelsvare(Q),
        h3('Virkestoffer: '),
        \virkestoffz(Q)
    ]
    ).

merkevarer(Q) -->
    { setof([Navn,ATC,Preparatomtale,Produktinfo],legemiddelmerkevare(Q,ATC,Navn,Preparatomtale,Produktinfo),LegemiddelBag)},
    html([ table([ class(block)
        ],
        [ tr([
            th('Navn'),
            th('ATC'),
            th('Preparatomtale'),
            th('Produktinfo')
        ])
        | \list_legemiddelmerkevarer(LegemiddelBag)
        ])
    ]).

list_legemiddelmerkevarer([]) -->
    [].
list_legemiddelmerkevarer([X|LegemiddelBag]) -->
    {takeout(VAL1,X,Y)},
    {takeout(VAL2,Y,Z)},
    {takeout(VAL3,Z,VAL4)},
    html(tr([
            td(VAL1),
            td(VAL2),
            td(a(href(encode(VAL3)), literal(VAL3))),
            td(VAL4)
    ])),
    list_legemiddelmerkevarer(LegemiddelBag).

interaksjoner(Q) -->
    {setof([Interaksjonsmekanisme,' ',Konsekvens,' ',Relevans,' ',Handtering],interaksjoner(Q,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering),InteraksjonerBag)},
    html([ table([ class(block)],
        [ \list_interaksjoner(InteraksjonerBag) ])
    ]).

list_interaksjoner([]) -->
    [].
list_interaksjoner([X|InteraksjonerBag]) -->
    html(tr([ td(X)])),
    list_interaksjoner(InteraksjonerBag).

legevirkestoff(Q) -->
    { setof([ATC,' ',Navn],legemiddelvirkestoff(Q,ATC,Navn),VirkestoffBag)},
    html([ table([ class(block)],
        [ \list_legemiddelvirkestoff(VirkestoffBag) ])
    ]).

list_legemiddelvirkestoff([]) -->
    [].
list_legemiddelvirkestoff([X|VirkestoffBag]) -->
    html(tr([ td(X)])),
    list_legemiddelvirkestoff(VirkestoffBag).

dose(Q) -->
    { setof([ATC,' ',Navn],legemiddeldose(Q,ATC,Navn),DoseBag)},
    html([ table([ class(block)],
        [ \list_legemiddeldose(DoseBag) ])
    ]).

list_legemiddeldose([]) -->
    [].
list_legemiddeldose([X|DoseBag]) -->
    html(tr([ td(X)])),
    list_legemiddeldose(DoseBag).

pakning(Q) -->
    { setof([ATC,' ',Navn],legemiddelpakning(Q,ATC,Navn),PakningBag)},
    html([ table([ class(block)],
        [ \list_legemiddelpakning(PakningBag) ])
    ]).

list_legemiddelpakning([]) -->
    [].
list_legemiddelpakning([X|PakningBag]) -->
    html(tr([ td(X)])),
    list_legemiddelpakning(PakningBag).

handelsvare(Q) -->
    { setof([Navn],medforbmatr(Q,Navn,Adr,LNavn,Tlf),HandelsBag)},
    html([ table([ class(block)],
        [ \list_handelsvare(HandelsBag) ])
    ]).

list_handelsvare([]) -->
    [].
list_handelsvare([X|HandelsBag]) -->
    html(tr([ td(X)])),
    list_handelsvare(HandelsBag).

virkestoffz(Q) -->
    { setof([Navn,' ',VAL],virkestoff(Q,Navn,VAL),VirkeBag)},
    html([ table([ class(block)],
        [ \list_virkestoff(VirkeBag) ])
    ]).

list_virkestoff([]) -->
    [].
list_virkestoff([X|VirkeBag]) -->
    html(tr([ td(X)])),
    list_virkestoff(VirkeBag).

stikkord(Q) -->
    {setof([Synonyms,', '],synonymer(Q,Synonyms),SynonymBag)},
    html([
        \list_stikkord(SynonymBag)
    ]).

list_stikkord([]) -->
    [].
list_stikkord([X|SynonymBag]) -->
    html(tr([ td(X)])),
    list_stikkord(SynonymBag).

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

interaksjoner(Input,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering) :-
    %takeout(Substance_1,Input,R),
    %takeout(Substance_2,R,_),
    rdf(_,interaksjon,I,fest),
    rdf(I,_,literal(substring(Input),_),fest),
    %rdf(I,_,literal(substring(Substance_2),_),fest),
    rdf(I,interaksjonsmekanisme,literal(Interaksjonsmekanisme),fest),
    rdf(I,kliniskKonsekvens,literal(Konsekvens),fest),
    rdf(I,relevans,literal(Relevans),fest),
    rdf(I,handtering,literal(Handtering),fest).

legemiddelmerkevare(Input,ATC,Navn,Preparatomtale,Produktinfo) :-
    rdf(_,legemiddelMerkevare,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest),
    rdf(L,preparatomtaleavsnitt,literal(Preparatomtale)),
    rdf(L,produktInfo,Produktinfo).

legemiddelvirkestoff(Input,ATC,Navn) :-
    rdf(_,legemiddelVirkestoff,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

legemiddeldose(Input,ATC,Navn) :-
    rdf(_,legemiddeldose,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

legemiddelpakning(Input,ATC,Navn) :-
    rdf(_,legemiddelpakning,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest).

medforbmatr(Input,Navn,Adr,LNavn,Tlf) :-
    rdf(_,medForbMatr,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,navn,literal(Navn),fest).
    %rdf(L,leverandor,LEV,fest),
    %rdf(LEV,adresse,Adr,fest),
    %rdf(LEV,navn,LNavn,fest),
    %rdf(LEV,tlf,Tlf,fest).

virkestoff(Input,Navn,VAL) :-
    rdf(A,_,literal(substring(Input),_),atc),
    rdf(A,rdfs:label,literal(Navn),atc),
    rdf(A,rdfs:seeAlso,literal(VAL),atc).

synonymer(Input,Synonyms) :-
    rdf(L,_,literal(substring(Input),_),icd10no),
    rdf(L, 'http://research.idi.ntnu.no/hilab/ehr/ontologies/icd10no.owl#synonym', literal(Synonyms)).

%["root","api/resources?r=http://www.legemiddelverket.no/Legemiddelsoek%23",ATC_code]
