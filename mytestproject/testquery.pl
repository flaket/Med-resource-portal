:- module(test_query,
    [

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
:- use_module(library(readutil)).
:- use_module(library(isub)).

:- http_handler(cliopatria('demonstrator'), query_form, []).
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
        h3([ 'Medikasjonshjelpen']),
        p([ 'Søkeord skilles med komma. Eksempel: bosutinib, sotalol .']),
        p([ 'Søket leter etter substrenger. Det betyr at du kan gi søk som: para, eta . Forvent å få mange resultater dersom substrengene er små.' ]),
        p([ 'Søket returnerer kun interaksjoner mellom kombinasjoner av søkeordene.' ]),
        p([ 'Dersom det ønskes å se alle interaksjoner et enkelt stoff inngår i må dette stoffet dukke opp to ganger blant søkeordene.' ]),
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
                    [ query(Query, [optional(true)])
                    ]),
    split_string(Query, ",", " ", L),
    findall(Xs,permutation(Xs,L),PermutationBag),
    directory_files('/Users/andreas/Documents/Sommerjobb_2014/Datakilder/NLH/NLH-html-20130925-01/output/', Files),
    reply_html_page(cliopatria(default),
    title('Resultater'),
    [h1(['Resultater for spørring: ', Query]),
        h3('Relevante stikkord: '),
        \stikkord(L),
        h3('Interaksjoner: '),
        \interaksjoner(PermutationBag),
        h3('Legemiddelmerkevarer: '),
        \merkevarer(L),
        h3('LegemiddelVirkestoffer: '),
        \legevirkestoff(L),
        h3('LegemiddelDoser: '),
        \dose(L),
        h3('LegemiddelPakninger: '),
        \pakning(L),
        h3('Handelsvarer: '),
        \handelsvare(L),
        h3('Virkestoffer: '),
        \virkestoff(L),
        h3('NLH kapitler: '),
        \nlh(L, Files)
    ]
    ).

merkevarer([]) -->
    [].
merkevarer([X|L]) -->
    {atom_string(Q,X)},
    (
    ({setof([Navn,ATC,Preparatomtale,Produktinfo,AdminVei,EnhetDosering,Bruk],legemiddelmerkevare(Q,ATC,Navn,Preparatomtale,Produktinfo,AdminVei,EnhetDosering,Bruk),LegemiddelBag)})
    -> html([ table([ class(block)
        ],
        [ tr([
            th('Navn'),
            th('ATC'),
            th('Preparatomtale'),
            th('Produktinfo'),
            th('Administrasjonsvei'),
            th('Enhetdosering'),
            th('Bruk')
        ])
        | \list_legemiddelmerkevarer(LegemiddelBag)
        ])
    ]),
    merkevarer(L)
    ; merkevarer(L)
    ).

list_legemiddelmerkevarer([]) -->
    [].
list_legemiddelmerkevarer([X|LegemiddelBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,A)},
    {select(VAL4,A,B)},
    {select(VAL5,B,C)},
    {select(VAL6,C,VAL7)},
    html(tr([
            td(VAL1),
            td(VAL2),
            td(a(href(encode(VAL3)), literal(VAL3))),
            td(VAL4),
            td(VAL5),
            td(VAL6),
            td(VAL7)
    ])),
    list_legemiddelmerkevarer(LegemiddelBag).

interaksjoner([]) -->
[].
interaksjoner([X|L]) -->
    {select(A,X,Y)},
    {select(B,Y,_)},
    {atom_string(Q1,A)},
    {atom_string(Q2,B)},
    (
    ({setof([Hit1,Hit2,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering],interaksjoner(Q1,Q2,Hit1,Hit2,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering),InteraksjonerBag)})
   -> html([ table([ class(block)
        ],
        [ tr([
            th('Stoff 1'),
            th('Stoff 2'),
            th('Mekanisme'),
            th('Konsekvens'),
            th('Relevans'),
            th('Håndtering')
        ])
        | \list_interaksjoner(InteraksjonerBag)
        ])
    ]),
    interaksjoner(L)
    ; interaksjoner(L)).

list_interaksjoner([]) -->
    [].
list_interaksjoner([X|InteraksjonerBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,A)},
    {select(VAL4,A,B)},
    {select(VAL5,B,VAL6)},
    html(tr([
        td(VAL1),
        td(VAL2),
        td(VAL3),
        td(VAL4),
        td(VAL5),
        td(VAL6)
    ])),
    list_interaksjoner(InteraksjonerBag).

legevirkestoff([]) -->
    [].
legevirkestoff([X|L]) -->
    {atom_string(Q,X)},
    (
    ({ setof([Navn,ATC,Reseptgruppe,Adminvei,Enhetdosering,Kortdose],legemiddelvirkestoff(Q,Navn,ATC,Reseptgruppe,Adminvei,Enhetdosering,Kortdose),VirkestoffBag)})
    -> html([ table([ class(block)
        ],
        [ tr([
            th('Navn'),
            th('ATC'),
            th('Reseptgruppe'),
            th('Administrasjonsvei'),
            th('Enhetdosering'),
            th('Kortdose')
        ])
        | \list_legemiddelvirkestoff(VirkestoffBag)
        ])
    ]),
    legevirkestoff(L)
    ; legevirkestoff(L)).

list_legemiddelvirkestoff([]) -->
    [].
list_legemiddelvirkestoff([X|VirkestoffBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,A)},
    {select(VAL4,A,B)},
    {select(VAL5,B,VAL6)},
    html(tr([
        td(VAL1),
        td(VAL2),
        td(VAL3),
        td(VAL4),
        td(VAL5),
        td(VAL6)
    ])),
    list_legemiddelvirkestoff(VirkestoffBag).

dose([]) -->
    [].
dose([X|L]) -->
    {atom_string(Q,X)},
    (
    ({ setof([Navn,ATC,Preparattype,Reseptgruppe],legemiddeldose(Q,Navn,ATC,Preparattype,Reseptgruppe),DoseBag)})
    -> html([ table([ class(block)
        ],
        [ tr([
            th('Navn'),
            th('ATC'),
            th('Preparattype'),
            th('Reseptgruppe')
        ])
        | \list_legemiddeldose(DoseBag)
        ])
    ]),
    dose(L)
    ; dose(L)).

list_legemiddeldose([]) -->
    [].
list_legemiddeldose([X|DoseBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,VAL4)},
    html(tr([
        td(VAL1),
        td(VAL2),
        td(VAL3),
        td(VAL4)
    ])),
    list_legemiddeldose(DoseBag).

pakning([]) -->
    [].
pakning([X|L]) -->
    {atom_string(Q,X)},
    (
    ({ setof([Navn,ATC,Pakningstype,Mengde,Enhetpakning,Omtale],legemiddelpakning(Q,Navn,ATC,Pakningstype,Mengde,Enhetpakning,Omtale),PakningBag)})
    -> html([ table([ class(block)
            ],
            [ tr([
                th('Navn'),
                th('ATC'),
                th('Pakningstype'),
                th('Mengde'),
                th('Enhetpakning'),
                th('Omtale')
            ])
            | \list_legemiddelpakning(PakningBag) ])
    ]),
    pakning(L)
    ; pakning(L)).

list_legemiddelpakning([]) -->
    [].
list_legemiddelpakning([X|PakningBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,A)},
    {select(VAL4,A,B)},
    {select(VAL5,B,VAL6)},
    html(tr([
        td(VAL1),
        td(VAL2),
        td(VAL3),
        td(VAL4),
        td(VAL5),
        td(VAL6)
    ])),
    list_legemiddelpakning(PakningBag).

handelsvare([]) -->
    [].
handelsvare([X|L]) -->
    {atom_string(Q,X)},
    (
    ({ setof([Navn,LNavn,Adr,Tlf,Pr,Str,Produktnr],medforbmatr(Q,Navn,LNavn,Adr,Tlf,Pr,Str,Produktnr),HandelsBag)})
    -> html([ table([ class(block)
            ],
        [ tr([
            th('Varenavn'),
            th('Leverandør Navn'),
            th('Leverandør Adresse'),
            th('Leverandør Telefon'),
            th('Per pakning'),
            th('Størrelse'),
            th('Produktnr.')
        ])
        | \list_handelsvare(HandelsBag)])
    ]),
    handelsvare(L)
    ; handelsvare(L)).

list_handelsvare([]) -->
    [].
list_handelsvare([X|HandelsBag]) -->
    {select(VAL1,X,Y)},
    {select(VAL2,Y,Z)},
    {select(VAL3,Z,A)},
    {select(VAL4,A,B)},
    {select(VAL5,B,C)},
    {select(VAL6,C,VAL7)},
    html(tr([
        td(VAL1),
        td(VAL2),
        td(VAL3),
        td(VAL4),
        td(VAL5),
        td(VAL6),
        td(VAL7)
    ])),
    list_handelsvare(HandelsBag).

virkestoff([]) -->
    [].
virkestoff([X|L]) -->
    {atom_string(Q,X)},
    (
    ({ setof([Navn,' ',VAL],drugbank(Q,Navn,VAL),VirkeBag)})
    -> html([ table([ class(block)
            ],
            [ tr([
                th('Navn'),
                th('Lenke')
        ])
        | \list_virkestoff(VirkeBag) ])
    ]),
    virkestoff(L)
    ; virkestoff(L)).

list_virkestoff([]) -->
    [].
list_virkestoff([X|VirkeBag]) -->
    {select(VAL1,X,VAL2)},
    html(tr([
        td(VAL1),
        td(VAL2)
    ])),
    list_virkestoff(VirkeBag).

stikkord([]) -->
    [].
stikkord([X|L]) -->
    {atom_string(Q,X)},
    (
    ({setof([Navn,Synonyms],synonymer(Q,Navn,Synonyms),SynonymBag)})
    -> html([
        \list_stikkord(SynonymBag)
    ]),
    stikkord(L)
    ; stikkord(L)).

list_stikkord([]) -->
    [].
list_stikkord([X|SynonymBag]) -->
    {select(VAL1,X,VAL2)},
    html(tr([
        td(VAL1),
        td(VAL2)
    ])),
    list_stikkord(SynonymBag).

nlh([], _Files) -->
    [].
nlh([X|L], Files) -->
    html([ table([ class(block)
    ],
    [ tr([
        th('Kapittel'),
        th('Similarity score'),
        th('Funn')
    ])
    | \do_file(Files, X) ])
    ]),
    nlh(L, Files).

do_file([], _X) -->
[].
do_file([File|Files], X) -->
    {string_concat('/Users/andreas/Documents/Sommerjobb_2014/Datakilder/NLH/NLH-html-20130925-01/output/',File,PathToTextFile)},
    (({sub_string(PathToTextFile,_,_,_,'.txt')})
    ->
    {
    read_file_to_string(PathToTextFile, TextString, []),
    isub(TextString, X, true, Similarity)},
        (({Similarity > 0.3})
        -> html(tr([
            td(File),
            td(Similarity),
            td('..utdrag av teksten..')
        ])),
        do_file(Files, X)
        ; do_file(Files, X))
    ; do_file(Files, X)).

   %sub_string(TextString,_,_,_,X),

interaksjoner(Q1,Q2,Hit1,Hit2,Interaksjonsmekanisme,Konsekvens,Relevans,Handtering) :-
    rdf(_,interaksjon,I,fest),
    % Følgende søker i de to substansgruppene som interaksjonen omtaler.
    ((rdf(I,substansgruppe,S,fest),
    rdf(I,substansgruppe,S2,fest),
    dif(S,S2),
    rdf(S,_,literal(substring(Q1),Hit1),fest),
    rdf(S2,_,literal(substring(Q2),Hit2),fest))
    -> true
    % Følgende søker etter substringer av søkeparameteret i teksten til interaksjonoppføringen.
    ;
    Q1 == Q2,
    rdf(I,_,literal(substring(Q1),_),fest),
    Hit1 = Q1, Hit2 = Q2
    ),
    rdf(I,interaksjonsmekanisme,literal(Interaksjonsmekanisme),fest),
    rdf(I,kliniskKonsekvens,literal(Konsekvens),fest),
    rdf(I,relevans,literal(Relevans),fest),
    rdf(I,handtering,literal(Handtering),fest).

legemiddelmerkevare(Input,ATC,Navn,Preparatomtale,Produktinfo,AdminVei,EnhetDosering,Bruk) :-
    rdf(_,legemiddelMerkevare,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest),
    rdf(L,preparatomtaleavsnitt,literal(Preparatomtale)),
    rdf(L,produktInfo,Produktinfo),
    rdf(L,administreringLegemiddel,A),
    rdf(A,administrasjonsvei,AdminVei),
    rdf(A,enhetDosering,EnhetDosering),
    (
    (rdf(A,bruksomradeEtikett,Bruk))
    -> true
    ; Bruk = "ukjent"
    ).

legemiddelvirkestoff(Input,Navn,ATC,Reseptgruppe,Adminvei,Enhetdosering,Kortdose) :-
    rdf(_,legemiddelVirkestoff,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest),
    rdf(L,reseptgruppe,Reseptgruppe),
    rdf(L,administreringLegemiddel,A),
    rdf(A,administrasjonsvei,Adminvei),
    rdf(A,enhetDosering,Enhetdosering),
    (
    (rdf(A,kortdose,Kortdose))
    -> true
    ; Kortdose = "ukjent"
    ).

legemiddeldose(Input,Navn,ATC,Preparattype,Reseptgruppe) :-
    rdf(_,legemiddeldose,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest),
    rdf(L,preparattype,Preparattype,fest),
    rdf(L,reseptgruppe,Reseptgruppe,fest).

legemiddelpakning(Input,Navn,ATC,Pakningstype,Mengde,Enhetpakning,Omtale) :-
    rdf(_,legemiddelpakning,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,atc,ATC,fest),
    rdf(L,navnFormStyrke,Navn,fest),
    rdf(L,pakningsinfo,P,fest),
    rdf(P,pakningstype,Pakningstype,fest),
    rdf(P,mengde,Mengde,fest),
    rdf(P,enhetPakning,Enhetpakning,fest),
    ((rdf(L,preparatomtaleavsnitt,Omtale))
    -> true
    ; Omtale = "ukjent"
    ).

medforbmatr(Input,Navn,LNavn,Adr,Tlf,Pr,Str,Produktnr) :-
    rdf(_,medForbMatr,L,fest),
    rdf(L,_,literal(substring(Input),_),fest),
    rdf(L,navn,Navn,fest),
    rdf(L,leverandor,LEV,fest),
    rdf(LEV,adresse,Adr,fest),
    rdf(LEV,navn,LNavn,fest),
    rdf(LEV,telefon,Tlf,fest),
    rdf(L,produktInfoVare,P),
    rdf(P,antPerPakning,Pr),
    rdf(P,enhetStorrelse,Str),
    rdf(P,produktNr,Produktnr).

drugbank(Input,Navn,Lenke) :-
    rdf(A,_,literal(substring(Input),_),atc),
    rdf(A,rdfs:label,Navn,atc),
    rdf(A,rdfs:seeAlso,Lenke,atc).

synonymer(Input,Navn,Synonyms) :-
    rdf(L,_,literal(substring(Input),_),icd10no),
    rdf(L,rdfs:label,Navn),
    rdf(L, 'http://research.idi.ntnu.no/hilab/ehr/ontologies/icd10no.owl#synonym', literal(Synonyms)).


