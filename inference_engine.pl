% inference_engine.pl
:- module(inference_engine, [resolve_query/2, resolve_query_with_fallback/2]).
:- use_module(database).  % Load your database.pl

% --------------------------------------------------------------------
% Basic Queries
% --------------------------------------------------------------------

% Resolve a basic definition query.

%resolve_query(definition(Term, Answer), Answer) :-
%    definition(Term, Answer).

% resolve_query(definition(Query, Answer), Answer) :-
%     (   % Convert Query to an atom if it's a string
%         (   string(Query)
%         ->  atom_string(AtomQuery, Query)
%         ;   AtomQuery = Query
%         ),
%         % First, try to find an exact match for the converted query
%         definition(AtomQuery, ExactAnswer)
%     ->  Answer = ExactAnswer
%     ;   % If no exact match is found, try to find an approximate match using the atom version
%         (   string(Query)
%         ->  atom_string(AtomQuery, Query)
%         ;   AtomQuery = Query
%         ),
%         approximate_definition(AtomQuery, ApproxAnswer)
%     ->  format(atom(Answer),
%            'No exact match found, but perhaps you meant "~w" which is: ~w',
%            [AtomQuery, ApproxAnswer])
%     ;   % If no match is found, return a default answer
%         Answer = 'No matching definition found.'
%     ).

resolve_query(definition(Query, Answer), Answer) :-
    (   % Convert Query to an atom if it's a string
        (   string(Query)
        ->  atom_string(AtomQuery, Query)
        ;   AtomQuery = Query
        ),
        % First, try to find an exact match for the converted query
        definition(AtomQuery, ExactAnswer)
    ->  Answer = ExactAnswer
    ;   % If no exact match is found, try to find an approximate match using the atom version
        (   string(Query)
        ->  atom_string(AtomQuery, Query)
        ;   AtomQuery = Query
        ),
        approximate_definition2(AtomQuery, BestMatch, ApproxAnswer)
    ->  format(atom(Answer),
           'No exact match found, but perhaps you meant "~w" which is: ~w',
           [BestMatch, ApproxAnswer])
    ;   % If no match is found, return a default answer
        Answer = 'No matching definition found.'
    ).

% Resolve a basic inventor query.

% resolve_query(inventor(Term, Answer), Answer) :-
    % inventor(Term, Answer).

% resolve_query(inventor(Query, Answer), Answer) :-
%     % Convert Query to an atom if it's a string.
%     (   string(Query)
%     ->  atom_string(AtomQuery, Query)
%     ;   AtomQuery = Query
%     ),
%     (   % First, try to find an exact match.
%         inventor(AtomQuery, ExactAnswer)
%     ->  Answer = ExactAnswer
%     ;   % If no exact match is found, try to find an approximate match.
%         approximate_inventor(AtomQuery, ApproxAnswer)
%     ->  format(atom(Answer),
%            'No exact match found for inventor, but perhaps you meant "~w" which is: ~w',
%            [AtomQuery, ApproxAnswer])
%     ;   % If no match is found, return a default answer.
%         Answer = 'No matching inventor found.'
%     ).


% Resolve a basic inventor query.
resolve_query(inventor(Query, Answer), Answer) :-
    % Convert Query to an atom if it is a string.
    (   string(Query)
    ->  atom_string(AtomQuery, Query)
    ;   AtomQuery = Query
    ),
    (   % First, try to find an exact match.
        inventor(AtomQuery, ExactAnswer)
    ->  Answer = ExactAnswer
    ;   % If no exact match is found, try to find an approximate match.
        approximate_inventor2(AtomQuery, BestMatch, ApproxAnswer)
    ->  format(atom(Answer),
           'No exact match found for inventor, but perhaps you meant "~w" which is: ~w',
           [BestMatch, ApproxAnswer])
    ;   % If no match is found, return a default answer.
        Answer = 'No matching inventor found.'
    ).


% Resolve a query for programming paradigms.
resolve_query(programming_paradigms(Paradigms), Paradigms) :-
    programming_paradigms(Paradigms).

% --------------------------------------------------------------------
% Composite Queries / Inference Rules
% --------------------------------------------------------------------

% Compare two definitions (e.g., compare hardware and software).
% This rule extracts the definitions of both terms and returns a formatted answer.
resolve_query(compare(hardware, software), Answer) :-
    definition(hardware, HardwareDef),
    definition(software, SoftwareDef),
    format(string(Answer),
           'Hardware: ~w~nSoftware: ~w',
           [HardwareDef, SoftwareDef]).

% A more generic composite query that compares definitions for any two terms.
% Example usage:
% ?- resolve_query(compare_definitions(algorithm, data_structure), Answer).
resolve_query(compare_definitions(Term1, Term2), Answer) :-
    definition(Term1, Def1),
    definition(Term2, Def2),
    format(string(Answer),
           'Definition of ~w: ~w~nDefinition of ~w: ~w',
           [Term1, Def1, Term2, Def2]).

% --------------------------------------------------------------------
% Handling Multiple Responses
% --------------------------------------------------------------------

% For queries that require multiple answers, such as retrieving all paradigms 
% in which an algorithm can be implemented.
% Example usage:
% ?- resolve_query(algorithm_paradigm(sorting), ParadigmList).
resolve_query(algorithm_paradigm(Algorithm), ParadigmList) :-
    findall(P, algorithm_paradigm(Algorithm, P), ParadigmList).

% If there are cases where an entity might have multiple inventors, you can use:
% Example usage:
% ?- resolve_query(inventor_all(computer), Inventors).
resolve_query(inventor_all(Entity), Inventors) :-
    findall(Inv, inventor(Entity, Inv), Inventors).

% --------------------------------------------------------------------
% Integration Interface
% --------------------------------------------------------------------
%
% This is the primary interface predicate that your parser will call.
% The parser should generate queries in one of the formats that resolve_query/2 can handle.
%
% For example, a parsed query could be one of:
%   - definition(algorithm, X)
%   - inventor(computer, X)
%   - programming_paradigms(X)
%   - compare_definitions(algorithm, data_structure)
%   - algorithm_paradigm(sorting)
%
% The resolve_query/2 predicate will then match the appropriate clause and
% return the answer(s) accordingly.

% --------------------------------------------------------------------
% Other Functions
% --------------------------------------------------------------------


% Define split_atom/3 using atomic_list_concat/3
% Predicado auxiliar: divide un átomo en una lista de partes
split_atom(Atom, Separator, List) :-
    atom(Atom),  % Asegurar que es un átomo antes de convertir
    atomic_list_concat(List, Separator, Atom).

% Predicado para resolver una consulta con tolerancia a errores y reducción progresiva
resolve_query_with_fallback(Query, Answer) :-
    ensure_string(Query, QueryString),  % Convertir a string si es necesario
    split_string(QueryString, "_", "", Parts),  % Separar en lista de strings
    maplist(atom_string, PartsAtoms, Parts),  % Convertir lista de strings a átomos
    try_subqueries(PartsAtoms, Answer).

% Convierte Query a string si es un átomo
ensure_string(Query, QueryString) :-
    (   string(Query) -> QueryString = Query   % Si ya es string, mantenerlo
    ;   atom(Query) -> atom_string(Query, QueryString)  % Convertir átomo a string
    ).

% Intenta buscar definiciones reduciendo progresivamente la consulta
try_subqueries([Word], Answer) :-
    resolve_query(definition(Word, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    length(Words, N),
    N > 2,
    append(_, [W1, W2, W3 | _], Words), % Toma las tres últimas palabras
    atomic_list_concat([W1, W2, W3], '_', SubQuery),
    resolve_query(definition(SubQuery, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    length(Words, N),
    N > 1,
    append(_, [W1, W2 | _], Words), % Toma las dos últimas palabras
    atomic_list_concat([W1, W2], '_', SubQuery),
    resolve_query(definition(SubQuery, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    last(Words, LastWord), % Última palabra individual
    resolve_query(definition(LastWord, Answer), Answer).
