% inference_engine.pl
:- module(inference_engine, [resolve_query/2, resolve_query_with_fallback/2, resolve_query_with_fallback2/2]).
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

% Predicate to try to find the exact query 
exact_definition(Query, Answer) :-
(   string(Query)
->  atom_string(AtomQuery, Query)
;   AtomQuery = Query
),
definition(AtomQuery, Answer).

% Define split_atom/3 using atomic_list_concat/3
% Predicate function to : divide an atom into a list
split_atom(Atom, Separator, List) :-
    atom(Atom),  % Making sure it is an atom
    atomic_list_concat(List, Separator, Atom).

% Predicate to solve a query with error tolerance and progressive reduction
resolve_query_with_fallback(Query, Answer) :-
    ensure_string(Query, QueryString),  % Converting into string if needed
    split_string(QueryString, "_", "", Parts),  % Separating into a list of strings
    maplist(atom_string, PartsAtoms, Parts),  % Converting the list into atoms
    (   % First, try to find an exact match at any level.
        find_exact_match(PartsAtoms, Answer)
    ->  true
    ;   % If no exact match is found, proceed to approximate search.
        try_subqueries(PartsAtoms, Answer)
    ).


% Convert Query to string if it is an atom
ensure_string(Query, QueryString) :-
    (   string(Query) -> QueryString = Query   % If it is already a string, keep it
    ;   atom(Query) -> atom_string(Query, QueryString)  % Convert atom to string
    ).

% Search for an exact match at different levels (complete, last 3, 2, and 1 word) using exact_definition/2.
find_exact_match(Words, Answer) :-
    (   % Try with the complete query.
        atomic_list_concat(Words, '_', FullQuery),
        exact_definition(FullQuery, Answer)
    )
    ;
    (   % Try with the last three words (if there are at least 4 words).
        length(Words, N), N > 2,
        append(_, [W1, W2, W3 | _], Words),
        atomic_list_concat([W1, W2, W3], '_', SubQuery3),
        exact_definition(SubQuery3, Answer)
    )
    ;
    (   % Try with the last two words (if there are at least 2).
        length(Words, N), N > 1,
        append(_, [W1, W2 | _], Words),
        atomic_list_concat([W1, W2], '_', SubQuery2),
        exact_definition(SubQuery2, Answer)
    )
    ;
    (   % Try with the last word.
        last(Words, LastWord),
        exact_definition(LastWord, Answer)
    ).

% Try to find definitions by progressively reducing the query
try_subqueries([Word], Answer) :-
    resolve_query(definition(Word, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    length(Words, N),
    N > 2,
    append(_, [W1, W2, W3 | _], Words), % Take the last three words
    atomic_list_concat([W1, W2, W3], '_', SubQuery),
    resolve_query(definition(SubQuery, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    length(Words, N),
    N > 1,
    append(_, [W1, W2 | _], Words), % Take the last two words
    atomic_list_concat([W1, W2], '_', SubQuery),
    resolve_query(definition(SubQuery, Answer), Answer),
    Answer \= 'No matching definition found.', !.

try_subqueries(Words, Answer) :-
    last(Words, LastWord), % Last individual word
    resolve_query(definition(LastWord, Answer), Answer).


% --------------------------------------------------------------------
% Auxiliary predicates for Inventors
% --------------------------------------------------------------------

% Search for an exact match for an inventor.
exact_inventor(Query, Answer) :-
(   string(Query)
->  atom_string(AtomQuery, Query)
;   AtomQuery = Query
),
inventor(AtomQuery, Answer).

% Try to find an exact match by progressively reducing the query (for inventors).
find_exact_inventor(Words, Answer) :-
(   % Try with the complete query.
    atomic_list_concat(Words, '_', FullQuery),
    exact_inventor(FullQuery, Answer)
)
;
(   % Try with the last three words (if there are at least 3).
    length(Words, N), N > 2,
    append(_, [W1, W2, W3 | _], Words),
    atomic_list_concat([W1, W2, W3], '_', SubQuery3),
    exact_inventor(SubQuery3, Answer)
)
;
(   % Try with the last two words (if there are at least 2).
    length(Words, N), N > 1,
    append(_, [W1, W2 | _], Words),
    atomic_list_concat([W1, W2], '_', SubQuery2),
    exact_inventor(SubQuery2, Answer)
)
;
(   % Try with the last word.
    last(Words, LastWord),
    exact_inventor(LastWord, Answer)
).

% Try to find inventors by progressively reducing the query
try_subqueries_inventor([Word], Answer) :-
resolve_query(inventor(Word, Answer), Answer),
Answer \= 'No matching inventor found.', !.

try_subqueries_inventor(Words, Answer) :-
length(Words, N),
N > 2,
append(_, [W1, W2, W3 | _], Words),  % Take the last three words
atomic_list_concat([W1, W2, W3], '_', SubQuery),
resolve_query(inventor(SubQuery, Answer), Answer),
Answer \= 'No matching inventor found.', !.

try_subqueries_inventor(Words, Answer) :-
length(Words, N),
N > 1,
append(_, [W1, W2 | _], Words),  % Take the last two words
atomic_list_concat([W1, W2], '_', SubQuery),
resolve_query(inventor(SubQuery, Answer), Answer),
Answer \= 'No matching inventor found.', !.

try_subqueries_inventor(Words, Answer) :-
last(Words, LastWord),  % Last individual word
resolve_query(inventor(LastWord, Answer), Answer).

% --------------------------------------------------------------------
% New function: resolve_query_with_fallback2 for inventors
% --------------------------------------------------------------------

resolve_query_with_fallback2(Query, Answer) :-
ensure_string(Query, QueryString),  % Convert the query to string if necessary.
split_string(QueryString, "_", "", Parts),  % Split the string using "_" as a separator.
maplist(atom_string, PartsAtoms, Parts),    % Convert the list of strings to atoms.
(   
    % First we try finding the exact query
    find_exact_inventor(PartsAtoms, Answer)
->  true
;   
    % If there is no exact match, we try to reduce the query
    try_subqueries_inventor(PartsAtoms, Answer)
).
