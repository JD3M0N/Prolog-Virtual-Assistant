% inference_engine.pl
:- module(inference_engine, [resolve_query/2]).
:- use_module(database).  % Load your database.pl

% --------------------------------------------------------------------
% Basic Queries
% --------------------------------------------------------------------

% Resolve a basic definition query.
resolve_query(definition(Term, Answer), Answer) :-
    definition(Term, Answer).

% Resolve a basic inventor query.
resolve_query(inventor(Term, Answer), Answer) :-
    inventor(Term, Answer).

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
