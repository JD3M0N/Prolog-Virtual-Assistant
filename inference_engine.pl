% inference_engine.pl
:- [database].  % Load the knowledge base.

% Resolve queries for definitions
resolve_query(definition(Term, Answer), Answer) :-
    definition(Term, Answer).

% Resolve queries for inventors
resolve_query(inventor(Term, Answer), Answer) :-
    inventor(Term, Answer).

% Resolve queries for programming paradigms
resolve_query(programming_paradigms(Paradigms), Paradigms) :-
    programming_paradigms(Paradigms).

% Additional rules can be added here if needed.
