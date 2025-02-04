% main.pl
:- [parser, inference_engine, interface].  % Load the necessary files.

% Predicate to process a query
process_query(UserInput, FinalAnswer) :-
    parse_query(UserInput, InternalQuery),  % Assuming parse_query/2 is defined in your parser module.
    resolve_query(InternalQuery, FinalAnswer).
