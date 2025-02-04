% main.pl
:- [parser, inference_engine, interface].  % Load the necessary files.

% Predicate to process a query
process_question(Pregunta, RespuestaFinal) :-
    parse_question(Pregunta, ConsultaInterna), % Parse the question
    resolve_query(ConsultaInterna, RespuestaFinal).
