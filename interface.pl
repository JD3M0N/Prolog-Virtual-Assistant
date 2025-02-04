:- module(interface, [start/0]).
:- use_module(parser).
:- use_module(inference_engine).

% Iniciar la interacción con el usuario
start :-
    write('Welcome to the Virtual Assistant. Type your question:'), nl,
    read_line_to_codes(user_input, Input),
    atom_codes(Atom, Input),
    split_string(Atom, " ", "", WordsList),  % Dividir entrada en palabras
    process(WordsList).

% Procesar la entrada del usuario
process(WordsList) :-
    parse_query(WordsList, Query),  % Convertir pregunta en consulta interna
    resolve_query(Query, Answer),   % Buscar respuesta en el motor de inferencia
    write('Answer: '), write(Answer), nl, nl,
    start.
