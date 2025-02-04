:- module(interface, [start/0]).
:- use_module(parser).
:- use_module(inference_engine).

start :-
    write('Welcome to the Virtual Assistant. Type your question:'), nl,
    read_line_to_codes(user_input, Input),
    atom_codes(Atom, Input),
    split_string(Atom, " ", "", WordsList),  % Lista de strings
    convert_tokens(WordsList, Tokens),        % Convertir a lista de átomos en minúsculas
    process(Tokens).

process(WordsList) :-
    (   parse_query(WordsList, Query) -> 
        (   resolve_query(Query, Answer)
        ->  write('Answer: '), write(Answer), nl, nl
        ;   write('Sorry, I do not understand your question.'), nl, nl
        )
    ;   write('Sorry, I do not understand your question.'), nl, nl
    ),
    start.
    
% Predicado auxiliar para convertir tokens
convert_tokens([], []).
convert_tokens([Str|RestStr], [Atom|RestAtoms]) :-
    atom_string(Temp, Str),
    downcase_atom(Temp, Atom),
    convert_tokens(RestStr, RestAtoms).
