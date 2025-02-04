:- module(interface, [start/0]).
:- use_module(parser).
:- use_module(inference_engine).

start :-
    write('Welcome to the Virtual Assistant. Type your question:'), nl,
    read_line_to_codes(user_input, Input),
    process(Input).

process(Sentence) :-
    (   parse_query(Sentence, Query) -> 
        (   resolve_query(Query, Answer)
        ->  write('Answer: '), write(Answer), nl, nl
        ;   write('Sorry, I do not understand your question.'), nl, nl
        )
    ;   write('Sorry, I do not understand your question.'), nl, nl
    ),
    start.
    
