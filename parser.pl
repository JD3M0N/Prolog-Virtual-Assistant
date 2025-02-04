:- module(parser, [parse_query/2]).
:- use_module(library(dcg/basics)).

% Gramática para preguntas del tipo "What is X?" o "What is an X?"
question(definition(Term, _)) --> [what, is], optional_article, subject(Term).

question(definition(Term, _)) --> ignore_until_specific_word, subject(Term), key_term.

question(definition(Term, _)) --> ignore_until_specific_word ,key_term, [of], subject(Term).

% Gramática para preguntas sobre inventores: "Who invented X?"
question(inventor(Term, _)) --> [who, invented], optional_article, subject(Term).

% Gramática para paradigmas de programación: "What are the programming paradigms?"
question(programming_paradigms(_)) --> [what, are, the, programming, paradigms].

% Permitir artículos opcionales (para manejar "a", "an", "the")
optional_article --> [a] | [an] | [the] | [].

% Regla para ignorar cualquier palabra antes de "subject(Term)"
ignore_until_specific_word --> [_], ignore_until_specific_word.
ignore_until_specific_word --> [].  % Caso base: detenerse si ya se encontró el subject

% Definir términos conocidos en la base de conocimientos
subject(algorithm) --> [algorithm].
subject(artificial_intelligence) --> [artificial, intelligence].
subject(computer) --> [computer].

key_term --> [concept].
key_term --> [definition].
key_term --> [meaning].

% Predicado principal para analizar la frase
parse_query(Sentence, Query) :-
    phrase(question(Query), Sentence).
