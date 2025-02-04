:- module(parser, [parse_query/2]).
:- use_module(library(dcg/basics)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado principal: parse_query/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_query/2 espera una lista de códigos (Input) y produce una Query.
parse_query(InputCodes, Query) :-
    % Convertir los códigos en un átomo
    atom_codes(Atom, InputCodes),
    % Separar el átomo en una lista de strings.
    % El primer argumento " " indica que separamos por espacios.
    % El tercer argumento ".,?!¡¿" indica los caracteres que se deben eliminar (paddings).
    split_string(Atom, " ", ".,?!¡¿", WordsList),
    % Convertir cada string a un átomo en minúsculas
    convert_tokens(WordsList, Tokens),
    % Aplicar la gramática sobre la lista de tokens
    phrase(question(Query), Tokens).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Conversión de tokens
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert_tokens(+ListOfStrings, -ListOfAtoms)
convert_tokens([], []).
convert_tokens([Str|RestStr], [Atom|RestAtoms]) :-
    downcase_atom(Str, Atom),
    convert_tokens(RestStr, RestAtoms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gramática de preguntas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Ejemplo 1: Consulta de definición
% "What is quantum computing?" se procesará como:
%   [what, is, quantum, computing] -> definition(quantum_computing, Answer)
question(definition(Term, Answer)) -->
    [what, is],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.



% Ejemplo 2: Consulta de inventor
% "Who invented computer?" se procesará como:
%   [who, invented, computer] -> inventor(computer, Answer)
question(inventor(Term, Answer)) -->
    [who, invented],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.



% Ejemplo 3: Consulta sobre paradigmas de programación
% "What are the programming paradigms?" se procesará como:
%   [what, are, the, programming, paradigms] -> programming_paradigms(Answer)
question(programming_paradigms(Answer)) -->
    [what, are, the, programming, paradigms],
    { Answer = _ }.



% Ejemplo 4: Consulta comparativa de definiciones
% "compare definitions algorithm and data structure" se procesa como:
%   [compare, definitions, algorithm, and, data, structure] -> compare_definitions(algorithm, data_structure)
question(compare_definitions(Term1, Term2)) -->
    [compare, definitions],
    subject_sequence(Tokens1),
    [and],
    subject_sequence(Tokens2),
    { atomic_list_concat(Tokens1, '_', Term1),
      atomic_list_concat(Tokens2, '_', Term2) }.     



question(definition(Term, Answer)) -->
    ignore_until_specific_word,
    key_term_def,
    [of],
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auxiliares para la gramática
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recolecta una secuencia de tokens que conforman un término (palabra o conjunto de palabras).
subject_sequence([Word]) --> [Word].
subject_sequence([Word|Rest]) --> [Word], subject_sequence(Rest).

% Permite artículos opcionales: a, an, the
optional_article --> [a] | [an] | [the] | [].

% Regla para ignorar cualquier palabra antes de "subject(Term)"
ignore_until_specific_word --> [_], ignore_until_specific_word.
ignore_until_specific_word --> [].  % Caso base: detenerse si ya se encontró el subject

% Terminos unificados de definition
key_term_def --> [concept].
key_term_def --> [definition].
key_term_def --> [meaning].
