:- module(parser, [parse_query/2]).
:- use_module(library(dcg/basics)).
:- use_module(database, [definition/2]).
:- use_module(library(pcre)).  % Para usar re_replace/4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado principal: parse_query/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parse_query/2 espera una lista de códigos (Input) y produce una Query.
parse_query(InputCodes, Query) :-
    % Convertir los códigos en un átomo
    atom_codes(Atom, InputCodes),
    re_replace("[\\.,\\?!¡'¿]" / g, "", Atom, CleanAtom),
    split_string(CleanAtom, " ", "", WordsList),
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

% DEFINITION
%%%%%%%%%%%%

% Cuando aparece la palabra clave de definicion al principio
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    key_term_def,
    [of],
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.

% Variante 1: Subject compuesto por cuatro tokens.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [Token1, Token2, Token3, Token4],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3, Token4], '_', Candidate),
      definition_exists(Candidate),  % Verifica que el candidato exista en la base de datos.
      Term = Candidate,
      Answer = _ }.

% Variante 2: Subject compuesto por tres tokens.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [Token1, Token2, Token3],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3], '_', Candidate),
      definition_exists(Candidate),  % Verifica que el candidato exista en la base de datos.
      Term = Candidate,
      Answer = _ }.      

% Variante 3: Subject compuesto por dos tokens.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [Token1, Token2],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2], '_', Candidate),
      definition_exists(Candidate),  % Verifica que el candidato exista en la base de datos.
      Term = Candidate,
      Answer = _ }.

% Variante 4: Subject de un solo token.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [Token],
    key_term_def,
    ignore_until_specific_token,
    { definition_exists(Token),
      Term = Token,
      Answer = _ }.

% Cuando aparece means al principio
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [means],
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.            

question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [what, is],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.

question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [whats],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.


% INVENTOR
%%%%%%%%%%

question(inventor(Term, Answer)) -->
    [who, invented],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.



% PARADIGMS
%%%%%%%%%%%

question(programming_paradigms(Answer)) -->
    [what, are, the, programming, paradigms],
    { Answer = _ }.



% COMPARE
%%%%%%%%%

question(compare_definitions(Term1, Term2)) -->
    [compare, definitions],
    subject_sequence(Tokens1),
    [and],
    subject_sequence(Tokens2),
    { atomic_list_concat(Tokens1, '_', Term1),
      atomic_list_concat(Tokens2, '_', Term2) }.     


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Auxiliares para la gramática
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recolecta una secuencia de tokens que conforman un término (palabra o conjunto de palabras).
subject_sequence([Word]) --> [Word].
subject_sequence([Word|Rest]) --> [Word], subject_sequence(Rest).

% Permite artículos opcionales: a, an, the
optional_article --> [a] | [an] | [the] | [].

% Regla para ignorar tokens hasta encontrar uno especifico
ignore_until_specific_token --> [_], ignore_until_specific_token.
ignore_until_specific_token --> [].  % Caso base: detenerse si ya se encontró el subject

% Verifica si existe una cláusula definition(Term, _) en la base de datos.
definition_exists(Term) :-
    definition(Term, _), !.

% Terminos unificados de definition
key_term_def --> [concept].
key_term_def --> [definition].
key_term_def --> [meaning].
key_term_def --> [means].
