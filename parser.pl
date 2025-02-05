:- module(parser, [parse_query/2]).
:- use_module(library(dcg/basics)).
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
    [the],
    [Token1, Token2, Token3, Token4],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3, Token4], '_', Term),
      Answer = _ }.

% Variante 2: Subject compuesto por tres tokens.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1, Token2, Token3],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3], '_', Term),
      Answer = _ }.      

% Variante 3: Subject compuesto por dos tokens.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1, Token2],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2], '_', Term),
      Answer = _ }.

% Variante 4: Subject de un solo token.
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token],
    key_term_def,
    ignore_until_specific_token,
    { atomic_list_concat([Token], '_', Term),
      Answer = _ }.

% Cuando aparece means al principio
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [means],
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.            

% Pregunta solo con what is 
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [what, is],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.

% Pregunta solo con whats
question(definition(Term, Answer)) -->
    ignore_until_specific_token,
    [whats],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.


% INVENTOR
%%%%%%%%%%

% Cuando aparece la palabra clave de inventor al principio
question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    key_term_inv0,
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.

question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    key_term_inv0,
    [of],
    optional_article,
    subject_sequence(Tokens),
    { atomic_list_concat(Tokens, '_', Term),
      Answer = _ }.      

% Variante 1
question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1, Token2, Token3, Token4],
    key_term_inv0,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3, Token4], '_', Term),
      Answer = _ }.      

% Variante 2
question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1, Token2, Token3],
    key_term_inv0,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2, Token3], '_', Term),
      Answer = _ }.

% Variante 3
question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1, Token2],
    key_term_inv0,
    ignore_until_specific_token,
    { atomic_list_concat([Token1, Token2], '_', Term),
      Answer = _ }.      

% Variante 4
question(inventor(Term, Answer)) -->
    ignore_until_specific_token,
    [the],
    [Token1],
    key_term_inv0,
    ignore_until_specific_token,
    { atomic_list_concat([Token1], '_', Term),
      Answer = _ }.      



% PARADIGMS
%%%%%%%%%%%

question(programming_paradigms(Answer)) -->
    ignore_until_specific_token,
    [programming, paradigms],
    { Answer = _ }.



% COMPARE
%%%%%%%%%

question(compare_definitions(Term1, Term2)) -->
    ignore_until_specific_token,
    key_term_cd,
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


% Terminos unificados de definition
key_term_def --> [concept].
key_term_def --> [definition].
key_term_def --> [meaning].
key_term_def --> [means].

% Terminos unificados de inventor
key_term_inv0 --> [invented].   
key_term_inv0 --> [created].
key_term_inv0 --> [discovered].
key_term_inv0 --> [creator].
key_term_inv0 --> [inventor].
key_term_inv0 --> [made].

% Unificados de compare definitions
key_term_cd --> [compare, definitions].
key_term_cd --> [compare].
key_term_cd --> [comparison, between].
key_term_cd --> [compare, between].