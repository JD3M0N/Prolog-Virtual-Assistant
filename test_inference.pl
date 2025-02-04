:- [inference_engine].

:- begin_tests(inference_engine).

% --------------------------------------------------------------------
% Tests for Definitions
% --------------------------------------------------------------------

test(definition_algorithm) :-
    resolve_query(definition(algorithm, X), X),
    assertion(X == 'An algorithm is a finite sequence of instructions to solve a problem.').

test(definition_data_structure) :-
    resolve_query(definition(data_structure, X), X),
    assertion(X == 'A data structure is a specialized format for organizing and storing data.').

test(definition_compiler) :-
    resolve_query(definition(compiler, X), X),
    assertion(X == 'A compiler is a program that translates source code written in a programming language into machine code that can be executed by a computer.').

test(definition_operating_system) :-
    resolve_query(definition(operating_system, X), X),
    assertion(X == 'An operating system is system software that manages computer hardware and software resources and provides common services for computer programs.').

% --------------------------------------------------------------------
% Tests for Inventors
% --------------------------------------------------------------------

test(inventor_computer) :-
    resolve_query(inventor(computer, X), X),
    assertion(X == 'Charles Babbage').

test(inventor_world_wide_web) :-
    resolve_query(inventor(world_wide_web, X), X),
    assertion(X == 'Tim Berners-Lee').

% --------------------------------------------------------------------
% Test for Programming Paradigms
% --------------------------------------------------------------------

test(programming_paradigms) :-
    resolve_query(programming_paradigms(X), X),
    assertion(X == ['imperative', 'functional', 'logical', 'object-oriented']).

% --------------------------------------------------------------------
% Test for Relationships between Concepts
% --------------------------------------------------------------------
% For example, verifying that the 'sorting' algorithm can be implemented using
% both the imperative and object-oriented paradigms.

test(algorithm_paradigm_sorting) :-
    findall(P, algorithm_paradigm(sorting, P), Paradigms),
    sort(Paradigms, Sorted),
    assertion(Sorted == [imperative, object_oriented]).

% --------------------------------------------------------------------
% Test for Composite Query / Inference Rule
% --------------------------------------------------------------------
% Testing the compare_definitions/3 predicate

test(compare_definitions) :-
    compare_definitions(algorithm, data_structure, Answer),
    Expected = "Definition of algorithm: An algorithm is a finite sequence of instructions to solve a problem.\nDefinition of data_structure: A data structure is a specialized format for organizing and storing data.",
    assertion(Answer == Expected).

:- end_tests(inference_engine).
