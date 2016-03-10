



%% Project Imports
%% ===============
:- use_module('../prolog/with_state.pl').

%% Library Imports
%% ===============
:- use_module(library(plunit)).
:- use_module(library(record)).


:- begin_tests(with_state).

:- record my_state(a:integer=1, b).


test('modify single field', true(A == 2)) :-
        default_my_state(State0), 
        run_state(State0, _State,
                  (
                   put(my_state, a, 2),
                   get(my_state, a, A)
                  )
                 ).

test('modify multiple fields', [true(A-B == 2-some(thing))]):- 
        default_my_state(State0), 
        run_state(State0, _State,
                  (
                   puts(my_state, [a(2), b(some(thing))]),
                   gets(my_state, [a(A), b(B)])
                  )
                 ).

test('modify multiple times', [true(A-B == 3-some(thing))]):- 
        default_my_state(State0), 
        run_state(State0, _State,
                  (
                   puts(my_state, [a(2), b(some(thing))]),
                   gets(my_state, [a(_), b(_)]),
                   puts(my_state, [a(3)]),
                   gets(my_state, [a(A), b(B)])
                  )
                 ).

test('throws error if field does not exist', throws(error(existence_error(_, _), _))) :-
        default_my_state(State0),
        run_state(State0, _State,
                   gets(my_state, [a(_), c(_)])).



:- end_tests(with_state).
        