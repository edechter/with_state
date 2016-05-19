



%% Project Imports
%% ===============
:- use_module('../prolog/with_state.pl').

%% Library Imports
%% ===============
:- use_module(library(plunit)).
:- use_module(library(record)).

:- initialization run_tests.

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

double_a(my_state(X, Y), my_state(X1, Y)) :-    
    X1 is 2 * X.

go -->
    with_state(double_a),
    put(my_state, b, 3).

test('call goal with_state', [State == my_state(2, 3)]) :-
    default_my_state(State0),
    phrase(go, [State0], [State]).



:- end_tests(with_state).
        
