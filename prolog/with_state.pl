



:- module(with_state, [
                       state//1,  %% //1: ?Record
                       state//2,  %% //2: ?StateIn, ?StateOut
                       
                       run_state/3, %% +StateIn, -StateOut, +Goal
                       
                       mod_state//1, %% +Goal
                       op(950, fx, mod_state),
                       
                       gets//2,  %% +Constructor, +FieldValues 
                       puts//2, %% +Constructor, +FieldValues 
                       
                       get//3,  %% +Constructor, +Field, -Value 
                       put//3  %% +Constructor, +Field, -Value 
                       
                       ]).

/** <module> Stateful computations with records. 
  
  This module implements predicates and macros for easily threading
  records (as in, `library(record)`) as state.  
  
  @author  Eyal Dechter <eyaldechter@gmail.com>
  
*/


:- use_module(library(record)).
:- use_module(library(lists)).


%! state(?State:record)// is det
%
% @author Markus Triska 
%
% Predicate passes state using DCG semi-context notation.
%
state(S), [S] --> [S].


%! state(?StateIn:record, ?StateOut:record)// is semidet
%
% @author Markus Triska
%
state(S0, S), [S] --> [S0].



:- discontiguous expand/2.

          %%%%%%%%%%%%%%%%%%%%%%%%%%
          %% Dummy predicate defs %%          
          %%%%%%%%%%%%%%%%%%%%%%%%%%

%% Predicate definitions below for predicates that should be
%% goal_expanded. We throw a existence_error if the predicate is not
%% goal_expanded and remains in the source.

%!    run_state(+StateIn:record, ?StateOut:record, +Goal) is nondet
%
% Run stateful computation Goal with initial state StateIn and resulting state StateOut.
%
% NB: run_state is subject to goal expansion.
%
% ==
% go(State) :- 
%         default_my_state(State0), 
%         run_state(State0, State,
%                   (
%                    put(my_state, a, 2),
%                    get(my_state, a, A)
%                   )
% ==
run_state(_, _, _) :-
    throw(error(existence_error(procedure, run_state/3), context(run_state/3, should_be_goal_expanded))).


%!    mod_state(+Constructor: atom, :Goal)//
%
% Call =Goal= to modify state. Goal must have type =| State --> State
% |= . Equivalent to
%
% ==
%  state(State0, State),
%  { call(Goal, State0, State) }
% ==
%
% NB: mod_state is subject to goal expansion.
%
mod_state(_, _, _) :-
    throw(error(existence_error(procedure, 'mod_state/3'), context('mod_state/3', should_be_goal_expanded))).


expand(mod_state(Goal, In, Out), (In-Out=[StateIn|X]-[StateOut|X], Goal1)):-

    strip_module(Goal, Mod, Plain),
    (atom(Plain) ->
         Args = [],
         Name = Plain
     ;     
     compound_name_arguments(Plain, Name, Args)
    ),
    append(Args, [StateIn, StateOut], Args1),         
    Plain1 =.. [Name|Args1], 
    Goal1 = (Mod:Plain1).





%!    gets(+Constructor:atom, +FieldValues:list(field(value)))// is det
%
% NB: run_state is subject to goal expansion.
%
% Example:
% ==
% gets(my_state, [my_field(Value1), my_term(Value2)]
% ==
%
gets(_, _, _, _) :-
        throw(error(existence_error(procedure, gets/4), context(gets/4, should_be_goal_expanded))).

%!    puts(+Constructor:atom, +FieldValues:list(field(value)))// is det
%
% NB: run_state is subject to goal expansion.
%
% Example:
% ==
% puts(my_state, [my_int(1), my_term(a)]
% ==
%
puts(_, _, _, _) :-
        throw(error(existence_error(procedure, puts/4), context(puts/4, should_be_goal_expanded))).

%!    get(+Constructor:atom, +Field:atom, ?Value:value)// is det
%
% NB: run_state is subject to goal expansion.
%
% Example:
% ==
% get(my_state, my_int, Int)
% ==
%
get(_, _, _, _, _) :-
        throw(error(existence_error(procedure, get/5), context(get/5, should_be_goal_expanded))).

%!    put(+Constructor:atom, +Field:atom, +Value:value)// is det
%
% NB: run_state is subject to goal expansion.
%
% Example:
% ==
% put(my_state, my_int, 1)
% ==
%

put(_, _, _, _, _) :-
        throw(error(existence_error(procedure, put/5), context(put/5, should_be_goal_expanded))).

          %%%%%%%%%%%%%%%
          % Expansions %
          %%%%%%%%%%%%%%%

expand(run_state(StateIn, StateOut, Goal), Body) :- 
        
        Term = (go --> Goal),
        dcg_translate_rule(Term, Term1),
        Term1 = (go([StateIn], [StateOut|_]) :- Body).


expand(gets(Constructor, Pairs, Head, Tail), (state(Rec, Head, Tail), Terms)) :-
    prolog_load_context(module, M),
    expand_gets(Pairs, M, Constructor, Rec, Terms).

expand(puts(Constructor, Pairs, Head, Tail), (state(RecIn, RecOut, Head, Tail), Terms)) :-
        prolog_load_context(module, M),
        expand_puts(Pairs, M, Constructor, RecIn, RecOut, Terms).

expand(get(Constructor, Field, Value, Head, Tail), Goal) :-
    FieldValue =.. [Field, Value],
    expand(gets(Constructor, [FieldValue], Head, Tail), Goal).
              
expand(put(Constructor, Field, Value, Head, Tail), Goal) :-
        FieldValue =.. [Field, Value],
        expand(puts(Constructor, [FieldValue], Head, Tail), Goal).

              
expand_gets([], _, _, _, true).
expand_gets([Pair|Pairs], M, Constructor, Rec, (M:Term, Terms)) :-
        Pair =.. [Field, Value],
        mk_get_term(Constructor, Field, Value, Term, Rec),        
        expand_gets(Pairs, M, Constructor, Rec, Terms).


expand_puts([], _, _, _, _, true) :- !.
expand_puts([Pair], M, Constructor, RecIn, RecOut, M:Term) :-
        !,
        Pair =.. [Field, Value],
        mk_put_term(Constructor, Field, Value, Term, RecIn, RecOut).
expand_puts([Pair|Pairs], M, Constructor, RecIn, RecOut, (M:Term, Terms)) :-
        Pair =.. [Field, Value],
        %% Unlike in the case of expand_gets, we need to thread new
        %% record RecTmp to the next call.
        mk_put_term(Constructor, Field, Value, Term, RecIn, RecTmp),
        expand_puts(Pairs, M, Constructor, RecTmp, RecOut, Terms).


mk_get_term(Constructor, Field, Value, Term, Rec) :-
        atomic_list_concat([Constructor, '_', Field], Pred),
        Term =.. [Pred, Rec, Value].

mk_put_term(Constructor, Field, Value, Term, RecIn, RecOut) :-
        atomic_list_concat(['set_', Field, '_of_', Constructor], Pred),
        %% T = set_<Field>_of_<Constructor>(+Value, +OldRecord, -NewRecord)
        Term =.. [Pred, Value, RecIn, RecOut].
        

          %%%%%%%%%%%%%%%%%%%%%%%%%
          %% Goal Expansion Hook %%          
          %%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile
	user:goal_expansion/2.

user:goal_expansion(In, Out) :-
    expand(In, Out).
    %% format('In: ~p --> ~n    Out: ~p~n', [In, Out]).
    


          %%%%%%%
          % Aux %
          %%%%%%%


%!    list_to_conj(List, Conj) is det
%
list_to_conj([], true) :- !.
list_to_conj([X], X) :- !.
list_to_conj([X|Xs], (X, Ys)) :-
        list_to_conj(Xs, Ys).


