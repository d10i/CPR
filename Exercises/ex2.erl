-module(ex2).
-export([sum/1]).
-export([sum_interval/2]).
-export([create/1]).
-export([reverse_create/1]).
-export([print/1]).
-export([even_print/1]).

sum(0) ->
  0;
sum(N) ->
  N + sum(N-1).

sum_interval(N,M) ->
  if
    N < M  -> M + sum_interval(N,M-1);
	N == M -> N;
	N > M  -> error
  end.


create(N) ->
  create_sub(N,[]).

create_sub(0,X) ->
  X;

create_sub(N,X) ->
  create_sub(N-1,[N|X]).

reverse_create(0) ->
  [];

reverse_create(N) ->
  [N|reverse_create(N-1)].


print(N) ->
  print(1,N).
  
print(A,N) ->
  if
    A =< N -> io:format("~p~n", [A]), print(A+1,N);
	A > N  -> ok
  end.

even_print(N) ->
  even_print(1,N).
  
even_print(A,N) ->
  if
    A =< N -> print_if_even(A), even_print(A+1,N);
	A > N  -> ok
  end.

print_if_even(A) when A rem 2 == 0 ->
  io:format("~p~n", [A]);
print_if_even(_) ->
  ok.