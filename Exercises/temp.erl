-module(temp).
-export([c2f/1]).
-export([f2c/1]).
-export([convert/1]).

f2c(F) ->
  (5*(F-32))/9.

c2f(C) ->
  ((9*C)/5)+32.
  
convert({c,C}) ->
  {f,c2f(C)};

convert({f,F}) ->
  {c,f2c(F)}.