-module(mutex_client).
-export([start/0,wait/0,signal/0]).
-export([init/0]).

start()  -> spawn(mutex_client, init, []).
wait()   -> call(wait).
signal() -> call(signal).

init() -> loop().

call(Message) -> mutex ! {self(), Message}.
  
loop() ->
  receive
    ok ->
      io:format("[~p] ok~n", [self()]),
	  loop()
  end.