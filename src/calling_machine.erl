-module(calling_machine).

-include_lib("eunit/include/eunit.hrl").
-export([start/1]).
-export([stop/1]).
-export([init/0]).
-export([phone/1]).
-export([supervisor/1]).
-export([mon/1]).

phone(Number) ->
  io:format("Phone ~B was created (~p)~n", [Number, self()]),
  phone(#{"phone" => created}, Number, []).

phone(#{"phone" := created}, Number, []) ->

  receive
    {init, Pids} ->
      phone(#{"phone" => inited}, Number, lists:filter(fun(X) -> X /= self() end, Pids));
    {stop} ->
      io:format("Phone ~B was stopped (~p)~n", [Number, self()])
  end;

phone(#{"phone" := inited}, Number, Pids) ->
  io:format("Phone ~B was inited (~p)~n", [Number, self()]),
  receive
    {start} ->
      phone(#{"phone" => idle}, Number, Pids);

    {stop} ->
      io:format("Phone ~B was stopped (~p)~n", [Number, self()])
  end;

phone(#{"phone" := idle}, Number, Pids) ->
  io:format("Phone ~B is ready for calling (~p)~n", [Number, self()]),
  receive
    {incoming, Pid} ->
      io:format("Phone ~B got call from ~p (~p)~n", [Number, Pid, self()]),
      phone(#{"phone" => connected, "caller" => Pid}, Number, Pids);

    {stop} ->
      io:format("Phone ~B was stopped (~p)~n", [Number, self()])
  after
    3000 ->
      phone(#{"phone" => calling}, Number, Pids)
  end;

phone(#{"phone" := calling}, Number, Pids) ->
  CallingPid = lists:nth(rand:uniform(length(Pids)), Pids),
  CallingPid ! {incoming, self()},
  io:format("Phone ~B is calling ~p (~p)~n", [Number, CallingPid, self()]),
  receive
    {answer, Pid = CallingPid} ->
      io:format("Phone ~B got answer from ~p (~p)~n", [Number, Pid, self()]),
      phone(#{"phone" => busy}, Number, Pids);
    {stop} ->
      io:format("Phone ~B was stopped (~p)~n", [Number, self()])
  after
    500 ->
      io:format("Phone ~B didn't get answer from ~p (~p)~n", [Number, CallingPid, self()]),
      phone(#{"phone" => idle}, Number, Pids)
  end;

phone(#{"phone" := connected, "caller" := Pid}, Number, Pids) ->
  Pid ! {answer, self()},
  io:format("Phone ~B is connected with ~p (~p)~n", [Number, Pid, self()]),
  phone(#{"phone" => busy}, Number, Pids);

phone(#{"phone" := busy}, Number, Pids) ->
  receive
    {stop} ->
      io:format("Phone ~B was stopped (~p)~n", [Number, self()])
  after
    3000 ->
      io:format("Phone ~B is busy (~p)~n", [Number, self()]),
      phone(#{"phone" => idle}, Number, Pids)
  end.


%%------------------------
start(N) ->
  {CM, Mon} = init(),
  CM ! {init, N},
  CM ! {start},
  {CM, Mon}.

stop({CM, Mon}) ->
  CM ! { destroy },
  Mon ! { destroy }.

init() ->
  Mon = spawn_link(calling_machine, mon, [none]),
  CM = spawn_link(calling_machine, supervisor, [Mon]),
  {CM, Mon}.

supervisor(Mon) ->

  Mon ! { set_state, created },
  io:format("Calling machine created. Waiting for init or destroy msg...~n"),
  receive

    {init, PhonesNum} ->
      io:format("Initializing the calling machine...~n"),
      Pids = [ spawn_link(calling_machine, phone, [ X ]) || X <- lists:seq(1, PhonesNum) ],
      [ Pid ! {init, Pids} || Pid <- Pids ],
      supervisor(#{"sv" => initialized}, Pids, Mon);

    {destroy} ->
      destroy([]),
      Mon ! { set_state, destroyed }

  end.

supervisor(#{"sv" := initialized}, Pids, Mon) ->

  Mon ! {set_state, initialized},

  io:format("Calling machine initialized. Waiting for start or destroy msg...~n"),
  receive

    {start} ->
      io:format("Starting the calling machine...~n"),
      [ Pid ! {start} || Pid <- Pids ],
      supervisor(#{"sv" => started}, Pids, Mon);

    {destroy} ->
      destroy(Pids),
      Mon ! { set_state, destroyed }

  end;

supervisor(#{"sv" := started}, Pids, Mon) ->
  Mon ! { set_state, started },
  io:format("Calling machine started. Waiting for destroy msg...~n"),
  receive

    {destroy} ->
      destroy(Pids),
      Mon ! { set_state, destroyed }

  end.


%% -------
destroy(Pids) ->
  io:format("Destroying the calling machine...~n"),
  [ Pid ! {stop} || Pid <- Pids ].

%% monitor
mon(State) ->
  receive
    {set_state, NewState } ->
      mon(NewState);

    {get_state, Pid} ->
      Pid ! State,
      mon(State);

    {destroy} -> destroy
  end.

get_state(Mon) ->
  Mon ! {get_state, self()},
  receive
    CurrentState ->
      CurrentState
  end.

%% tests
%%

cm_create_destroy_test() ->
  {CM, Mon} = init(),
  ?assert(is_process_alive(CM)),
  ?assert(is_process_alive(Mon)),
  timer:sleep(1),
  ?assertEqual(created, get_state(Mon)),

  CM ! {destroy},
  timer:sleep(1),
  ?assertEqual(destroyed, get_state(Mon)).

cm_create_init_destroy_test() ->
  {CM, Mon} = init(),
  ?assert(is_process_alive(CM)),
  ?assert(is_process_alive(Mon)),
  timer:sleep(1),
  ?assertEqual(created, get_state(Mon)),

  CM ! {init, 4},
  timer:sleep(1),
  ?assertEqual(initialized, get_state(Mon)),

  CM ! {destroy},
  timer:sleep(1),
  ?assertEqual(destroyed, get_state(Mon)).



cm_create_init_start_destroy_test() ->
  {CM, Mon} = init(),
  ?assert(is_process_alive(CM)),
  ?assert(is_process_alive(Mon)),
  timer:sleep(1),
  ?assertEqual(created, get_state(Mon)),

  CM ! {init, 4},
  timer:sleep(1),
  ?assertEqual(initialized, get_state(Mon)),

  CM ! {start},
  timer:sleep(1),
  ?assertEqual(started, get_state(Mon)),

  CM ! {destroy},
  timer:sleep(1),
  ?assertEqual(destroyed, get_state(Mon)).


