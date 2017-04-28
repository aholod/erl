-module(flatten).

-include_lib("eunit/include/eunit.hrl").
-export([flatten/1]).
-export([flatten2/1]).

flatten([]) ->
  [];
flatten([H | T]) ->
  flatten(H) ++ flatten(T);
flatten(X) ->
  [X].


%% ------------------------------
flatten2(L) -> flatten2(L, []).

flatten2([H | T], A) ->
  flatten2(H, flatten2(T, A));

flatten2([], A) ->
  A;

flatten2(E, A) ->
  [E | A].

%%  Tests -----------------------------

flatten_test_() ->
  [
   ?_assertEqual([],flatten([])),
   ?_assertEqual([1], flatten([1])),
   ?_assertEqual([1,2], flatten([1,2])),
   ?_assertEqual([1], flatten([[1]])),
   ?_assertEqual([1,2], flatten([1,[2]])),
   ?_assertEqual([a,b,c], flatten([[a],[[b],[c]]]))
  ].
flatten2_test_() ->
  [
   ?_assertEqual([],flatten2([])),
   ?_assertEqual([1], flatten2([1])),
   ?_assertEqual([1,2], flatten2([1,2])),
   ?_assertEqual([1], flatten2([[1]])),
   ?_assertEqual([1,2], flatten2([1,[2]])),
   ?_assertEqual([a,b,c], flatten2([[a],[[b],c]]))
  ].
