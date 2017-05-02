-module(mzip).

-include_lib("eunit/include/eunit.hrl").

-export([zip/2]).
-export([zipa/1]).

%%zip([], []) ->
%%  [];
zipa([]) ->
  [];
zipa([H | []]) ->
  H;
zipa([H | T]) ->
  %% do something with H;
  zipa(T).



zip([ Ah | At], [ Bh | Bt ]) ->
  [{ Ah, Bh } | zip(At, Bt)];

zip(_, _) ->
  [].

