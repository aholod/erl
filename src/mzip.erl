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
  [Hh , Ht] =  head(H),
  [Hh | zipa(T)] ++ zipa(Ht).

head([Hh, Ht]) ->
  [Hh, Ht];
head(E) ->
  [E, []].


zip([ Ah | At], [ Bh | Bt ]) ->
  [{ Ah, Bh } | zip(At, Bt)];

zip(_, _) ->
  [].

