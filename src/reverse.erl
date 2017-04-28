-module(reverse).

-include_lib("eunit/include/eunit.hrl").

-export([reverse/1]).

reverse(L) ->
  reverse (L,[]).


reverse([], E) -> E;

reverse([H | T], A) ->
  reverse(T, [H | A]).

%% Tests
reverse_empty_test() ->
  ?assertEqual(
     [],
     reverse:reverse([])
    ).

reverse_one_element_test() ->
  ?assertEqual(
     [1],
     reverse:reverse([1])
    ).

reverse_simple_list_test() ->
  ?assertEqual(
     [a, b, c],
     reverse:reverse([c, b, a])
    ).

reverse_string_test() ->
  ?assertEqual(
     "abcdef",
     reverse:reverse("fedcba")
    ).
