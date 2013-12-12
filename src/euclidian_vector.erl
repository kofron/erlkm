%% @doc euclidian_vector is a very simple module which implements gen_metric.
%% @author Jared Kofron <jared.kofron.at.work@gmail.com>
-module(euclidian_vector).
-behavior(gen_metric).

%% gen_metric exports
-export([new/1, distance/2]).

%% underlying data structure.
-record(vec, 
	{
	  data :: list(number()) %% the payload of the vec record
	}
       ).
-opaque vector() :: #vec{}.
-export_type([vector/0]).

-spec new(list({term(), term()})) -> {ok, vector()} | {error, {bad_size, term()}}.
new(PropList) ->
    Size = proplists:get_value(size, PropList, 3),
    case Size of
	X when is_integer(X) andalso X > 0 ->
	    {A, B, C} = erlang:now(),
	    _ = random:seed(A,B,C),
	    {ok, #vec{data=[random:uniform() || _ <- lists:seq(1,Size)]}};
	BadSize ->
	    {error, {bad_size, BadSize}}
    end.

-spec sumsq(number(), number()) -> float().
sumsq(X,Y) ->
    math:pow(X - Y, 2.0).

-spec distance(vector(), vector()) -> float().
distance(#vec{data=A}, #vec{data=B}) ->
    lists:sum(lists:zipwith(fun sumsq/2,A,B)).

%% EUNIT
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

good_new_test() ->
    {R, _V} = ?MODULE:new([{size, 10}]),
    ?assertEqual(R, ok).

bad_new_test() ->
    {R, _V} = ?MODULE:new([{size, abc}]),
    ?assertEqual(R, error).

equal_distance_is_zero_test() ->
    {ok, V} = ?MODULE:new([{size, 10}]),
    ?assertEqual(0.0, ?MODULE:distance(V,V)).

-endif.
