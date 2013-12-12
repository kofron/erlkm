%% @doc euclidian_vector is a very simple module which implements gen_metric.
%% @author Jared Kofron <jared.kofron.at.work@gmail.com>
-module(euclidian_vector).
-behavior(gen_metric).

%% gen_metric exports
-export([new/1, distance/2, add/2, subtract/2, scale/2]).

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

-spec list_add([number()], [number()]) -> [number()].
list_add(A, B) ->
    lists:zipwith(fun(X,Y) ->
			  X + Y
		  end,
		  A, B).

-spec list_subtract([number()], [number()]) -> [number()].
list_subtract(A, B) ->
    lists:zipwith(fun(X,Y) ->
			  X - Y
		  end,
		  A, B).

-spec list_scalar_mult([number()], float()) -> [number()].
list_scalar_mult(L, S) ->
    [S*X || X <- L].

-spec distance(vector(), vector()) -> float().
distance(#vec{data=A}, #vec{data=B}) ->
    lists:sum(lists:zipwith(fun sumsq/2,A,B)).

-spec add(vector(), vector()) -> vector().
add(#vec{data=A}, #vec{data=B}) ->
    #vec{data=list_add(A,B)}.

-spec subtract(vector(), vector()) -> vector().
subtract(#vec{data=A}, #vec{data=B}) ->
    #vec{data=list_subtract(A,B)}.

-spec scale(vector(), float()) -> vector().
scale(#vec{data=A}, S) ->
    #vec{data=list_scalar_mult(A, S)}.

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

add_test() ->
    V1 = #vec{data=[1,2,3]},
    V2 = #vec{data=[4,5,6]},
    V3 = ?MODULE:add(V1,V2),
    ?assertEqual(V3#vec.data, [5,7,9]).

subtract_test() ->
    V1 = #vec{data=[1,2,3]},
    V2 = #vec{data=[4,5,6]},
    V3 = ?MODULE:subtract(V1,V2),
    ?assertEqual(V3#vec.data, [-3,-3,-3]).

scale_test() ->
    V1 = #vec{data=[1,2,3]},
    S = 2.0,
    V2 = ?MODULE:scale(V1, S),
    ?assertEqual(V2#vec.data, [2.0,4.0,6.0]).

-endif.
