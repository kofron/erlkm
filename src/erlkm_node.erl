%% @doc erlkm_node defines a node in a kohonen map.  this is the real workhorse
%% module for erlkm.  a node is *not* an abstraction at the level of a process!
%% really it is a glorified data structure module which is updated through 
%% train/2 and score/2.
%% @author Jared Kofron <jared.kofron@gmail.com>
-module(erlkm_node).

-record(node_data,
	{
	  data_mod :: atom(), %% the name of the callback module for the data 
		              %% stored at this node.
	  data :: term() %% the data which is used for comparisons.
	}
       ).
-opaque node_data() :: #node_data{}.

-export_type([node_data/0]).
-export([new/2, score/2, update/3]).

%% @doc new/2 (invoked as new(N, D)) constructs a new node with its data
%% initialized to M:new(D).  If the call fails, this constructor of course
%% also fails.
%% @spec new(atom(), [{term(), term()}]) -> {ok, node_data()} | {error, term()}.
-spec new(atom(), [{term(), term()}]) -> 
		 {ok, node_data()} 
		     | {error, term()}.
new(ModuleName, ModuleData) ->
    case ModuleName:new(ModuleData) of
	{ok, NewData} ->
	    {ok, #node_data{data_mod=ModuleName, data=NewData}};
	{error, _Reason}=Failure ->
	    Failure
    end.

%% @doc score/2 (invoked as score(N, S)) returns the distance between 
%% the data stored at the node N and the sample S.  The type of S must match 
%% the type of the data stored at the node N to avoid tragic results.
%% @spec score(node_data(), term()) -> float().
-spec score(node_data(), term()) -> float().
score(#node_data{data_mod=M, data=D}, Sample) ->
    M:distance(D, Sample).

%% @doc update/3 (invoked as update(N, S, P)) updates the data structure
%% according to the formula D2 = D1 + P*(D1 - S), where D1 is the current
%% "weight vector" stored at the node N.  The implementation of the addition and
%% multiplication is defined in the callback module passed as an argument at
%% init/1.
%% @spec update(node_data(), term(), float()) -> node_data().
-spec update(node_data(), term(), float()) -> node_data().
update(#node_data{data_mod=M, data=D}=ND, Sample, Prefactor) ->
    ND#node_data{data=M:add(D, M:scale(M:subtract(D,Sample), Prefactor))}.
