% ekm_node.erl
% a gen_fsm model of the neurons that make up the kohonen map.
% exposes an interface to learn or analyze an input.  the
% input is trusted in the sense that it will naively try to 
% compute the cartesian distance between the input and its
% state vector.
-module(ekm_node).
-behavior(gen_fsm).

% state record
-record(state, {x = 0, % x coordinate
		y = 0, % y coordinate
		v = [] % the state 'vector'
	       }
       ).

% Default length of vector
-define(DEFAULTLENGTH, 3).

% gen_fsm exports
-export([start_link/0,start_link/1,init/1,code_change/4, terminate/3,
	 handle_event/3, handle_sync_event/4,handle_info/3]).

% api exports
-export([ask/2, learn/2, switch_mode/2, set_coords/2]).

% states
-export([analyze/2, learning/2]).

%%%%%%%%%%%
%%% api %%%
%%%%%%%%%%%
-spec ask(pid(), {{pid(), reference()}, list()}) -> ok.
ask(P,{{D,R},L}) ->
    gen_fsm:send_event(P, {input,L,{D,R}}).

-spec learn(pid(), {bmu, integer(), integer(), float()}) -> ok.
learn(P,{bmu,_X,_Y,_L}=Data) ->
    gen_fsm:send_event(P, Data).		   

-spec switch_mode(pid(), atom()) -> ok.
switch_mode(P, learn) ->
    gen_fsm:send_all_state_event(P, learn);
switch_mode(P, analyze) ->
    gen_fsm:send_all_state_event(P, analyze).

-spec set_coords(pid(), {integer(), integer()}) -> ok.
set_coords(P, {X,Y}) ->
    gen_fsm:sync_send_all_state_event(P, {setcoords, {X,Y}}).

%%%%%%%%%%%%%%
%%% states %%%
%%%%%%%%%%%%%%
analyze({input, L, From}, St) ->
    Payload = {St#state.x, St#state.y, cartesian_distance(L, St#state.v)},
    gen_fsm:reply(From, Payload),
    {next_state, analyze, St}.

learning({bmu, X, Y, L}, State) ->
    NewState = acclimate({X,Y,L}, State),
    {next_state, learning, NewState}.

%%%%%%%%%%%%%%%%
%%% internal %%%
%%%%%%%%%%%%%%%%
-spec acclimate(list(), record()) -> record().
acclimate({X,Y,L}, State) -> % move each element of vector closer randomly
    Closer = lists:map( fun({X1,X2}) -> random:uniform()*(X1-X2) end,
			lists:zip(L, State#state.v)),
    Change = lists:map( fun(C) -> 
				math:abs(math:exp(-1.0*
					 cartesian_distance([X,Y], 
							    State#state.v)))*
				    C
			end, Closer),
    State#state{v = vectoradd(State#state.v, Change)}.

-spec vectoradd(list(), list()) -> list().
vectoradd(L1,L2) ->
    vectoradd(L1,L2,[]).
vectoradd([],[],Acc) ->
    lists:reverse(Acc);
vectoradd([H1|T1],[H2|T2],Acc) ->
    vectoradd(T1,T2,[H1+H2|Acc]).

-spec cartesian_distance(list(), list()) -> float().
cartesian_distance(L1,L2) ->
    L = lists:foldl(fun({X1,X2},Acc) ->
			    [math:pow(X1-X2, 2)|Acc] end,
		    [],
		    lists:zip(L1,L2)),
    math:sqrt(lists:sum(L)).

-spec random_vector(integer()) -> list().
random_vector(Length) ->
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    random_vector(Length, []).
random_vector(0, Acc) ->
    Acc;
random_vector(N, Acc) when N > 0 ->
    random_vector(N-1,[random:uniform()|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_fsm exports %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    ?MODULE:start_link([]).
start_link(Args) ->
    gen_fsm:start_link(?MODULE, [Args], []).
init([PList]) ->
    VectorLength = case proplists:get_value(length, PList) of
		       undefined ->
			   ?DEFAULTLENGTH;
		       Length ->
			   Length
		   end,
    {ok, analyze, #state{v = random_vector(VectorLength)}}.

handle_event(learn, _FSMState, State) ->
    {next_state, learning, State};
handle_event(analyze, _FSMState, State) ->
    {next_state, analyze, State}.
handle_sync_event({setcoords, {X,Y}}, _From, FSMState, State) ->
    {reply, ok, FSMState, State#state{x=X, y=Y}}.

handle_info(Event, FSMState, State) ->
    error_logger:error_msg("ekm_node(~p) recvd unexpected msg: ~p~n",
			   [self(), Event]),
    {next_state, FSMState, State}.

terminate(_Reason, _FSMState, _State) ->
    ok.
code_change(_OldVsn, FSMState, State, _Extras) ->
    {ok, FSMState, State}.
