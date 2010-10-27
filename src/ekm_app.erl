% ekm_app.erl
% application module for erlang kohonen map
-module(ekm_app).
-behavior(application).

% application exports
-export([start/2, stop/1]).

start(_Type, _Args) ->
    {ok, Pid} = ekm_sup:start_link(),
    % Get the X and Y dimensions
    {ok, [[X,Y]]} = init:get_argument(dims),
    ekm:buildmap(list_to_integer(X),list_to_integer(Y)),
    {ok, Pid}.

stop(_State) ->
    ok.
