% ekm_app.erl
% application module for erlang kohonen map
-module(ekm_app).
-behavior(application).

% application exports
-export([start/2, stop/1]).

start(_Type, _Args) ->
    case ekm_sup:start_link() of
	{ok, _Pid} = NormalStart ->
	    NormalStart;
	_ ->
	    {error, nostart}
    end.

stop(_State) ->
    ok.
