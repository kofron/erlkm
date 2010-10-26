% ekm_sup.erl
% top level supervisor for kohonen map.
-module(ekm_sup).
-behavior(supervisor).

% supervisor name
-define(SUPERVISOR, ?MODULE).

% supervisor exports
-export([start_link/0, init/1]).

start_link() ->
    CStrat = {one_for_one, 0, 1},
    NodeSup = {ekm_node_sup,
	       {ekm_node_sup, start_link, []},
	       permanent,
	       10000,
	       supervisor,
	       [ekm_node_sup]},
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, {CStrat, [NodeSup]}).

init(Spec) ->
    {ok, Spec}.
