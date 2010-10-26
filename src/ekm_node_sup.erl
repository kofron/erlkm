% ekm_node_sup.erl
% a supervisor for the nodes in a kohonen map.
% each node is an ekm_node as defined in ekm_node.erl
% the idea is to keep the supervisor as a simple_one_for_one
% supervisor, as it only manages nodes and we dynamically create
% them at runtime.
-module(ekm_node_sup).
-behavior(supervisor).

% supervisor name
-define(SUPERVISOR, ?MODULE).

% supervisor exports
-export([start_link/0, init/1]).

start_link() ->
    CStrat = {simple_one_for_one, 0, 1},
    CSpec = {ekm_node, 
	     {ekm_node, start_link, []}, 
	     permanent, 
	     brutal_kill, 
	     worker, 
	     [ekm_node]},
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, {CStrat, [CSpec]}).

init(Spec) ->
    {ok, Spec}.
