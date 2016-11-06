%% @private
-module(google_roads_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{google_roads_client, {google_roads_client, start_link, []},
		temporary, 5000, worker, [google_roads_client]}],
	{ok, {{simple_one_for_one, 10, 10}, Procs}}.
