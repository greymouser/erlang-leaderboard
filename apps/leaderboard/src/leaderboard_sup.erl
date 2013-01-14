
-module(leaderboard_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

% Let the server run until it is purposefully killed
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, brutal_kill, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(leaderboard_server, worker)]} }.
