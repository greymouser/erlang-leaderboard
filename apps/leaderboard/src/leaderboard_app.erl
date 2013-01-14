-module(leaderboard_app).
-behaviour(application).

-export([start/2, stop/1]).

%%% Application callbacks

start(_StartType, _StartArgs) ->
    lager:debug("Starting leaderboard application"),
    case leaderboard_sup:start_link() of
        {ok, Pid} -> 
            alarm_handler:clear_alarm({application_stopped, leaderboard}),
	       {ok, Pid};
	Error ->
	    alarm_handler:set_alarm({{application_stopped, leaderboard}, []}),
	    Error
    end.

stop(_State) ->
    alarm_handler:set_alarm({{application_stopped, leaderboard}, []}),
    ok.
