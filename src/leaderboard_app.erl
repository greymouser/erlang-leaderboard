-module(leaderboard_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
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

% %% ===================================================================
% % Tests
% %% ===================================================================
% -ifdef(TEST).

% setup() ->
%     ok = application:start(sasl),
%     ok = application:start(mnesia).    

% cleanup() ->
%     application:stop(mnesia),
%     application:stop(sasl).

% start_test() ->
%     setup(),
%     ok = application:start(vdpms),
%     ?assertNot(undefined == whereis(vdpms_sup)),
%     application:stop(vdpms),
%     cleanup().

% %% start_fail_test() ->
% %%     setup(),
% %%     erlymock:start(),
% %%     erlymock:stub(vdpms_sup, start_link, [], [{throw, garbagecan}]), 
% %%     erlymock:replay(), 
% %%     ?assertThrow(garbagecan,application:start([])),
% %%     ok = erlymock:verify(),
% %%     application:stop(vdpms),
% %%     cleanup().

% stop_test() ->
%     setup(),
%     ok = application:start(vdpms),
%     ok = application:stop(vdpms),
%     ?assert(undefined == whereis(vdpms_sup)),
%     cleanup().

% -endif.
