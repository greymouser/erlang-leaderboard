-module(leaderboard_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, create/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% records
%% ------------------------------------------------------------------

%-record(state, {socket,
%                port,
%                acceptor}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    process_flag(trap_exit, true).

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

%handle_cast({create, _Pid}, #state{socket = Socket} = State) ->
%    New_pid = vdpms_server_socket:start_link(self(), Socket, State#state.port),
%    {noreply, State#state{acceptor=New_pid}};

handle_cast(_Msg, State) ->
  {noreply, State}.

%% %% The current acceptor has died, log it out of interest, and move along
%handle_info({'EXIT', Pid, Abnormal}, #state{acceptor=Pid} = State) ->
%    error_logger:warning_msg("server_socket handler PID(~p) exited with (~p)", [Pid, Abnormal]),
%    {noreply, State};

%% Specialize normal exit
%handle_info({'EXIT', Pid, normal}, #state{acceptor=Pid} = State) ->
%    {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

