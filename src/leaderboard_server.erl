-module(leaderboard_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%% exports
-export([start_link/0, create/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% records
-record(state, {host,
		port,
		client}).

%%% definitions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%% gen_server defintions
init(_Args) ->
    %[RedisHost, RedisPort] = Args,
    RedisHost = "127.0.0.1",
    RedisPort = 6379,
    process_flag(trap_exit, true),
    case erldis:connect(RedisHost, RedisPort) of
	{ok, Client} ->
	    error_logger:info_msg("Connected to redis client<~p>~n", [Client]),
	    {ok, #state{host = RedisHost, port = RedisPort, client = Client}};
	X ->
	    error_logger:error_msg("Could not connect to redis: ~p~n", [X]),
	    {stop, X}
    end.

handle_call({members_total, Leaderboard}, _From, #state{client = Client} = State) ->
    {reply, members_total(Client, Leaderboard), State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({leaderboard_delete, Leaderboard}, #state{client = Client} = State) ->
    {noreply, erldis:del(Client, Leaderboard), State};

handle_cast({member_rank, Member, Score}, #state{client = Client} = State) ->
    {noreply, erldis:zadd(Client, Score, Member), State};

handle_cast({member_remove, Member}, #state{client = Client} = State) ->
    {noreply, erldis:zrem(Client, Member), State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

members_total(Client, Leaderboard) ->
    erldis:zcard(Client, Leaderboard).
