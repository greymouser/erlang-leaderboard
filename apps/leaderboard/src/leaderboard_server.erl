-module(leaderboard_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%% exports
-export([start_link/0, create/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%% records
-record(state, {client,
		host,
		port,
		with_scores,
		with_rank,
		zero_index_for_rank,
		page_size}).

%%% definitions
start_link() ->
    Args = application:get_all_env(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

create(ServerPid, Pid) ->
    gen_server:cast(ServerPid, {create, Pid}).

%%% gen_server defintions
init(Args) ->
    {host, Host} = lists:keyfind(host, 1, Args),
    {port, Port} = lists:keyfind(port, 1, Args),
    {with_scores, WithScores} = lists:keyfind(with_scores, 1, Args),
    {with_rank, WithRank} = lists:keyfind(with_rank, 1, Args),
    {zero_index_for_rank, ZeroIndexForRank} = lists:keyfind(zero_index_for_rank, 1, Args),
    {page_size, PageSize} = lists:keyfind(page_size, 1, Args),
    process_flag(trap_exit, true),
    case erldis:connect(Host, Port) of
	{ok, Client} ->
	    lager:info("Connected to redis client: ~p~n", [Client]),
	    {ok, #state{client = Client,
			host = Host,
			port = Port,
			with_scores = WithScores,
			with_rank = WithRank,
			zero_index_for_rank = ZeroIndexForRank,
			page_size = PageSize}};
	X ->
	    lager:error("Could not connect to redis: ~p~n", [X]),
	    {stop, X}
    end.

%%% Calls

handle_call({members_total, Leaderboard}, _From, #state{client = Client} = State) ->
    {reply, erldis:zcard(Client, Leaderboard), State};

handle_call({pages, Leaderboard}, _From, #state{client = Client, page_size = PageSize} = State) ->
    {reply, ceiling(members_total(Client, Leaderboard)/PageSize), State};

handle_call({rank_for, Leaderboard, Member}, _From, #state{client = Client} = State) ->
    {reply, ranking_format(State#state.zero_index_for_rank, erldis:zrevrank(Client, Leaderboard, Member)), State};

handle_call({score_for, Leaderboard, Member}, _From, #state{client = Client} = State) ->
    {reply, erldis:zscore(Client, Leaderboard, Member), State};

handle_call({member_check, Leaderboard, Member}, _From, #state{client = Client} = State) ->
    {reply, erldis:zscore(Client, Leaderboard, Member), State}; %FIXME: check for empty or not

handle_call({score_and_rank, Leaderboard, Member}, _From, #state{client = Client} = State) ->
    {reply,
     [{score, erldis:zscore(Client, Leaderboard, Member)},
      {rank, ranking_format(State#state.zero_index_for_rank, erldis:zrevrank(Client, Leaderboard, Member))}],
     State};

handle_call({members_in_score_range, Leaderboard, Min, Max}, _From, #state{client = Client} = State) ->
    {reply, erldis:zcount(Client, Leaderboard, Min, Max), State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

%%% Casts

handle_cast({delete, Leaderboard}, #state{client = Client} = State) ->
    {noreply, erldis:del(Client, Leaderboard), State};

handle_cast({member_rank, Leaderboard, Member, Score, MemberData}, #state{client = Client} = State) ->
    {noreply,
        erldis:zadd(Client, Leaderboard, Score, Member),
        case MemberData of
            nil -> ok;
            _Else -> erldis:hset(Client, Leaderboard ++ ":member_data", Member, MemberData)
        end,
        State};

handle_cast({member_remove, Leaderboard, Member}, #state{client = Client} = State) ->
    {noreply, erldis:zrem(Client, Leaderboard, Member), State};

handle_cast({members_remove_in_range, Leaderboard, Min, Max}, #state{client = Client} = State) ->
    {noreply, erldis:zremrangebyscore(Client, Leaderboard, Min, Max), State};

handle_cast({member_change_score, Leaderboard, Member, Delta}, #state{client = Client} = State) ->
    {noreply, erldis:zincrby(Client, Leaderboard, Delta, Member), State};

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

ranking_format(ZeroRank, Rank) ->
    case ZeroRank of
	   true -> Rank;
	   _ -> Rank + 1
    end.

members_total(Client, Leaderboard) ->
    erldis:zcard(Client, Leaderboard).

%% floor(X) ->
%%     T = erlang:trunc(X),
%%     case (X - T) of
%%         Neg when Neg < 0 ->
%% 	    T - 1;
%% 	Pos when Pos > 0 -> T;
%%         _ -> T
%%     end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
