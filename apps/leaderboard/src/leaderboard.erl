-module(leaderboard).
%-export([setup/1,
%	 find/2, delete/2]).

-include("leaderboard.hrl").

-compile(export_all).
%-export([start/0]).

%%% shortcut to start the app

start() ->
    application:start(leaderboard).

%%% API

delete(Leaderboard) ->
    gen_server:cast(leaderboard_server, {delete, Leaderboard}).

members_total(Leaderboard) ->
    gen_server:call(leaderboard_server, {members_total, Leaderboard}).

leaderboard_pages(Leaderboard) ->
    gen_server:call(leaderboard_server, {leaderboard_pages, Leaderboard}).

rank_for(Leaderboard, Member) ->
    gen_server:call(leaderboard_server, {rank_for, Leaderboard, Member}).

score_for(Leaderboard, Member) ->
    gen_server:call(leaderboard_server, {score_for, Leaderboard, Member}).

member_check(Leaderboard, Member) ->
    Ret = gen_server:call(leaderboard_server, {member_check, Leaderboard, Member}),
    lager:debug("member_check(~p)", [Ret]),
    Ret.

score_and_rank(Leaderboard, Member) ->
    Ret = gen_server:call(leaderboard_server, {score_and_rank, Leaderboard, Member}),
    error_logger:info_msg("score_and_rank", Ret),
    Ret.

members_in_score_range(Leaderboard, Min, Max) ->
    Ret = gen_server:call(leaderboard_server, {members_in_score_range, Leaderboard, Min, Max}),
    error_logger:info_msg("members_in_score_range", Ret),
    Ret.

member_rank(Leaderboard, Member, Score) ->
    gen_server:cast(leaderboard_server, {member_rank, Leaderboard, Member, Score}).

member_remove(Leaderboard, Member) ->
    gen_server:cast(leaderboard_server, {member_remove, Leaderboard, Member}).

members_remove_in_range(Leaderboard, Min, Max) ->
    gen_server:cast(leaderboard_server, {members_remove_in_range, Leaderboard, Min, Max}).

member_change_score(Leaderboard, Member, Delta) ->
    gen_server:cast(leaderboard_server, {member_change_score, Leaderboard, Member, Delta}).
