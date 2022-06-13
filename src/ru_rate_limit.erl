-module(ru_rate_limit).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1,
    limit/4
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-define(REFRESH, refresh).

-record(state, {
    table :: ets:table(),
    refresh :: non_neg_integer()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec limit(
    Table :: ets:table(),
    Key :: any(),
    Value :: non_neg_integer(),
    Default :: non_neg_integer()
) -> {ok, non_neg_integer()} | {error, limit_reached}.
limit(Table, Key, Value, Default) ->
    case
        ets:update_counter(
            Table,
            Key,
            {2, Value * -1, 0, 0},
            {default, Default}
        )
    of
        0 -> {error, limit_reached};
        Counter -> {ok, Counter}
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(#{name := Name, refresh := Refresh} = _Args) ->
    Table = ets:new(Name, [
        public,
        named_table,
        set,
        {write_concurrency, true}
    ]),
    lager:info("init rate limit for ~p", [Name]),
    _ = erlang:send_after(Refresh, self(), ?REFRESH),
    {ok, #state{table = Table, refresh = Refresh}}.

handle_call(Msg, _From, State) ->
    lager:info("unhandled call msg ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:info("unhandled cast msg ~p", [Msg]),
    {noreply, State}.

handle_info(?REFRESH, #state{table = Table, refresh = Refresh} = State) ->
    true = ets:delete_all_objects(Table),
    _ = erlang:send_after(Refresh, self(), ?REFRESH),
    {noreply, State};
handle_info(Msg, State) ->
    lager:info("unhandled info msg ~p", [Msg]),
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

limit_test() ->
    Name = limit_test,
    {ok, Pid} = ?MODULE:start_link(#{name => Name, refresh => timer:seconds(1)}),

    Default = 10,
    Max = 10000,

    Keys = lists:foldl(
        fun(_, Acc) ->
            Key = crypto:strong_rand_bytes(32),
            Value = rand:uniform(10),
            _ = erlang:spawn(?MODULE, limit, [Name, Key, Value, Default]),
            [{Key, Value} | Acc]
        end,
        [],
        lists:seq(1, Max)
    ),

    {Key, Value} = lists:nth(rand:uniform(Max), Keys),
    ?assertEqual([{Key, Default - Value}], ets:lookup(Name, Key)),
    ?assertEqual({ok, Default - Value - 1}, ?MODULE:limit(Name, Key, 1, Default)),

    timer:sleep(timer:seconds(1)),
    ?assertEqual([], ets:lookup(Name, Key)),
    ?assertEqual([], ets:tab2list(Name)),

    gen_server:stop(Pid),
    ok.

-endif.
