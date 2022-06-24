%%%-------------------------------------------------------------------
%%% @doc
%%% == Reputation System ==
%%% @end
%%%-------------------------------------------------------------------
-module(ru_reputation).

%% ------------------------------------------------------------------
%% API Exports
%% ------------------------------------------------------------------
-export([
    init/0,
    denied/1,
    threshold/0, threshold/1,
    track_offer/2,
    track_packet/2,
    track_unknown/1,
    reputations/0,
    reputation/1,
    reset/1,
    crawl_offers/1,
    crawl_reputations/0
]).

-define(ETS, ru_reputation_ets).
-define(OFFER_ETS, ru_reputation_offers_ets).
-define(CRAWL_OFFER_TIMER, timer:minutes(10)).
-define(CRAWL_REP_TIMER, timer:hours(2)).
-define(THRESHOLD, ru_reputation_threshold).
-define(DEFAULT_THRESHOLD, 50).

%% Reputation
%% {missed :: non_neg_integer(), unknown :: non_neg_integer()}
%% missed = Offer purchased but packet not received
%% unknown = Packet received but bad

%% ------------------------------------------------------------------
%% API Functions
%% ------------------------------------------------------------------

-spec init() -> ok.
init() ->
    Opts1 = [
        public,
        named_table,
        set,
        {read_concurrency, true}
    ],
    _ = ets:new(?ETS, Opts1),
    Opts2 = [
        public,
        named_table,
        set,
        {write_concurrency, true}
    ],
    _ = ets:new(?OFFER_ETS, Opts2),
    ok = spawn_crawl_offers(?CRAWL_OFFER_TIMER),
    ok = spawn_crawl_reputations(?CRAWL_REP_TIMER),
    ok.

-spec denied(Hotspot :: libp2p_crypto:pubkey_bin()) -> boolean().
denied(Hotspot) ->
    {Missed, Unknown} = ?MODULE:reputation(Hotspot),
    Missed + Unknown >= ?MODULE:threshold().

-spec threshold() -> non_neg_integer().
threshold() ->
    persistent_term:get(?THRESHOLD, ?DEFAULT_THRESHOLD).

-spec threshold(Int :: non_neg_integer()) -> ok.
threshold(Int) ->
    persistent_term:put(?THRESHOLD, Int).

-spec track_offer(Hotspot :: binary(), PHash :: binary()) -> ok.
track_offer(Hotspot, PHash) ->
    Now = erlang:system_time(millisecond),
    true = ets:insert(?OFFER_ETS, {{Hotspot, PHash}, Now}),
    ok.

-spec track_packet(Hotspot :: binary(), PHash :: binary()) -> ok.
track_packet(Hotspot, PHash) ->
    true = ets:delete(?OFFER_ETS, {Hotspot, PHash}),
    ok.

-spec track_unknown(Hotspot :: binary()) -> non_neg_integer().
track_unknown(Hotspot) ->
    %% Here we update unknown counter (pos 3)
    ets:update_counter(?ETS, Hotspot, {3, 1}, {default, 0, 0}).

-spec reputations() -> list({binary(), non_neg_integer(), non_neg_integer()}).
reputations() ->
    ets:tab2list(?ETS).

-spec reputation(Hotspot :: binary()) -> {non_neg_integer(), non_neg_integer()}.
reputation(Hotspot) ->
    case ets:lookup(?ETS, Hotspot) of
        [] -> {0, 0};
        [{Hotspot, Missed, Unknown}] -> {Missed, Unknown}
    end.

-spec reset(Hotspot :: binary()) -> ok.
reset(Hotspot) ->
    true = ets:delete(?ETS, Hotspot),
    ok.

-spec crawl_offers(Timer :: non_neg_integer()) -> ok.
crawl_offers(Timer) ->
    Now = erlang:system_time(millisecond) - Timer,
    %% MS = ets:fun2ms(fun({Key, Time}) when Time < Now -> Key end),
    MS = [{{'$1', '$2'}, [{'<', '$2', {const, Now}}], ['$1']}],
    Expired = ets:select(?OFFER_ETS, MS),
    lists:foreach(
        fun({Hotspot, PHash}) ->
            true = ets:delete(?OFFER_ETS, {Hotspot, PHash}),
            %% Here we update missed counter (pos 2)
            _Counter = ets:update_counter(?ETS, Hotspot, {2, 1}, {default, 0, 0})
        end,
        Expired
    ),
    ok.

%% This function decrease reputation based on threshold
%% Equal or under threshold: 20% decrease (% based on threshold)
%% Over threshold:
%%     lowest score set to: 0
%%     highest score set to: threshold - 10% (% based on threshold)
-spec crawl_reputations() -> ok.
crawl_reputations() ->
    Threshold = ?MODULE:threshold(),
    DecreaseBy20Per = decrease_by(Threshold, 0.2),
    DecreaseBy10Per = decrease_by(Threshold, 0.1),
    lists:foreach(
        fun({Hotspot, Missed, Unknown}) ->
            case Missed + Unknown of
                0 ->
                    noop;
                Rep when Rep =< Threshold andalso Missed >= Unknown ->
                    _ = ets:update_counter(?ETS, Hotspot, {2, DecreaseBy20Per, 0, 0});
                Rep when Rep =< Threshold andalso Missed < Unknown ->
                    _ = ets:update_counter(?ETS, Hotspot, {3, DecreaseBy20Per, 0, 0});
                Rep when Rep > Threshold andalso Missed >= Unknown ->
                    _ = ets:insert(?ETS, {Hotspot, Threshold + DecreaseBy10Per, 0});
                Rep when Rep > Threshold andalso Missed < Unknown ->
                    _ = ets:insert(?ETS, {Hotspot, 0, Threshold + DecreaseBy10Per})
            end
        end,
        ?MODULE:reputations()
    ).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec decrease_by(Threshold :: non_neg_integer(), Per :: float()) -> neg_integer().
decrease_by(Threshold, Per) when Per > 0 andalso Per < 1 ->
    case Threshold * Per of
        X when X < 1 -> -1;
        X -> erlang:round(X) * -1
    end.

-spec spawn_crawl_offers(Timer :: non_neg_integer()) -> ok.
spawn_crawl_offers(Timer) ->
    _ = erlang:spawn(fun() ->
        ok = timer:sleep(Timer),
        ok = crawl_offers(Timer),
        ok = spawn_crawl_offers(Timer)
    end),
    ok.

-spec spawn_crawl_reputations(Timer :: non_neg_integer()) -> ok.
spawn_crawl_reputations(Timer) ->
    _ = erlang:spawn(fun() ->
        ok = timer:sleep(Timer),
        ok = crawl_reputations(),
        ok = spawn_crawl_offers(Timer)
    end),
    ok.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

bad_test() ->
    ok = ?MODULE:init(),

    Hotspot = crypto:strong_rand_bytes(32),
    PHash = crypto:strong_rand_bytes(32),
    ok = ?MODULE:track_offer(Hotspot, PHash),

    ?assertEqual({0, 0}, ?MODULE:reputation(Hotspot)),
    timer:sleep(110),
    ok = ?MODULE:crawl_offers(100),

    ?assertEqual({1, 0}, ?MODULE:reputation(Hotspot)),
    ?assertEqual([], ets:lookup(?ETS, {Hotspot, PHash})),

    lists:foreach(
        fun(_) ->
            _ = erlang:spawn(?MODULE, track_offer, [
                Hotspot,
                crypto:strong_rand_bytes(32)
            ])
        end,
        lists:seq(1, 99)
    ),

    timer:sleep(110),
    ok = ?MODULE:crawl_offers(100),

    ?assertEqual({100, 0}, ?MODULE:reputation(Hotspot)),
    ?assertEqual(true, ?MODULE:denied(Hotspot)),

    ?assertEqual([{Hotspot, 100, 0}], ?MODULE:reputations()),

    ?assertEqual(1, ?MODULE:track_unknown(Hotspot)),
    ?assertEqual({100, 1}, ?MODULE:reputation(Hotspot)),
    ?assertEqual([{Hotspot, 100, 1}], ?MODULE:reputations()),

    ok = ?MODULE:reset(Hotspot),

    ?assertEqual({0, 0}, ?MODULE:reputation(Hotspot)),
    ?assertEqual(false, ?MODULE:denied(Hotspot)),

    ets:delete(?ETS),
    ets:delete(?OFFER_ETS),

    ok.

good_test() ->
    ok = ?MODULE:init(),

    Hotspot = crypto:strong_rand_bytes(32),
    PHash = crypto:strong_rand_bytes(32),
    ok = ?MODULE:track_offer(Hotspot, PHash),

    ok = ?MODULE:track_packet(Hotspot, PHash),
    timer:sleep(110),

    ok = ?MODULE:crawl_offers(100),

    ?assertEqual({0, 0}, ?MODULE:reputation(Hotspot)),
    ?assertEqual([], ets:lookup(?ETS, {Hotspot, PHash})),
    ?assertEqual([], ?MODULE:reputations()),

    ets:delete(?ETS),
    ets:delete(?OFFER_ETS),

    ok.

crawl_reputations_test() ->
    ok = ?MODULE:init(),

    Threshold = ?MODULE:threshold(),

    Hotspot1 = crypto:strong_rand_bytes(32),
    Hotspot2 = crypto:strong_rand_bytes(32),
    Hotspot3 = crypto:strong_rand_bytes(32),
    Hotspot4 = crypto:strong_rand_bytes(32),
    Hotspot5 = crypto:strong_rand_bytes(32),
    Hotspot6 = crypto:strong_rand_bytes(32),
    Hotspot7 = crypto:strong_rand_bytes(32),
    ?assert(ets:insert(?ETS, {Hotspot1, 0, 0})),
    ?assert(ets:insert(?ETS, {Hotspot2, Threshold, 0})),
    ?assert(ets:insert(?ETS, {Hotspot3, 0, Threshold})),
    ?assert(ets:insert(?ETS, {Hotspot4, Threshold - 1, 0})),
    ?assert(ets:insert(?ETS, {Hotspot5, 0, Threshold - 1})),
    ?assert(ets:insert(?ETS, {Hotspot6, Threshold + 1, 1})),
    ?assert(ets:insert(?ETS, {Hotspot7, 1, Threshold + 1})),

    ?assertEqual(ok, ?MODULE:crawl_reputations()),

    ?assertEqual({0, 0}, ?MODULE:reputation(Hotspot1)),
    ?assertEqual({40, 0}, ?MODULE:reputation(Hotspot2)),
    ?assertEqual({0, 40}, ?MODULE:reputation(Hotspot3)),
    ?assertEqual({39, 0}, ?MODULE:reputation(Hotspot4)),
    ?assertEqual({0, 39}, ?MODULE:reputation(Hotspot5)),
    ?assertEqual({45, 0}, ?MODULE:reputation(Hotspot6)),
    ?assertEqual({0, 45}, ?MODULE:reputation(Hotspot7)),

    ets:delete(?ETS),
    ets:delete(?OFFER_ETS),
    ok.

-endif.
