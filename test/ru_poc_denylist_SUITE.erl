-module(ru_poc_denylist_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    init_per_testcase/2
    %% end_per_testcase/2
]).

-export([
    start_from_scratch/1
]).

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
all() ->
    [
        start_from_scratch
    ].

init_per_testcase(TestCase, Config) ->
    application:ensure_all_started(lager),
    application:ensure_all_started(hackney),

    BaseDir = erlang:atom_to_list(TestCase),
    ok = application:set_env(blockchain, base_dir, BaseDir ++ "/blockchain_data"),
    Config.

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

start_from_scratch(_Config) ->
    Denied = libp2p_crypto:b58_to_bin("1112BVrz6rsgtvmAXS9dBPh9JoASYfmtu5u3sYQjvK19AmgjGkq"),
    Good = libp2p_crypto:b58_to_bin("112XYe7Ej1cw1CfBXer3kn7fLwEFuaJ2maWxTCHoECpx5v1X4M1F"),
    ?assertEqual(false, ru_poc_denylist:check(Denied)),
    ?assertEqual(false, ru_poc_denylist:check(Good)),

    TmpDir = string:chomp(os:cmd("mktemp -d")),
    StartArgs = #{
        denylist_keys => ["1SbEYKju337P6aYsRd9DT2k4qgK5ZK62kXbSvnJgqeaxK3hqQrYURZjL"],
        denylist_url => <<"https://api.github.com/repos/helium/denylist/releases/latest">>,
        denylist_base_dir => TmpDir,
        denylist_check_timer => {immediate, timer:hours(6)}
    },
    {ok, Pid0} = ru_poc_denylist:start_link(StartArgs),

    %% nothing to pull in disk
    ?assertEqual({ok, 0}, ru_poc_denylist:get_version()),

    ok = wait_until(fun() ->
        ru_poc_denylist:get_version() /= {ok, 0}
    end),
    {ok, LatestVersion} = ru_poc_denylist:get_binary(),
    ?assertEqual(true, ru_poc_denylist:check(Denied)),
    ?assertEqual(false, ru_poc_denylist:check(Good)),

    ok = gen_server:stop(Pid0),

    %% ===================================================================
    %% Filters must be signed, and there's many steps to make that happen. We're
    %% going to piggyback off the success of this test and make sure we can
    %% startup and load from disk.

    {ok, _Pid1} = ru_poc_denylist:start_link(StartArgs),
    ?assertEqual({ok, LatestVersion}, ru_poc_denylist:get_binary()),
    ?assertEqual(true, ru_poc_denylist:check(Denied)),
    ?assertEqual(false, ru_poc_denylist:check(Good)),

    ok.

%% ===================================================================
%% Helpers
%% ===================================================================

wait_until(Fun) ->
    wait_until(Fun, 100, 100).

wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Res = Fun(),
    case Res of
        true ->
            ok;
        {fail, _Reason} = Fail ->
            Fail;
        _ when Retry == 1 ->
            {fail, Res};
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry - 1, Delay)
    end.
