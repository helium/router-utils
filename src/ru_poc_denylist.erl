%%%-------------------------------------------------------------------
%% @doc
%% == Router Utils POC Denylist ==
%%
%% - Loads POC denylist from github.
%% - Loads POC from local disk if available.
%% - Checks if loaded filter contains pubkeybin.
%%
%% Startup Options:
%%   denylist_check_timer:
%%     `{immediate, MsTimeout}' -> checks remote url immediately
%%     `{timer, MsTimeout}'     -> checks remote url after timeout
%%     `manual'                 -> no checks scheduled, only manual calls
%%
%% @end
%%%-------------------------------------------------------------------
-module(ru_poc_denylist).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
    start_link/1,
    check/1,
    get_version/0,
    get_binary/0,
    check_remote/0
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

-define(CHECK_REMOTE, check_remote).

-type version() :: integer().
-type etag() :: binary().
-type url() :: binary().

-record(state, {
    check_timer :: {immediate | timer, non_neg_integer()} | manual,
    filename :: file:filename_all(),
    keys :: list(string()),
    url :: url(),
    version = 0 :: version(),
    etag :: undefined | etag(),
    filter :: undefined | reference()
}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link([Args]) ->
    start_link(Args);
start_link(Args) ->
    case Args of
        #{
            denylist_keys := DenyKeys,
            denylist_url := DenyURL,
            denylist_base_dir := BaseDir,
            denylist_check_timer := CheckTimer
        } ->
            gen_server:start_link(
                {local, ?MODULE},
                ?MODULE,
                [DenyURL, DenyKeys, BaseDir, CheckTimer],
                []
            );
        _ ->
            lager:info("ignored ~p", [Args]),
            ignore
    end.

-spec check(libp2p_crypto:pubkey_bin()) -> boolean().
check(PubkeyBin) ->
    try persistent_term:get(?MODULE) of
        Xor ->
            xorf:contains(Xor, xxhash:hash64(PubkeyBin))
    catch
        _:_ ->
            %% not enabled/ready
            false
    end.

-spec get_version() -> pos_integer().
get_version() ->
    gen_server:call(?MODULE, get_version).

-spec get_binary() -> {ok, binary()} | {error, term()}.
get_binary() ->
    gen_server:call(?MODULE, get_binary).

-spec check_remote() -> ok.
check_remote() ->
    gen_server:cast(?MODULE, ?CHECK_REMOTE).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([URL, Keys, BaseDir, CheckTimer]) ->
    %% load up any existing xor we have on disk
    DenyFile = filename:join([BaseDir, "denylist", "latest"]),
    ok = filelib:ensure_dir(DenyFile),
    %% filter version will be a positive integer or 0 depending on if we could load one from disk

    State0 = #state{url = URL, keys = Keys, check_timer = CheckTimer, filename = DenyFile},

    State1 =
        try
            {ok, Bin} = file:read_file(DenyFile),
            {ok, Contents} = is_valid_filter_bin(Bin, Keys),

            <<Version:32/integer-unsigned-little, FilterBin/binary>> = Contents,
            xorf:from_bin({exor, 32}, FilterBin)
        of
            {ok, Filter} ->
                ok = persistent_term:put(?MODULE, Filter),
                %% No need to write to disk, that's where we got it
                State0#state{version = Version, filter = Filter}
        catch
            _:Err ->
                lager:error("failed to bootstrap denylist from disk: [err: ~p]", [Err]),
                State0#state{version = 0}
        end,

    lager:info(
        "attempted bootstrap from disk [version: ~p] [check-timer: ~p]",
        [State1#state.version, CheckTimer]
    ),
    ok =
        case CheckTimer of
            {immediate, _} -> schedule_check(0);
            {timer, Timeout} -> schedule_check(Timeout);
            manual -> ok
        end,
    {ok, State1}.

handle_call(get_version, _From, State) ->
    {reply, {ok, State#state.version}, State};
handle_call(get_binary, _From, #state{filename = DenyFile} = State) ->
    Bin = file:read_file(DenyFile),
    {reply, Bin, State};
handle_call(Msg, _From, State) ->
    lager:info("unhandled call msg ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:info("unhandled cast msg ~p", [Msg]),
    {noreply, State}.

handle_info(?CHECK_REMOTE, #state{check_timer = CheckTimer, filename = DenyFile} = State) ->
    NextState =
        case fetch_and_verify_latest_denylist(State) of
            {ok, #state{filter = Filter} = NewState, AssetBin} ->
                %% Peristent term gets the filter we want to query.
                ok = persistent_term:put(?MODULE, Filter),
                %% File sys gets the whole Bin the Filter was pulled from.
                ok = write_filter_to_disk(DenyFile, AssetBin),
                NewState;
            {skip, _} = Skip ->
                lager:info("skipping: ~p", [Skip]),
                State;
            {error, _} = Err ->
                lager:notice("something went wrong: ~p", [Err]),
                State
        end,
    ok =
        case CheckTimer of
            {_, Timeout} -> schedule_check(Timeout);
            manual -> ok
        end,
    {noreply, NextState};
handle_info(Msg, State) ->
    lager:info("unhandled info msg ~p", [Msg]),
    {noreply, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec fetch_and_verify_latest_denylist(#state{}) ->
    {ok, #state{}, AssetBin :: binary()}
    | {skip, any()}
    | {error, any()}.
fetch_and_verify_latest_denylist(#state{url = URL, keys = Keys, version = ExistingVersion} = State) ->
    ReqHeaders = request_headers(State),

    try
        %% Start with the baseline
        {ok, NewVersion, ETag, AssetURL} = fetch_version_etag_asset(
            URL,
            ReqHeaders,
            ExistingVersion
        ),
        lager:info("fetch [version: ~p] [etag: ~p]", [NewVersion, ETag]),

        %% Pull out the denylist target.
        {ok, AssetBin} = fetch_denylist(AssetURL, ReqHeaders),

        %% Verify binary and get the contents.
        {ok, Contents} = is_valid_filter_bin(AssetBin, Keys),

        %% More validation and getting the FilterBin.
        <<NewVersion:32/integer-unsigned-little, FilterBin/binary>> = Contents,

        %% Turn into an exor filter.
        xorf:from_bin({exor, 32}, FilterBin)
    of
        {ok, Filter} ->
            {ok, State#state{version = NewVersion, etag = ETag, filter = Filter}, AssetBin}
    catch
        error:{badmatch, {skip, _} = Skip} ->
            %% This is fine
            Skip;
        _:Err:Stack ->
            lager:error("Err: ~p~n", [Stack]),
            {error, Err}
    end.

-spec fetch_version_etag_asset(url(), proplists:proplist(), version()) ->
    {ok, version(), etag(), url()}
    | {skip, existing_version | regressed_version}
    | {error, any()}.
fetch_version_etag_asset(URL, ReqHeaders, ExistingVersion) ->
    case hackney:get(URL, ReqHeaders, <<>>, [with_body]) of
        {ok, 200, ResHeaders, Body} ->
            Etag = proplists:get_value(<<"ETag">>, ResHeaders),
            Decoded = jsx:decode(Body),
            case maybe_binary_to_integer(maps:get(<<"tag_name">>, Decoded, undefined)) of
                undefined ->
                    {error, no_tag_name};
                ExistingVersion ->
                    {skip, existing_version};
                NewVersion when NewVersion < ExistingVersion ->
                    {skip, regressed_version};
                NewVersion ->
                    case maps:get(<<"assets">>, Decoded, undefined) of
                        undefined ->
                            {error, missing_asset};
                        Assets ->
                            case get_asset_url(Assets) of
                                {error, _} = Err ->
                                    Err;
                                {ok, AssetURL} ->
                                    {ok, NewVersion, Etag, AssetURL}
                            end
                    end
            end;
        {ok, 304, _, _} ->
            {error, etag_unchanged};
        _Other ->
            {error, unexpected_response}
    end.

-spec fetch_denylist(url(), proplists:proplist()) -> {ok, binary()} | {error, any()}.
fetch_denylist(AssetURL, ReqHeaders) ->
    case hackney:get(AssetURL, ReqHeaders, <<>>, [with_body, {follow_redirect, true}]) of
        {ok, 200, _Headers, AssetBin} ->
            {ok, AssetBin};
        {ok, Code, _, _} ->
            {error, {unexpected_response_code, Code}};
        Other ->
            {error, {bad_response, Other}}
    end.

-spec write_filter_to_disk(string(), binary()) -> ok | {error, any()}.
write_filter_to_disk(DenyFile, Content) ->
    TmpDenyFile = DenyFile ++ "-tmp",

    try
        ok = file:write_file(TmpDenyFile, Content),
        lager:info("wrote to ~p, renaming to ~p", [TmpDenyFile, DenyFile]),
        ok = file:rename(TmpDenyFile, DenyFile)
    of
        ok ->
            ok
    catch
        _:Err ->
            {error, Err}
    end.

-spec is_valid_filter_bin(binary(), list(string())) -> {ok, binary()}.
is_valid_filter_bin(Bin, Keys) ->
    <<Version:8/integer, SignatureLen:16/integer-unsigned-little, Signature:SignatureLen/binary,
        Contents/binary>> = Bin,
    true = Version == 1,
    true = lists:any(
        fun(Key0) ->
            Key = libp2p_crypto:b58_to_pubkey(Key0),
            catch libp2p_crypto:verify(Contents, Signature, Key)
        end,
        Keys
    ),
    {ok, Contents}.

-spec schedule_check(integer()) -> ok.
schedule_check(Time) ->
    _ = erlang:send_after(Time, self(), ?CHECK_REMOTE),
    ok.

%% ------------------------------------------------------------------
%% Helpers
%% ------------------------------------------------------------------

-spec request_headers(#state{}) -> proplists:proplist().
request_headers(#state{etag = undefined}) ->
    [{"user-agent", "https://github.com/helium/router-utils"}];
request_headers(#state{etag = Etag}) ->
    [
        {<<"user-agent">>, <<"https://github.com/helium/router-utils">>},
        {<<"if-none-match">>, Etag}
    ].

-spec get_asset_url(list(map())) -> {ok, url()} | {error, any()}.
get_asset_url([]) ->
    {error, no_asset_url};
get_asset_url([#{<<"name">> := <<"filter.bin">>, <<"browser_download_url">> := URL} | _]) ->
    {ok, URL};
get_asset_url([_ | Rest]) ->
    get_asset_url(Rest).

-spec maybe_binary_to_integer(binary() | undefined) -> integer() | undefined.
maybe_binary_to_integer(Bin) ->
    case Bin of
        undefined -> undefined;
        _ -> erlang:binary_to_integer(Bin)
    end.
