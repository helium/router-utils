-module(ru_denylist).

-export([
    init/1,
    check/1,
    insert/2
]).

-define(DENY_FILE, "denylist.json").

-spec init(BaseDir :: string()) -> ok.
init(BaseDir) ->
    DenyFile = filename:join([BaseDir, ?DENY_FILE]),
    case read_file(DenyFile) of
        {error, _} ->
            ok;
        {ok, List} ->
            Set = lists:foldl(
                fun(B58Binary, Acc) ->
                    PubKeyBin = libp2p_crypto:b58_to_bin(erlang:binary_to_list(B58Binary)),
                    sets:add_element(PubKeyBin, Acc)
                end,
                sets:new([{version, 2}]),
                List
            ),
            ok = persistent_term:put(?MODULE, Set)
    end.

-spec check(Key :: libp2p_crypto:pubkey_bin()) -> boolean().
check(Key) ->
    try persistent_term:get(?MODULE) of
        Set ->
            sets:is_element(Key, Set)
    catch
        _:_ ->
            %% not enabled/ready
            false
    end.

-spec insert(BaseDir :: string(), Key :: libp2p_crypto:pubkey_bin()) -> ok | {error, any()}.
insert(BaseDir, Key) ->
    DenyFile = filename:join([BaseDir, ?DENY_FILE]),
    case read_file(DenyFile) of
        {error, _} ->
            {error, failed_to_read};
        {ok, List0} ->
            case {lists:member(Key, List0), ?MODULE:check(Key)} of
                {true, true} ->
                    ok;
                {true, false} ->
                    Set0 = persistent_term:get(?MODULE),
                    Set1 = sets:add_element(Key, Set0),
                    ok = persistent_term:put(?MODULE, Set1);
                {false, true} ->
                    List1 = [erlang:list_to_binary(libp2p_crypto:bin_to_b58(Key)) | List0],
                    write_filter_to_disk(DenyFile, jsx:encode(List1));
                {false, false} ->
                    Set0 = persistent_term:get(?MODULE),
                    Set1 = sets:add_element(Key, Set0),
                    ok = persistent_term:put(?MODULE, Set1),
                    List1 = [erlang:list_to_binary(libp2p_crypto:bin_to_b58(Key)) | List0],
                    write_filter_to_disk(DenyFile, jsx:encode(List1))
            end
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec read_file(DenyFile :: string()) -> {ok, list(binary())} | {error, failed}.
read_file(DenyFile) ->
    ok = filelib:ensure_dir(DenyFile),
    try
        case file:read_file(DenyFile) of
            {error, enoent} ->
                ok = file:write_file(DenyFile, jsx:encode([])),
                [];
            {ok, BinFile} ->
                jsx:decode(BinFile, [{return_maps, false}])
        end
    of
        JSON -> {ok, JSON}
    catch
        _:_ ->
            {error, failed}
    end.

-spec write_filter_to_disk(string(), binary()) -> ok | {error, any()}.
write_filter_to_disk(DenyFile, Content) ->
    TmpDenyFile = DenyFile ++ "-tmp",
    try
        ok = file:write_file(TmpDenyFile, Content),
        ok = file:rename(TmpDenyFile, DenyFile)
    of
        ok ->
            ok
    catch
        _:Err ->
            {error, Err}
    end.

%% ------------------------------------------------------------------
%% EUNIT Tests
%% ------------------------------------------------------------------
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

no_file_test() ->
    BaseDir = string:chomp(os:cmd("mktemp -d")),
    #{public := PubKey0} = libp2p_crypto:generate_keys(ecc_compact),
    PubKeyBin = libp2p_crypto:pubkey_to_bin(PubKey0),

    ?assertEqual(ok, ?MODULE:init(BaseDir)),
    ?assertEqual(
        {ok, []},
        read_file(filename:join([BaseDir, ?DENY_FILE]))
    ),
    ?assertEqual(false, ?MODULE:check(PubKeyBin)),
    ?assertEqual(ok, ?MODULE:insert(BaseDir, PubKeyBin)),
    ?assertEqual(true, ?MODULE:check(PubKeyBin)),
    ?assertEqual(
        {ok, [erlang:list_to_binary(libp2p_crypto:bin_to_b58(PubKeyBin))]},
        read_file(filename:join([BaseDir, ?DENY_FILE]))
    ),
    ok.

file_test() ->
    BaseDir = string:chomp(os:cmd("mktemp -d")),
    DenyFile = filename:join([BaseDir, ?DENY_FILE]),
    #{
        public := PubKey0
    } = libp2p_crypto:generate_keys(ecc_compact),
    PubKeyBin = libp2p_crypto:pubkey_to_bin(PubKey0),

    ok = file:write_file(
        DenyFile,
        jsx:encode([erlang:list_to_binary(libp2p_crypto:bin_to_b58(PubKeyBin))])
    ),

    ?assertEqual(ok, ?MODULE:init(BaseDir)),
    ?assertEqual(
        {ok, [erlang:list_to_binary(libp2p_crypto:bin_to_b58(PubKeyBin))]},
        read_file(DenyFile)
    ),
    ?assertEqual(true, ?MODULE:check(PubKeyBin)),
    ok.

-endif.
