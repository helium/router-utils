%%%-------------------------------------------------------------------
%%% @author jonathanruttenberg
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2022 12:58 PM
%%%-------------------------------------------------------------------
-module(udp_worker_utils).
-author("jonathanruttenberg").

-include("semtech_udp.hrl").

%% API
-export([handle_push_data/3, pubkeybin_to_mac/1, send_tx_ack/2, handle_pull_response/3, handle_pull_ack/6, handle_pull_data_timeout/3, send_pull_data/1]).

handle_push_data(PushDataMap, Location, PacketTime) ->
  #{pub_key_bin := PubKeyBin,
    region := Region,
    tmst := Tmst,
    payload := Payload,
    frequency := Frequency,
    datarate := Datarate,
    signal_strength := SignalStrength,
    snr := Snr} = PushDataMap,

  MAC = pubkeybin_to_mac(PubKeyBin),
  Token = semtech_udp:token(),
  {Index, Lat, Long} =
    case Location of
      undefined -> {undefined, undefined, undefined};
      no_location -> {undefined, undefined, undefined};
      {_, _, _} = L -> L
    end,

  Data = semtech_udp:push_data(
    Token,
    MAC,
    #{
      time => iso8601:format(
        calendar:system_time_to_universal_time(PacketTime, millisecond)
      ),
      tmst => Tmst band 16#FFFFFFFF,
      freq => Frequency,
      rfch => 0,
      modu => <<"LORA">>,
      codr => <<"4/5">>,
      stat => 1,
      chan => 0,
      datr => erlang:list_to_binary(Datarate),
      rssi => erlang:trunc(SignalStrength),
      lsnr => Snr,
      size => erlang:byte_size(Payload),
      data => base64:encode(Payload)
    },
    #{
      regi => Region,
      inde => Index,
      lati => Lat,
      long => Long,
      pubk => libp2p_crypto:bin_to_b58(PubKeyBin)
    }
  ),
  {Token, Data}.

-spec pubkeybin_to_mac(binary()) -> binary().
pubkeybin_to_mac(PubKeyBin) ->
  <<(xxhash:hash64(PubKeyBin)):64/unsigned-integer>>.

-spec send_tx_ack(
    binary(),
    #{pubkeybin := libp2p_crypto:pubkey_bin(),
    socket := pp_udp_socket:socket()}) -> ok | {error, any()}.
send_tx_ack(
    Token,
    #{pubkeybin := PubKeyBin, socket := Socket}
) ->
  Data = semtech_udp:tx_ack(Token, udp_worker_utils:pubkeybin_to_mac(PubKeyBin)),
  Reply = pp_udp_socket:send(Socket, Data),
  lager:debug("sent ~p/~p to ~p replied: ~p", [
    Token,
    Data,
    pp_udp_socket:get_address(Socket),
    Reply
  ]),
  Reply.

handle_pull_response(Data, PubKeyBin, Socket) ->
  Token = semtech_udp:token(Data),
  send_tx_ack(Token, #{pubkeybin => PubKeyBin, socket => Socket}).

handle_pull_ack(Data, PullDataToken, PullDataRef, PullDataTimer, NetID, MetricsPrefix) ->
  case semtech_udp:token(Data) of
    PullDataToken ->
      erlang:cancel_timer(PullDataRef),
      lager:debug("got pull ack for ~p", [PullDataToken]),
      _ = schedule_pull_data(PullDataTimer),
      ok = gwmp_metrics:pull_ack(MetricsPrefix, NetID),
      undefined;
    _UnknownToken ->
      lager:warning("got unknown pull ack for ~p", [_UnknownToken]),
      ignore
  end.

-spec schedule_pull_data(non_neg_integer()) -> reference().
schedule_pull_data(PullDataTimer) ->
  _ = erlang:send_after(PullDataTimer, self(), ?PULL_DATA_TICK).

handle_pull_data_timeout(PullDataTimer, NetID, MetricsPrefix) ->
  lager:debug("got a pull data timeout, ignoring missed pull_ack [retry: ~p]", [PullDataTimer]),
  ok = gwmp_metrics:pull_ack_missed(MetricsPrefix, NetID),
  _ = udp_worker_utils:schedule_pull_data(PullDataTimer).

-spec send_pull_data(#{pubkeybin := libp2p_crypto:pubkey_bin(), socket := pp_udp_socket:socket(),
pull_data_timer := non_neg_integer()}) -> {ok, {reference(), binary()}} | {error, any()}.
send_pull_data(
    #{
      pubkeybin := PubKeyBin,
      socket := Socket,
      pull_data_timer := PullDataTimer
    }
) ->
  Token = semtech_udp:token(),
  Data = semtech_udp:pull_data(Token, pubkeybin_to_mac(PubKeyBin)),
  case pp_udp_socket:send(Socket, Data) of
    ok ->
      lager:debug("sent pull data keepalive ~p", [Token]),
      TimerRef = erlang:send_after(PullDataTimer, self(), ?PULL_DATA_TIMEOUT_TICK),
      {ok, {TimerRef, Token}};
    Error ->
      lager:warning("failed to send pull data keepalive ~p: ~p", [Token, Error]),
      Error
  end.

