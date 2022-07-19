%%%-------------------------------------------------------------------
%%% @author jonathanruttenberg
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2022 5:37 PM
%%%-------------------------------------------------------------------
-module(gwmp_metrics).
-author("jonathanruttenberg").

-define(METRICS_GWMP_COUNT, "gwmp_counter").

%% erlfmt-ignore
-define(VALID_NET_IDS, sets:from_list(
  lists:seq(16#000000, 16#0000FF) ++
    lists:seq(16#600000, 16#6000FF) ++
    lists:seq(16#C00000, 16#C000FF) ++
    lists:seq(16#E00000, 16#E000FF)
)).

%% API
-export([push_ack/2, clean_net_id/1]).

-spec push_ack(Prefix :: string(), NetID :: non_neg_integer()) -> ok.
push_ack(Prefix, NetID) ->
  Name = build_name(Prefix, ?METRICS_GWMP_COUNT),
  prometheus_counter:inc(Name, [clean_net_id(NetID), push_ack, hit]).

build_name(Prefix, Body) ->
  list_to_atom(Prefix ++ Body).

-spec clean_net_id(non_neg_integer()) -> unofficial_net_id | non_neg_integer().
clean_net_id(NetID) ->
  case sets:is_element(NetID, ?VALID_NET_IDS) of
    true -> NetID;
    false -> unofficial_net_id
  end.