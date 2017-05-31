-module(rebar3_vir).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State2} = vir_prv:init(State),
  {ok, State2}.


