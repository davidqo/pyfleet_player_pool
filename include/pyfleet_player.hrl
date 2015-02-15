%%%-------------------------------------------------------------------
%%% @author Davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Февр. 2015 17:04
%%%-------------------------------------------------------------------

-record(pyfleet_player_api_request, {
  api_ref,
  sequrity_data,
  body
}).

-record(pyfleet_player_api_response, {
  api_resp,
  sequrity_data,
  body
}).