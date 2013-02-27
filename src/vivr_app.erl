%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the vivr application.

-module(vivr_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for vivr.
start(_Type, _StartArgs) ->
    vivr_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for vivr.
stop(_State) ->
    ok.
