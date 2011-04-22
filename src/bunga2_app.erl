%% @author Mochi Media <dev@mochimedia.com>
%% @copyright bunga2 Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the bunga2 application.

-module(bunga2_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for bunga2.
start(_Type, _StartArgs) ->
    bunga2_deps:ensure(),
    bunga2_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for bunga2.
stop(_State) ->
    ok.
