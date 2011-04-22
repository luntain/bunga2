%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc bunga2.

-module(bunga2).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the bunga2 server.
start() ->
    bunga2_deps:ensure(),
    ensure_started(crypto),
    application:start(bunga2).


%% @spec stop() -> ok
%% @doc Stop the bunga2 server.
stop() ->
    application:stop(bunga2).
