%% src/tic_tac_toe_app.erl
-module(tic_tac_toe_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/websocket", websocket_handler, []}
        ]}
    ]),

    {ok, Port} = application:get_env(tic_tac_toe, websocket_port),

    {ok, _} = cowboy:start_clear(http_listener, [{port, Port}], #{
        env => #{dispatch => Dispatch}
    }),

    tic_tac_toe_sup:start_link().

stop(_State) ->
    ok.
