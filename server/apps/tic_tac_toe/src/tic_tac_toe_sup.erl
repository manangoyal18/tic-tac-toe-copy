-module(tic_tac_toe_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        {game_manager,
         {game_manager, start_link, []},
         permanent, 5000, worker, [game_manager]}
    ],
    {ok, {{one_for_one, 10, 10}, Children}}.
