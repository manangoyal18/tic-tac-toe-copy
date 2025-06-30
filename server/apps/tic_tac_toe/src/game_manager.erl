%% src/game_manager.erl
-module(game_manager).
-behaviour(gen_server).

-export([start_link/0, join_game/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("tic_tac_toe.hrl").


-record(gm_state, {
    waiting_player = undefined  %% Holds one player waiting to be matched
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Called by websocket_handler
join_game(PlayerPid) ->
    gen_server:cast(?MODULE, {join_game, PlayerPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_cast({join_game, Player1}, State = #gm_state{waiting_player = undefined}) ->
    ?LOG_INFO("Player ~p is waiting for an opponent", [Player1]),
    {noreply, State#gm_state{waiting_player = Player1}};

handle_cast({join_game, Player2}, State = #gm_state{waiting_player = Player1}) ->
    ?LOG_INFO("Matching players ~p and ~p", [Player1, Player2]),
    %% Start a new game_session
    {ok, GamePid} = game_session:start_link(Player1, Player2),

    %% Assign symbols
    Player1 ! {assign_symbol, <<"x">>, GamePid},
    Player2 ! {assign_symbol, <<"o">>, GamePid},

    {noreply, State#gm_state{waiting_player = undefined}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
