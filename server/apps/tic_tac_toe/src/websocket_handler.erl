-module(websocket_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include("tic_tac_toe.hrl").


-record(ws_state, {
    symbol,
    game_pid,
    player_pid
}).

%% Called when client connects
init(_Req, _State) ->
    {cowboy_websocket, _Req, #{}}.

%% After websocket is established
websocket_init(_StateMap) ->
    Self = self(),
    game_manager:join_game(Self),
    {ok, #ws_state{}}.

%% Handle incoming message from client
websocket_handle({text, Msg}, State = #ws_state{game_pid = undefined}) ->
    %% Ignore any input before symbol assignment
    {ok, State};

websocket_handle({text, Msg}, State = #ws_state{game_pid = GamePid, symbol = Symbol}) ->
    case catch jsone:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"move">>, <<"row">> := Row, <<"col">> := Col} ->
            GamePid ! {player_move, self(), Symbol, Row, Col},
            {ok, State};
        _ ->
            {reply, {text, <<"Invalid input">>}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

%% Handle internal messages
websocket_info({assign_symbol, Symbol, GamePid}, State) ->
    Msg = jsone:encode(#{type => <<"assign_symbol">>, symbol => Symbol}),
    {reply, {text, Msg}, State#ws_state{symbol = Symbol, game_pid = GamePid}};

websocket_info({game_state, Board, Turn}, State) ->
    Msg = jsone:encode(#{type => <<"game_state">>, board => Board, turn => Turn}),
    {reply, {text, Msg}, State};

websocket_info({game_result, Result}, State) ->
    Msg = jsone:encode(#{type => <<"game_result">>, result => Result}),
    {reply, {text, Msg}, State};

websocket_info({send, Msg}, State) when is_binary(Msg) ->
    {reply, {text, Msg}, State};

websocket_info(_Other, State) ->
    {ok, State}.

%% On disconnect
terminate(_Reason, _Req, State = #ws_state{game_pid = undefined}) ->
    ok;

terminate(_Reason, _Req, State = #ws_state{game_pid = GamePid}) ->
    GamePid ! {player_disconnected, self()},
    ok.
