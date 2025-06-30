-module(game_session).
-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("tic_tac_toe.hrl").

start_link(Player1, Player2) ->
    gen_server:start_link(?MODULE, {Player1, Player2}, []).

init({Player1, Player2}) ->
    process_flag(trap_exit, true),
    monitor(process, Player1),
    monitor(process, Player2),
    {ok, #state{player_x = Player1, player_o = Player2}}.

handle_info({player_move, From, Symbol, Row, Col}, State) ->
    case is_valid_move(From, Symbol, Row, Col, State) of
        true ->
            NewBoard = update_board(State#state.board, Row, Col, Symbol),
            Winner = check_winner(NewBoard),
            NewTurn = next_turn(Symbol),
            NewState = State#state{board = NewBoard, turn = NewTurn},

            %% Notify both players of updated board
            send_to_players({game_state, NewBoard, NewTurn}, NewState),

            %% Handle game end
            case Winner of
                <<"">> -> {noreply, NewState};
                _ ->
                    send_to_players({game_result, <<Symbol/binary, " wins!">>}, NewState),
                    {stop, normal, NewState}
            end;

        false ->
            ?LOG_WARNING("Invalid move by ~p: ~p ~p ~p", [From, Symbol, Row, Col]),
            {noreply, State}
    end;

handle_info({player_disconnected, Pid}, State) ->
    Other = get_other_player(Pid, State),
    OtherMsg = jsone:encode(#{type => <<"game_result">>, result => <<"Opponent disconnected">>}),
    Other ! {send, OtherMsg},
    {stop, normal, State};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Same as above, but trap exits
    handle_info({player_disconnected, Pid}, State);

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

is_valid_move(From, Symbol, Row, Col, #state{board = Board, player_x = X, player_o = O, turn = Turn}) ->
    WithinBounds = Row >= 0 andalso Row < 3 andalso Col >= 0 andalso Col < 3,
    IsCorrectPlayer = (Symbol == <<"x">> andalso From == X) orelse
                      (Symbol == <<"o">> andalso From == O),
    IsTurn = Symbol == Turn,
    IsEmpty = lists:nth(Col + 1, lists:nth(Row + 1, Board)) == <<"">>,

    WithinBounds andalso IsCorrectPlayer andalso IsTurn andalso IsEmpty.

update_board(Board, Row, Col, Symbol) ->
  element(1, lists:mapfoldl(fun(R, I) ->
        if I == Row ->
            {UpdatedRow, _} = lists:mapfoldl(fun(C, J) ->
                if J == Col -> {Symbol, J + 1}; true -> {C, J + 1} end
            end, 0, R),
            {UpdatedRow, I + 1};
        true ->
            {R, I + 1}
        end
    end, 0, Board)).


check_winner(Board) ->
    Lines = Board ++
        transpose(Board) ++
        [[lists:nth(1, lists:nth(1, Board)),
          lists:nth(2, lists:nth(2, Board)),
          lists:nth(3, lists:nth(3, Board))],
         [lists:nth(3, lists:nth(1, Board)),
          lists:nth(2, lists:nth(2, Board)),
          lists:nth(1, lists:nth(3, Board))]],
    case lists:filter(fun(Line) ->
             Line == [<<"x">>, <<"x">>, <<"x">>] orelse
             Line == [<<"o">>, <<"o">>, <<"o">>]
         end, Lines) of
        [[Winner,_,_]] -> Winner;
        _ -> <<"">>
    end.

transpose([[A1,A2,A3],[B1,B2,B3],[C1,C2,C3]]) ->
    [[A1,B1,C1],[A2,B2,C2],[A3,B3,C3]].

next_turn(<<"x">>) -> <<"o">>;
next_turn(<<"o">>) -> <<"x">>.

send_to_players(MsgTuple, #state{player_x = X, player_o = O}) ->
    X ! MsgTuple,
    O ! MsgTuple.

get_other_player(Pid, #state{player_x = X, player_o = O}) when Pid == X -> O;
get_other_player(Pid, #state{player_x = X, player_o = O}) when Pid == O -> X;
get_other_player(_, _) -> undefined.
