-ifndef(TIC_TAC_TOE_HRL).
-define(TIC_TAC_TOE_HRL, true).

-record(state, {
    board = [["", "", ""], ["", "", ""], ["", "", ""]],
    player_x,
    player_o,
    turn = <<"x">>
}).

-define(EMPTY_CELL, <<"">>).
-define(SYMBOL_X, <<"x">>).
-define(SYMBOL_O, <<"o">>).
-define(LOG_INFO(Format, Args), logger:info(Format, Args)).
-define(LOG_WARNING(Format, Args), logger:warning(Format, Args)).

-endif.
