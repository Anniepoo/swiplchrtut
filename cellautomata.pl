
% http://mathworld.wolfram.com/CellularAutomaton.html
%
:- use_module(library(chr)).

:- chr_constraint cell_rule/4, cell/5, init_cells/0, init_a_cell/1.

init :-
%         L  C  R  N
    cell_rule(w, w, w, w),
    cell_rule(w, w, b, b),
    cell_rule(w, b, w, b),
    cell_rule(w, b, b, b),
    cell_rule(b, w, w, b),
    cell_rule(b, w, b, w),
    cell_rule(b, b, w, w),
    cell_rule(b, b, b, w),
    init_cells.

% cells are indexed 0 to 19
% cells 10 starts black, others start white

% propagate new generation
% we're in the middle and can figure out color from
% rules
cell(Pos, R, _, N, LClr),
cell(_, Pos, _, N, PosClr),
cell(_, L, Pos, N, RClr),
cell_rule(LClr, PosClr, RClr, NewClr)
    ==>
    N < 10 |                  % stop after 10 generations
    succ(N, NN),
    cell(L, Pos, R, NN, NewClr).
cell(_, 0, _, N, _) ==>
    N < 10 |
    succ(N, NN),
    cell(_, 0, _, NN, w).
cell(_, 19, _, N, _) ==>
    N < 10 |
    succ(N, NN),
    cell(_, 19, _, NN, w).

init_cells <=> init_a_cell(19).

init_a_cell(0) <=>
    cell(none, 0, 1, 1, w).
init_a_cell(19) <=>
    cell(18, 19, none, 1, w),
    init_a_cell(18).
init_a_cell(N) <=>
    succ(L, N),
    succ(N, R),
    cell(L, N, R, 1, w),
    init_a_cell(L).


print_cells :-
    between(0, 19, Row),
    get_line(Row, Line),
    format('~s~n', [Line]),
    fail.
print_cells.

:- chr_constraint   get_line/2, temp_cell/3, collect_line/3.

get_line(Row, _), cell(_, Row, _, N, Val) ==>
    temp_cell(Row, N, Val).
get_line(Row, Line) <=> collect_line(Row, 1, Line).

collect_line(_, Gen, Out) <=> Gen >= 10 | Out = [].
collect_line(Row, Gen, Out) \
    temp_cell(Row, Gen, Val) <=>
    succ(Gen, NG),
    convert_val(Val, OutVal),
    Out = [OutVal | Rest],
    collect_line(Row, NG, Rest).

convert_val(b, 0'b).
convert_val(w, 0'w).
convert_val(none, 0'!).
convert_val(_, 0'?).






