-module(util).

%% API
-export([
    print_maze/1,
    rand/2,
    is_wall/2, add_wall/2, rem_wall/2,
    get_directions/4
]).

print_maze(Maze) ->
    Fun =
        fun(Index) ->
            ["X", [convert(E) || E <- tuple_to_list(element(Index, Maze))], "X\n"]
        end,
    Width = size(element(1, Maze)),
    High = size(Maze),
    Borders = [lists:duplicate(Width + 2, "X"), "\n"],
    io:format("~ts", [[Borders, lists:map(Fun, lists:seq(1, High)), Borders]]).

convert(0) ->
    "X";
convert(1) ->
    " ".

rand(Min, Max) ->
    M = Min - 1,
    rand:uniform(Max - M) + M.

is_wall({X, Y}, Maze) ->
    element(X, element(Y, Maze)) =:= 0.

add_wall({X, Y}, Maze) ->
    setelement(Y, Maze, setelement(X, element(Y, Maze), 0)).

rem_wall({X, Y}, Maze) ->
    setelement(Y, Maze, setelement(X, element(Y, Maze), 1)).

get_directions({X, Y}, Distance, Width, High) ->
    [
        {X + DX, Y + DY} ||
        {DX, DY} <- [{Distance, 0}, {-Distance, 0}, {0, Distance}, {0, -Distance}],
        X + DX > 0 andalso X + DX =< Width andalso Y + DY > 0 andalso Y + DY =< High
    ].