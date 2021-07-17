%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 回溯算法生成的迷宫
%%% @end
%%% Created : 17. 7月 2021 11:25
%%%-------------------------------------------------------------------
-module(recursive_maze).

%% API
-export([create_maze/2]).

%%%=================================================================
%%% API Functions
%%%=================================================================
create_maze(Width, High) ->
    Width1 = Width div 2 * 2 + 1,
    High1 = High div 2 * 2 + 1,
    Maze = list_to_tuple(lists:duplicate(High1, list_to_tuple(lists:duplicate(Width1, 1)))),
    gen_maze(Maze, 1, 1, Width1, High1).

gen_maze(Maze, WStart, HStart, WEnd, HEnd) ->
    case WEnd - WStart > 2 andalso HEnd - HStart > 2 of
        true ->
            X = util:rand(WStart + 1, WEnd) div 2 * 2,
            Y = util:rand(HStart + 1, HEnd) div 2 * 2,
            io:format("WStart:~w, HStart:~w, WEnd:~w, HEnd:~w X:~w Y:~w~n", [WStart, HStart, WEnd, HEnd, X, Y]),
            Walls = get_walls(X, Y, WStart, HStart, WEnd, HEnd),
            Maze1 = lists:foldl(fun util:add_wall/2, Maze, Walls),
            util:print_maze(Maze1),
            Maze2 = gen_maze(Maze1, WStart, HStart, X - 1, Y - 1),
            Maze3 = gen_maze(Maze2, X + 1, 1, WEnd, Y - 1),
            Maze4 = gen_maze(Maze3, 1, Y + 1, X - 1, HEnd),
            gen_maze(Maze4, X + 1, Y + 1, WEnd, HEnd);
        false ->
            Maze
    end.

get_walls(X, Y, WStart, HStart, WEnd, HEnd) ->
    AllWalls = [
        {rand:uniform(), [{rand:uniform(), {X1, Y}} || X1 <- lists:seq(WStart, X - 1)]},
        {rand:uniform(), [{rand:uniform(), {X1, Y}} || X1 <- lists:seq(X + 1, WEnd)]},
        {rand:uniform(), [{rand:uniform(), {X, Y1}} || Y1 <- lists:seq(HStart, Y - 1)]},
        {rand:uniform(), [{rand:uniform(), {X, Y1}} || Y1 <- lists:seq(Y + 1, HEnd)]}
    ],
    [{_, Walls1}, {_, Walls2}, {_, Walls3}, {_, Walls4}] = lists:sort(AllWalls),
    AllWalls1 = tl(lists:sort(Walls1)) ++ tl(lists:sort(Walls2)) ++ tl(lists:sort(Walls3)) ++ lists:sort(Walls4),
    [{X, Y} | [Wall || {_, Wall} <- AllWalls1]].