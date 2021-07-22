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
-export([create_maze/2, create_maze/4]).

%%%=================================================================
%%% API Functions
%%%=================================================================
-spec create_maze(Width :: non_neg_integer(), High :: non_neg_integer()) -> Maze :: tuple().
create_maze(Width, High) ->
    create_maze(Width, High, 1, 1).

-spec create_maze(Width :: non_neg_integer(), High :: non_neg_integer(), I :: non_neg_integer(), J :: non_neg_integer()) -> Maze :: tuple().
create_maze(Width, High, I, J) ->
    Width1 = Width div 2 * 2 + 1,
    High1 = High div 2 * 2 + 1,
    Maze = list_to_tuple(lists:duplicate(High1, list_to_tuple(lists:duplicate(Width1, 1)))),
    gen_maze(Maze, [{1, 1, Width1, High1}], I, J).

%%%=================================================================
%%% Internal Functions
%%%=================================================================
gen_maze(Maze, [{WStart, HStart, WEnd, HEnd} | T], I, J) ->
    case WEnd - WStart > I andalso HEnd - HStart > J of
        true ->
            X = maze_util:rand(WStart + 1, WEnd) div 2 * 2,
            Y = maze_util:rand(HStart + 1, HEnd) div 2 * 2,
            Walls = get_walls(X, Y, WStart, HStart, WEnd, HEnd),
            Maze1 = lists:foldl(fun maze_util:add_wall/2, Maze, Walls),
            Heap = [{WStart, HStart, X - 1, Y - 1}, {X + 1, HStart, WEnd, Y - 1},
                {WStart, Y + 1, X - 1, HEnd}, {X + 1, Y + 1, WEnd, HEnd} | T],
            gen_maze(Maze1, Heap, I, J);
        false ->
            gen_maze(Maze, T, I, J)
    end;
gen_maze(Maze, [], _I, _J) ->
    Maze.

get_walls(X, Y, WStart, HStart, WEnd, HEnd) ->
    AllWalls = [
        {rand:uniform(), [{rand:uniform(), {X1, Y}} || X1 <- lists:seq(WStart, X - 1)]},
        {rand:uniform(), [{rand:uniform(), {X1, Y}} || X1 <- lists:seq(X + 1, WEnd)]},
        {rand:uniform(), [{rand:uniform(), {X, Y1}} || Y1 <- lists:seq(HStart, Y - 1)]},
        {rand:uniform(), [{rand:uniform(), {X, Y1}} || Y1 <- lists:seq(Y + 1, HEnd)]}
    ],
    [{_, Walls1}, {_, Walls2}, {_, Walls3}, {_, Walls4}] = lists:sort(AllWalls),
    AllWalls1 = get_walls_1(lists:sort(Walls1))
        ++ get_walls_1(lists:sort(Walls2))
        ++ get_walls_1(lists:sort(Walls3))
        ++ lists:sort(Walls4),
    [{X, Y} | [Wall || {_, Wall} <- AllWalls1]].

get_walls_1([{_, {X, Y}} | T]) when X band 1 =:= 1; Y band 1 =:= 1 ->
    T;
get_walls_1([Grid | T]) ->
    [Grid | get_walls_1(T)].