%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 基于深度优先生成的迷宫
%%% @end
%%% Created : 16. 7月 2021 20:42
%%%-------------------------------------------------------------------
-module(dfs_maze).

%% API
-export([create_maze/2]).

create_maze(Width, High) ->
    Width1 = Width div 2 * 2 + 1,
    High1 = High div 2 * 2 + 1,
    Maze = list_to_tuple(lists:duplicate(High1, list_to_tuple(lists:duplicate(Width1, 0)))),
    Maze1 = util:rem_wall({1, 1}, Maze),
    Heap = [{1, 1}],
    gen_maze(Maze1, Heap, Width1, High1).

gen_maze(Maze, [{X, Y} = Grid | T] = Heap, Width, High) ->
    Directions = util:get_directions(Grid, 2, Width, High),
    case choice_wall(Directions, Maze) of
        none ->
            gen_maze(Maze, T, Width, High);
        {NX, NY} = ChoiceGrid ->
            Maze1 = util:rem_wall(ChoiceGrid, Maze),
            WallGrid = {NX + max(min(X - NX, 1), -1), NY + max(min(Y - NY, 1), -1)},
            Maze2 = util:rem_wall(WallGrid, Maze1),
            gen_maze(Maze2, [ChoiceGrid | Heap], Width, High)
    end;
gen_maze(Maze, [], _Width, _High) ->
    Maze.

choice_wall(Directions, Maze) ->
    case lists:sort(get_walls(Directions, Maze)) of
        [] ->
            none;
        [{_, Grid} | _] ->
            Grid
    end.

get_walls([Grid | T], Maze) ->
    case util:is_wall(Grid, Maze) of
        true ->
            [{rand:uniform(), Grid} | get_walls(T, Maze)];
        false ->
            get_walls(T, Maze)
    end;
get_walls([], _Maze) ->
    [].