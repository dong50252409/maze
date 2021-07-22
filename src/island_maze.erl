%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 基于元细胞自动机生成的迷宫
%%% @end
%%% Created : 16. 7月 2021 20:42
%%%-------------------------------------------------------------------
-module(island_maze).

%% API
-export([create_maze/2, create_maze/3]).

%%%=================================================================
%%% API Functions
%%%=================================================================
-spec create_maze(Width :: non_neg_integer(), High :: non_neg_integer()) -> Maze :: tuple().
create_maze(Width, High) ->
    create_maze(Width, High, 0.40).

-spec create_maze(Width :: non_neg_integer(), High :: non_neg_integer(), Rate :: 0..1) -> Maze :: tuple().
create_maze(Width, High, Rate) ->
    Maze = list_to_tuple(lists:duplicate(High, list_to_tuple(lists:duplicate(Width, 1)))),
    Grids = [{rand:uniform(), {X, Y}} || X <- lists:seq(1, Width), Y <- lists:seq(1, High)],
    ShuffleGrids = [Grid || {_, Grid} <- lists:sort(Grids)],
    Blocks = lists:sublist(ShuffleGrids, trunc(Width * High * Rate)),
    Maze1 = lists:foldl(fun maze_util:add_wall/2, Maze, Blocks),

    Regulars1 = [regular_1(Width, High, 1, 5), regular_2(Width, High, 2, 2)],
    Maze2 = iterator_regular(Regulars1, {1, 1}, Width, High, Maze1, Maze1, 3),

    Regulars2 = [regular_1(Width, High, 1, 5)],
    iterator_regular(Regulars2, {1, 1}, Width, High, Maze2, Maze2, 4).

%%%=================================================================
%%% Internal Functions
%%%=================================================================
iterator_regular(Regulars, {X, Y} = Grid, Width, High, PrevMaze, Maze, N) when N > 0 ->
    case any(Regulars, Grid, PrevMaze) of
        true ->
            Maze1 = maze_util:add_wall(Grid, Maze);
        false ->
            Maze1 = maze_util:rem_wall(Grid, Maze)
    end,
    if
        X < Width ->
            iterator_regular(Regulars, {X + 1, Y}, Width, High, PrevMaze, Maze1, N);
        Y < High ->
            iterator_regular(Regulars, {1, Y + 1}, Width, High, PrevMaze, Maze1, N);
        true ->
            iterator_regular(Regulars, {1, 1}, Width, High, Maze1, Maze1, N - 1)
    end;
iterator_regular(_Regulars, _Grid, _Width, _High, _PrevMaze, Maze, _N) ->
    Maze.

any([Regular | T], Grid, Maze) ->
    Regular(Grid, Maze) orelse any(T, Grid, Maze);
any([], _Grid, _Maze) ->
    false.

regular_1(Width, High, Round, Num) ->
    fun(Grid, Maze) ->
        Directions = get_round_grids(Round),
        get_neighbour_blocks(Grid, Width, High, Maze, Directions) >= Num
    end.

regular_2(Width, High, Round, Num) ->
    fun(Grid, Maze) ->
        Directions = get_round_grids(Round),
        get_neighbour_blocks(Grid, Width, High, Maze, Directions) =< Num
    end.

get_round_grids(Round) ->
    [{DX, DY} || DX <- lists:seq(-Round, Round), DY <- lists:seq(-Round, Round)] -- [{0, 0}].

get_neighbour_blocks({X, Y}, Width, High, Maze, [{DX, DY} | T]) ->
    X1 = X + DX,
    Y1 = Y + DY,
    case X1 =< 0 orelse X1 > Width orelse Y1 =< 0 orelse Y1 > High orelse maze_util:is_wall({X1, Y1}, Maze) of
        true ->
            1 + get_neighbour_blocks({X, Y}, Width, High, Maze, T);
        false ->
            get_neighbour_blocks({X, Y}, Width, High, Maze, T)
    end;
get_neighbour_blocks(_Grid, _Width, _High, _Maze, []) ->
    0.
