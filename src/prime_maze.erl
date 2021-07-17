%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%% 基于prime算法生成的迷宫
%%% @end
%%% Created : 16. 7月 2021 20:42
%%%-------------------------------------------------------------------
-module(prime_maze).

-export([create_maze/2]).

%%-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.
-type maze() :: tuple().

%%%=================================================================
%%% API Functions
%%%=================================================================
-spec create_maze(Width :: non_neg_integer(), High :: non_neg_integer()) -> maze().
create_maze(Width, High) ->
    Width1 = Width div 2 * 2 + 1,
    High1 = High div 2 * 2 + 1,
    Maze = list_to_tuple(lists:duplicate(High1, list_to_tuple(lists:duplicate(Width1, 0)))),
    Walls = orddict:store(0, {{1, 1}, {1, 1}}, orddict:new()),
    gen_maze(Maze, Walls, Width1, High1).

%%%=================================================================
%%% Internal Functions
%%%=================================================================
get_ways([Grid | T], Maze) ->
    case util:is_wall(Grid, Maze) of
        true ->
            get_ways(T, Maze);
        false ->
            1 + get_ways(T, Maze)
    end;
get_ways([], _Maze) ->
    0.

add_walls([{X, Y} = Grid | T], {PX, PY} = ParentGrid, Maze, Walls, Width, High) ->
    {WX, WY} = WallGrid = {PX + max(min(X - PX, 1), -1), PY + max(min(Y - PY, 1), -1)},
    case WX > 0 andalso WX =< Width andalso WY > 0 andalso WY =< High andalso util:is_wall(Grid, Maze) of
        true ->
            Walls1 = orddict:store(rand:uniform(), {Grid, WallGrid}, Walls),
            add_walls(T, ParentGrid, Maze, Walls1, Width, High);
        false ->
            add_walls(T, ParentGrid, Maze, Walls, Width, High)
    end;
add_walls([], _ParentGrid, _Maze, Walls, _Width, _High) ->
    Walls.

gen_maze(Maze, [], _Width, _High) ->
    Maze;
gen_maze(Maze, [{_, {Grid, ParentGrid}} | Walls], Width, High) ->
    Directions1 = util:get_directions(Grid, 1, Width, High),
    case get_ways(Directions1, Maze) of
        Count when Count < 1 ->
            Maze1 = util:rem_wall(Grid, Maze),
            Maze2 = util:rem_wall(ParentGrid, Maze1),
            Directions2 = util:get_directions(Grid, 2, Width, High),
            Walls1 = add_walls(Directions2, Grid, Maze2, Walls, Width, High),
            gen_maze(Maze2, Walls1, Width, High);
        _Count ->
            gen_maze(Maze, Walls, Width, High)
    end.

