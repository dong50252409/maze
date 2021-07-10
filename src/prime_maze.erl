-module(prime_maze).

-export([create_maze/4]).

-type grid() :: {X :: non_neg_integer(), Y :: non_neg_integer()}.
-type maze() :: tuple().

%%%=================================================================
%%% API Functions
%%%=================================================================
-spec create_maze(Start :: grid(), End :: grid(), Width :: non_neg_integer(), High :: non_neg_integer()) -> maze().
create_maze(Start, End, Width, High) ->
    Maze = list_to_tuple(lists:duplicate(High, list_to_tuple(lists:duplicate(Width, 0)))),
    Maze1 = erase_wall(Start, Maze),
    Maze2 = erase_wall(End, Maze1),
    Walls = gb_trees:enter(0, {Width div 2, High div 2}, gb_trees:empty()),
    Visited = #{},
    gen_maze(Maze2, Walls, Visited, Width, High).

%%%=================================================================
%%% Internal Functions
%%%=================================================================
get_direction({X, Y}, Width, High) ->
    [
        {X + DX, Y + DY} ||
        {DX, DY} <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
        X + DX > 0 andalso X + DX =< Width andalso Y + DY > 0 andalso Y + DY =< High
    ].

check_ways([{X, Y} | T], Maze) ->
    element(X, element(Y, Maze)) + check_ways(T, Maze);
check_ways([], _Maze) ->
    0.

erase_wall({X, Y}, Maze) ->
    Row = element(Y, Maze),
    Col = setelement(X, Row, 1),
    setelement(Y, Maze, Col).

add_walls([{X, Y} = Grid | T], Walls, Maze, Visited) ->
    case element(X, element(Y, Maze)) of
        0 ->
            {Walls1, Visited1} = add_wall(Grid, Walls, Visited),
            add_walls(T, Walls1, Maze, Visited1);
        1 ->
            add_walls(T, Walls, Maze, Visited)
    end;
add_walls([], Walls, _Maze, Visited) ->
    {Walls, Visited}.

add_wall(Grid, Walls, Visited) when is_map_key(Grid, Visited) ->
    {Walls, Visited};
add_wall(Grid, Walls, Visited) ->
    Walls1 = gb_trees:enter(rand:uniform(), Grid, Walls),
    {Walls1, Visited#{Grid => true}}.

pop_wall(Walls) ->
    {_, Grid, Walls1} = gb_trees:take_smallest(Walls),
    {Grid, Walls1}.

gen_maze(Maze, {0, nil}, _Visited, _Width, _High) ->
    Maze;
gen_maze(Maze, Walls, Visited, Width, High) ->
    {Grid, Walls1} = pop_wall(Walls),
    Directions = get_direction(Grid, Width, High),
    case check_ways(Directions, Maze) of
        Count when Count =< 1 ->
            Maze1 = erase_wall(Grid, Maze),
            {Walls2, Visited1} = add_walls(Directions, Walls1, Maze1, Visited),
            gen_maze(Maze1, Walls2, Visited1, Width, High);
        _Count ->
            gen_maze(Maze, Walls1, Visited, Width, High)
    end.

