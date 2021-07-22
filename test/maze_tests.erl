%%%-------------------------------------------------------------------
%%% @author gz1417
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. 7月 2021 10:53
%%%-------------------------------------------------------------------
-module(maze_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    DFSMaze = dfs_maze:create_maze(100, 100),
    ?assert(is_tuple(DFSMaze)),
    maze_util:draw_maze("test/dfs_maze", DFSMaze),
    IsLandMaze = island_maze:create_maze(100, 100),
    ?assert(is_tuple(IsLandMaze)),
    maze_util:draw_maze("test/island_maze", IsLandMaze),
    PrimeMaze = prime_maze:create_maze(100, 100),
    ?assert(is_tuple(PrimeMaze)),
    maze_util:draw_maze("test/prime_maze", PrimeMaze),
    RecurMaze = recursive_maze:create_maze(100, 100),
    ?assert(is_tuple(RecurMaze)),
    maze_util:draw_maze("test/recursive_maze", RecurMaze),
    ?assert(true).
