maze
=====

多种迷宫生成算法实现的Erlang库，包含深度优先算法、回溯算法、prime算法等

额外附带一个岛屿生成算法，基于原细胞自动机算法

Build
-----

    $ rebar3 compile

How to use
----

    1> Width = 10.          
    10
    2> Hight = 10.
    10
    3> dfs_maze:create_maze(Width,Hight).
    {{1,0,1,1,1,1,1,0,1,1,1},
    {1,0,1,0,1,0,1,0,1,0,0},
    {1,0,1,0,1,0,1,0,1,1,1},
    {1,0,0,0,1,0,1,0,0,0,1},
    {1,1,1,1,1,0,1,1,1,0,1},
    {0,0,0,0,0,0,0,0,1,0,1},
    {1,1,1,1,1,1,1,0,1,1,1},
    {1,0,1,0,0,0,0,0,0,0,1},
    {1,0,1,0,1,1,1,1,1,0,1},
    {1,0,1,0,1,0,1,0,0,0,1},
    {1,0,1,1,1,0,1,1,1,1,1}}
    4> util:print_maze(v(3)).
    XXXXXXXXXXXXX
    X X     X   X
    X X X X X XXX
    X X X X X   X
    X XXX X XXX X
    X     X   X X
    XXXXXXXXX X X
    X       X   X
    X X XXXXXXX X
    X X X     X X
    X X X X XXX X
    X X   X     X
    XXXXXXXXXXXXX
    ok



