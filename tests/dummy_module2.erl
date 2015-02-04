-module(dummy_module2).

-export([foo/0]).

foo() ->
    _ = 1,
    _ = 2,
    _ = 3,
    _ = 4,
    _ = 5,
    Oops = [ oops || _ <- lists:seq(1,5) ],
    bar(Oops),
    foo.

bar([]) ->
    oops;
bar([_| Tail]) ->
    bar(Tail).
