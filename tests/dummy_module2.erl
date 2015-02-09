-module(dummy_module2).

-export([f1/2, f2/1]).

-type t1() :: 't1'.

-record(r0, {f01}).
-record(r1, {f11 :: t1(), f12 = #r0{}, f13 :: integer()}).
-record(r2, {f21 = #r1{}, f22, f23}).
-record(r3, {f31, f32, f33}).

-type t3() :: #r3{}.

f1(X, Y = #r3{}) ->
    #r2{f22 = X, f23 = Y}.

-spec f2(#r3{}) -> #r3{}.
f2(X = #r3{}) ->
    X.
