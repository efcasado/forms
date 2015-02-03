-module(dummy_transform).

-compile({parse_transform, forms_pt}).

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    ToTextFunction =
        forms:function(to_text,
                       fun(X1) -> Y1;
                          (X2) -> Y2;
                          (X3) -> Y3;
                          (X4) -> Y4;
                          (X5) -> Y5;
                          (_)  -> {error, invalid_input}
                       end,
                       [
                        {'X1', 1}, {'Y1', "one"},
                        {'X2', 2}, {'Y2', "two"},
                        {'X3', 3}, {'Y3', "three"},
                        {'X4', 4}, {'Y4', "four"},
                        {'X5', 5}, {'Y5', "five"}
                       ]),
    [EOF = {eof, _}| OtherForms] = lists:reverse(Forms),
    lists:reverse([EOF, ToTextFunction| OtherForms]).
