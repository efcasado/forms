-module(dummy_transform).

-compile({parse_transform, forms_pt}).

-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
    Bindings1 = [
                 [{'X', 1}, {'Y', "one"}],
                 [{'X', 2}, {'Y', "two"}],
                 [{'X', 3}, {'Y', "three"}],
                 [{'X', 4}, {'Y', "four"}],
                 [{'X', 5}, {'Y', "five"}]
                ],
    ToTextFunction =
        forms:function(to_text,
                       [
                        {fun(X) -> Y end, Bindings1},
                        {fun(_) -> {error, invalid_input} end, []}
                       ]),
    [EOF = {eof, _}| OtherForms] = lists:reverse(Forms),
    lists:reverse([EOF, ToTextFunction| OtherForms]).
