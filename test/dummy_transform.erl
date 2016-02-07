%%%========================================================================
%%% File: dummy_transform.erl
%%%
%%% A dummy parse transform used for testing. The parse transform
%%% implemented in this module adds a to_text/1 function which converts
%%% integers from 1 to 5 to its text representation (e.g. 1 becomes "one").
%%%
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   November, 2014
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014-2016 Enrique Fernandez
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining
%%% a copy of this software and associated documentation files (the
%%% "Software"), to deal in the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software,
%%% and to permit persons to whom the Software is furnished to do so,
%%% subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%========================================================================
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
