%%%========================================================================
%%% File: forms_pt.erl
%%%
%%% Parse transform allowing developers to programatically define functions
%%% using Erlang syntax (as opposed to abstract code).
%%%
%%% When this parse transform finds a call to the `forms:function/3`
%%% pseudo-function, it applies some transformations and replaces it
%%% by a call to `forms_pt:gen_function/4`.
%%%
%%% The following code
%%%
%%% Name = int_to_text,
%%% Bindings = [
%%%             [{'X', 1}, {'Y', "one"}],
%%%             [{'X', 2}, {'Y', "two"}]
%%%            ],
%%% forms:function(Name,
%%%                [
%%%                   {fun(X) -> Y end, Bindings},
%%%                   {fun(_) -> {error, invalid_input} end, []}
%%%                ]).
%%%
%%% generates the function below
%%%
%%% int_to_text(1) -> "one";
%%% int_to_text(2) -> "two";
%%% int_to_text(_) -> {error, invalid_input}.
%%%
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   January, 2015
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2015 Enrique Fernandez
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
-module(forms_pt).

%% Parse transform
-export([parse_transform/2]).
%% Functions used by the parse transform
-export([gen_function/3]).


%% ========================================================================
%%  Parse transform
%% ========================================================================

parse_transform(Forms, _Options) ->
    forms:map(
      fun({call, _,
           {remote, _,
            {atom, _, forms},
            {atom, _, function}},
           [
            Name,
            {cons, _,
             {tuple, _,
              [{'fun', _,
                {clauses,
                 [{clause, _, Args, _Guards, _Body}| _]}},
               _Bindings]},
             _Tail} = Clauses
           ]}) ->
              Arity = length(Args),
              %%AbsArity = to_abstract(AbsArity),

              QuotedClauses =
                  lists:map(fun({tuple, _,
                                 [{'fun', _, {clauses, Cs}}, Bs]}) ->
                                    {tuple, 0, [forms:quote(Cs), Bs]}
                            end,
                            forms:cons_to_list(Clauses)),

              QuotedClauses1 = forms:list_to_cons(QuotedClauses),

              %% We cannot pass the function clauses as is because it
              %% would not be a valid abstract code for a function call.
              %% This is why we convert the function clauses in a binary
              %% that we can pass as an argument to the
              %% `forms_pt:gen_function/4` function.
              %% QuotedClauses = forms:quote(Clauses),

              %% Replace `forms:function/3` pseudo-call by a call to
              %% `forms_pt:gen_function/4`, which generates a function
              %% and returns its abstract code.
              {call,0,{remote,0,{atom,0,?MODULE},{atom,0,gen_function}},
               [
                Name,
                %% to_abstract(length(Args)),
                {integer, 0, Arity},
                QuotedClauses1
               ]};
         (Other) ->
              Other
      end,
      Forms).


%% =========================================================================
%%  Utility functions
%% =========================================================================

gen_function(Name, Arity, QuotedClauses)
  when is_atom(Name) andalso
       is_integer(Arity) ->
    BoundClauses = bind(QuotedClauses),
    {function, 0, Name, Arity, BoundClauses}.

bind(Forms) ->
    bind(Forms, []).

bind([], Acc) ->
    Acc;
bind([{QuotedClauses, []}| Tail], Acc) ->
    UnquotedClauses = forms:unquote(QuotedClauses),
    Acc1 = lists:append([Acc, UnquotedClauses]),
    bind(Tail, Acc1);
bind([{QuotedClauses, Bindings}| Tail], Acc) ->
    UnquotedClauses = forms:unquote(QuotedClauses),
    BoundClauses =
        lists:flatmap(
          fun(B) ->
                  forms:map(
                    fun({var, Line, Var}) ->
                            case proplists:get_value(Var, B) of
                                undefined ->
                                    {var, Line, Var};
                                Value ->
                                    to_abstract(Value)
                            end;
                       (Other) ->
                            Other
                    end,
                    UnquotedClauses)
          end,
          Bindings),
    Acc1 = lists:append([Acc, BoundClauses]),
    bind(Tail, Acc1).

to_abstract(Element) ->
    X1 = lists:flatten(io_lib:format("~p", [Element])),
    [X2] = forms:to_abstract(
             lists:append([X1, "."])),
    X2.
