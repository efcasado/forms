%%%========================================================================
%%% File: forms.erl
%%%
%%% Collection of functions that simplify working with Erlang abstract
%%% forms.
%%%
%%% This work has been inpired by the excellent work done by Ulf Wiger in
%%% parse_trans (https://github.com/uwiger/parse_trans).
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   November, 2014
%%%
%%%-- LICENSE -------------------------------------------------------------
%%% The MIT License (MIT)
%%%
%%% Copyright (c) 2014 Enrique Fernandez
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
-module(forms).

%% API
-export(
   [
    read/1,
    map/2,
    reduce/3,
    mr/3,
    %% Debug functions
    from_abstract/1,
    to_abstract/1
   ]).

%% ========================================================================
%%  Type definitions
%% ========================================================================

-type form()  :: erl_parse:abstract_form().
-type forms() :: list(form()).
-type mapf()  :: fun((form()) -> any()).
-type redf()  :: fun((form(), any()) -> any()).
-type mrf()   :: fun((form(), any()) -> {any(), any()}).


%% ========================================================================
%%  API
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Read the Erlang abstract forms from the specified source file or binary
%% compiled using the -debug_info compile option.
%% @end
%%-------------------------------------------------------------------------
-spec read(atom() | iolist()) -> forms().
read(Module) when is_atom(Module) ->
    {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} =
        beam_lib:chunks(code:which(Module), [abstract_code]),
    Forms;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Calls the provided fun/1 on all given forms, including nested forms.
%% The original forms are replaced by the resulting Erlang term after
%% applying the provided fun/1 on them.
%% @end
%%-------------------------------------------------------------------------
-spec map(mapf(), forms()) -> forms().
map(Fun, Fs) ->
    map(Fun, [], Fs).

map(_Fun, Acc, []) ->
    lists:reverse(Acc);
map(Fun, Acc, [F| Fs]) when is_list(F) ->
    map(Fun, [map(Fun, F)| Acc], Fs);
map(Fun, Acc, [F| Fs]) ->
    case Fun(F) of
        {next, T} ->
            map(Fun, [T| Acc], Fs);
        T when is_tuple(T) ->
            map(Fun, [list_to_tuple(map(Fun, tuple_to_list(T)))| Acc], Fs);
        F1 ->
            map(Fun, [F1| Acc], Fs)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Calls the provided fun/2 on all given forms, including nested forms.
%% fun/2 must return a new accumulator which is passed to the next call.
%% @end
%%-------------------------------------------------------------------------
-spec reduce(redf(), any(), forms()) -> any().
reduce(_Fun, Acc, []) ->
    Acc;
reduce(Fun, Acc, [F| Fs]) when is_tuple(F) ->
    reduce(Fun, reduce(Fun, Fun(F, Acc), tuple_to_list(F)), Fs);
reduce(Fun, Acc, [F| Fs]) when is_list(F) ->
    reduce(Fun, reduce(Fun, Acc, F), Fs);
reduce(Fun, Acc, [F| Fs]) ->
    reduce(Fun, Fun(F, Acc), Fs).

%%-------------------------------------------------------------------------
%% @doc
%% Combines the operations of map/2 and reduce/3 into one pass.
%% @end
%%-------------------------------------------------------------------------
-spec mr(mrf(), any(), forms()) -> {any(), any()}.
mr(Fun, Acc, Fs) ->
    mr(Fun, Acc, [], Fs).

mr(_Fun, Acc1, Acc2, []) ->
    {Acc1, lists:reverse(Acc2)};
mr(Fun, Acc1, Acc2, [F| Fs]) when is_list(F) ->
    {NewAcc1, NewAcc2} = mr(Fun, Acc1, F),
    mr(Fun, NewAcc1, [NewAcc2| Acc2], Fs);
mr(Fun, Acc1, Acc2, [F| Fs]) ->
    case Fun(F, Acc1) of
        {Acc, {next, T}} ->
            mr(Fun, Acc, [T| Acc2], Fs);
        {Acc, T} when is_tuple(T) ->
            {NewAcc1, NewAcc2} = mr(Fun, Acc, tuple_to_list(T)),
            mr(Fun, NewAcc1, [list_to_tuple(NewAcc2)| Acc2], Fs);
        {Acc, F1} ->
            mr(Fun, Acc, [F1| Acc2], Fs)
    end.


%% ========================================================================
%%  Debug functions
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Turn the provided Erlang attribute or expression into its abstract
%% format representation.
%% @end
%%-------------------------------------------------------------------------
-spec to_abstract(string()) -> form().
to_abstract(String) ->
    {ok, Tokens, _EndLocation} =
        erl_scan:string(String),
    {ok, AbsForm} =
        try
            {ok, _} = erl_parse:parse_form(Tokens)
        catch
            _:_ ->
                {ok, _} = erl_parse:parse_exprs(Tokens)
        end,
    AbsForm.

%%-------------------------------------------------------------------------
%% @doc
%% Turn the provided abstract form into an Erlang representation.
%% @end
%%-------------------------------------------------------------------------
-spec from_abstract(form()) -> string().
from_abstract(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
from_abstract(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).
