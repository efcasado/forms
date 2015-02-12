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
%%% Copyright (c) 2014-2015 Enrique Fernandez
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
    quote/1,
    unquote/1,
    map/2, map/3,
    reduce/3, reduce/4,
    mr/3,
    filter/2,
    any/2,
    all/2,
    cons_to_list/1,
    list_to_cons/1,
    %% Debug functions
    eval/1,
    from_abstract/1,
    to_abstract/1
   ]).

%% ========================================================================
%%  Type definitions
%% ========================================================================

-type form()      :: erl_parse:abstract_form().
-type forms()     :: list(form()).
-type mapf()      :: fun((form()) -> any()).
-type redf()      :: fun((form(), any()) -> any()).
-type mrf()       :: fun((form(), any()) -> {any(), any()}).
-type predicate() :: fun((form()) -> boolean()).
-type opt()       :: 'forms_only'.
-type opts()      :: list(opt()).

%% ========================================================================
%%  Macro definitions
%% ========================================================================

%% Options supported by functions such as maps, reduce, etc.
-define(OPTS,
        [
         forms_only
        ]).


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
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {no_debug_info, _}} ->
            throw({forms_not_found, Module});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({module_not_found, Module})
    end;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Calls the provided fun/1 on all given forms, including nested forms.
%% The original forms are replaced by the resulting Erlang term after
%% applying the provided fun/1 on them.
%% @end
%%-------------------------------------------------------------------------
-spec map(mapf(), forms()) -> forms().
map(Fun, Forms) ->
    map(Fun, Forms, []).

-spec map(mapf(), forms(), opts()) -> forms().
map(Fun, Forms, Opts)
  when is_list(Opts) ->
    Opts1 = parse_opts(Opts),
    '_map'(Fun, [], Forms, Opts1);
map(Fun, Forms, Opts) ->
    '_map'(Fun, [], Forms, Opts).

'_map'(_Fun, Acc, [], _Opts) ->
    lists:reverse(Acc);
'_map'(Fun, Acc, [F| Fs], Opts) when is_list(F) ->
    '_map'(Fun, [map(Fun, F, Opts)| Acc], Fs, Opts);
'_map'(Fun, Acc, [F| Fs], Opts) ->
    case forms_only(Opts) of
        true ->
            case is_form(F) of
                true ->
                    case Fun(F) of
                        {next, T} ->
                            '_map'(Fun, [T| Acc], Fs, Opts);
                        T when is_tuple(T) ->
                            '_map'(Fun,
                                   [list_to_tuple(
                                      map(Fun, tuple_to_list(T), Opts))| Acc],
                                   Fs,
                                   Opts);
                        F1 ->
                            '_map'(Fun, [F1| Acc], Fs, Opts)
                    end;
                false ->
                    '_map'(Fun, [F| Acc], Fs, Opts)
            end;
        false ->
            case Fun(F) of
                {next, T} ->
                    '_map'(Fun, [T| Acc], Fs, Opts);
                T when is_tuple(T) ->
                    '_map'(Fun,
                           [list_to_tuple(
                              map(Fun, tuple_to_list(T), Opts))| Acc],
                           Fs,
                           Opts);
                F1 ->
                    '_map'(Fun, [F1| Acc], Fs, Opts)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Calls the provided fun/2 on all given forms, including nested forms.
%% fun/2 must return a new accumulator which is passed to the next call.
%% @end
%%-------------------------------------------------------------------------
-spec reduce(redf(), any(), forms()) -> any().
reduce(Fun, Acc, Forms) ->
    reduce(Fun, Acc, Forms, []).

-spec reduce(redf(), any(), forms(), opts()) -> any().
reduce(Fun, Acc, Forms, Opts) ->
    Opts1 = parse_opts(Opts),
    '_reduce'(Fun, Acc, Forms, Opts1).

'_reduce'(_Fun, Acc, [], _Opts) ->
    Acc;
'_reduce'(Fun, Acc, [F| Fs], Opts) when is_tuple(F) ->
    case forms_only(Opts) of
        true ->
            NewAcc =
                case is_form(F) of
                    true ->
                        Fun(F, Acc);
                    false ->
                        Acc
                end,
            '_reduce'(Fun,
                      '_reduce'(Fun, NewAcc, tuple_to_list(F), Opts),
                      Fs,
                      Opts);
        false ->
            '_reduce'(Fun,
                      '_reduce'(Fun, Fun(F, Acc), tuple_to_list(F), Opts),
                      Fs,
                      Opts)
    end;
'_reduce'(Fun, Acc, [F| Fs], Opts) when is_list(F) ->
    '_reduce'(Fun, '_reduce'(Fun, Acc, F, Opts), Fs, Opts);
'_reduce'(Fun, Acc, [F| Fs], Opts) ->
    case forms_only(Opts) of
        true ->
            case is_form(F) of
                true ->
                    '_reduce'(Fun, Fun(F, Acc), Fs, Opts);
                false ->
                    '_reduce'(Fun, Acc, Fs, Opts)
            end;
        false ->
            '_reduce'(Fun, Fun(F, Acc), Fs, Opts)
    end.

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
%% mr(Fun, Acc1, Acc2, [F| Fs]) ->
%%     case is_form(F) of
%%         true ->
%%             case Fun(F, Acc1) of
%%                 {Acc, {next, T}} ->
%%                     mr(Fun, Acc, [T| Acc2], Fs);
%%                 {Acc, T} when is_tuple(T) ->
%%                     {NewAcc1, NewAcc2} = mr(Fun, Acc, tuple_to_list(T)),
%%                     mr(Fun, NewAcc1, [list_to_tuple(NewAcc2)| Acc2], Fs);
%%                 {Acc, F1} ->
%%                     mr(Fun, Acc, [F1| Acc2], Fs)
%%             end;
%%         false ->
%%             mr(Fun, Acc1, [F| Acc2], Fs)
%%     end.
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


%%-------------------------------------------------------------------------
%% @doc
%% Filter out all forms not meeting the provided predicate.
%% @end
%%-------------------------------------------------------------------------
-spec filter(predicate(), forms()) -> forms().
filter(Fun, Forms) ->
    lists:reverse(reduce(
                    fun(Form, Acc) ->
                            case Fun(Form) of
                                true ->
                                    [Form| Acc];
                                false ->
                                    Acc
                            end
                    end,
                    [],
                    Forms)).

%%-------------------------------------------------------------------------
%% @doc
%% Check if there is any form meeting the provided predicate.
%% @end
%%-------------------------------------------------------------------------
-spec any(predicate(), forms()) -> boolean().
any(_Pred, []) ->
    false;
any(Pred, [F| Fs]) when is_tuple(F) ->
    Any = case is_form(F) of
              true ->
                  Pred(F);
              false ->
                  false
          end,
    case Any of
        true ->
            true;
        false ->
            case any(Pred, tuple_to_list(F)) of
                true ->
                    true;
                false ->
                    any(Pred, Fs)
            end
    end;
any(Pred, [F| Fs]) when is_list(F) ->
    case any(Pred, F) of
        true ->
            true;
        false ->
            any(Pred, Fs)
    end;
any(Pred, [_F| Fs]) ->
    any(Pred, Fs).


%%-------------------------------------------------------------------------
%% @doc
%% Check if all forms meet the provided predicate.
%% @end
%%-------------------------------------------------------------------------
-spec all(predicate(), forms()) -> boolean().
all(_Pred, []) ->
    false;
all(Pred, [F| Fs]) when is_tuple(F) ->
    All = case is_form(F) of
              true ->
                  Pred(F);
              false ->
                  true
          end,
    case All of
        false ->
            false;
        true ->
            case all(Pred, tuple_to_list(F)) of
                false ->
                    false;
                true ->
                    all(Pred, Fs)
            end
    end;
all(Pred, [F| Fs]) when is_list(F) ->
    case all(Pred, F) of
        false ->
            false;
        true ->
            all(Pred, Fs)
    end;
all(Pred, [_F| Fs]) ->
    all(Pred, Fs).


%%-------------------------------------------------------------------------
%% @doc
%% Quote a form so that it can, for instance, be bound to a variable
%% when manipulating Erlang's abstract code.
%%
%% The following abstract code is not valid code
%%
%%     {match, 1, {var, 1, 'A'},
%%                {function, 1, foo, 0,
%%                 [
%%                  {clause, 0, [], [], [{atom, 1, foo}]}
%%                 ]}}
%%
%% because, in Erlang code, it would be equivalent to
%%
%%     A = foo() -> foo.
%%
%% which is, obviously, no valid Erlang code. However, one could quote
%% the right-hand side of the above match operation so that it becomes
%% a valid Erlang expression.
%%
%% One could consider that an expression similar to the one below
%%
%%     {match, 1, {var, 1, 'A'},
%%                forms:quote(
%%                    {function, 1, foo, 0,
%%                     [
%%                      {clause, 0, [], [], [{atom, 1, foo}]}
%%                     ]})}
%%
%% becomes something like
%%
%%     A = <<...>>.
%% @end
%%-------------------------------------------------------------------------
quote(Term) ->
    {bin, 0,
     [ {bin_element, 0, {integer, 0, X}, default, default}
       || <<X>> <= term_to_binary(Term) ]}.

%%-------------------------------------------------------------------------
%% @doc
%% Inverse of the quote/1 function. Takes a quoted form and returns its
%% original form.
%% @end
%%-------------------------------------------------------------------------
unquote({bin, _, BinElements}) ->
    binary_to_term(
      lists:foldl(fun({bin_element, _,
                       {integer, _, X},
                       default,
                       default},
                      Acc) ->
                          <<Acc/binary, X>> end,
                  <<>>,
                  BinElements));
unquote(Binary) when is_binary(Binary) ->
    binary_to_term(Binary).


%% ========================================================================
%%  Debug functions
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Evaluate the provided String expression or abstract form.
%% @end
%%-------------------------------------------------------------------------
-spec eval(string() | form()) -> term().
eval(Expr) when is_list(Expr) ->
    {ok, A, _} = erl_scan:string(Expr),
    {ok, B} = erl_parse:parse_exprs(A),
    {value, Value, _} = erl_eval:exprs(B, []),
    Value;
eval(Form) ->
    eval(lists:append(from_abstract(Form), ".")).

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

%%-------------------------------------------------------------------------
%% @doc
%% Convert a cons (abstract representation of a list) into a list
%% @end
%%-------------------------------------------------------------------------
-spec cons_to_list(form()) -> list().
cons_to_list(Cons) ->
    lists:reverse('_cons_to_list'(Cons, [])).

'_cons_to_list'({nil, _}, Acc) ->
    Acc;
'_cons_to_list'({cons, _, H, Cons}, Acc) ->
    '_cons_to_list'(Cons, [H| Acc]).

%%-------------------------------------------------------------------------
%% @doc
%% Convert a list into a cons (abstract representation of a list)
%% @end
%%-------------------------------------------------------------------------
-spec list_to_cons(list()) -> form().
list_to_cons([]) ->
    {nil, 0};
list_to_cons([H| Tail]) ->
    {cons, 0, H, list_to_cons(Tail)}.

%% ========================================================================
%%  Local functions
%% ========================================================================

parse_opts(Opts) ->
    [ Opt || Opt <- Opts, lists:member(Opt, ?OPTS) ].

forms_only(Opts) ->
    proplists:get_value(forms_only, Opts, false).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the provided abstract form is valid.
%% @end
%%-------------------------------------------------------------------------
-spec is_form(any()) -> boolean().
is_form(Form) ->
    case catch from_abstract(Form) of
        {'EXIT', _} ->
            false;
        _ ->
            true
    end.
