%%%========================================================================
%%% forms_tests.erl
%%%
%%% Unit tests for the forms module.
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
-module(forms_test).

-include_lib("eunit/include/eunit.hrl").


%% ========================================================================
%%  Tests
%% ========================================================================

read_from_source_test() ->
    SourceFile = "test/dummy_module.erl",
    Forms = forms:read(SourceFile),
    {attribute, 1, file, {SourceFile, 1}} = hd(Forms).

read_from_source_error_test() ->
    {file_not_found, "i_do_not_exist.erl"} =
        (catch forms:read("i_do_not_exist.erl")).

read_from_binary_test() ->
    Forms = forms:read(dummy_module),
    {attribute, _Line, file, {_SrcFile, 1}} = hd(Forms).

read_from_binary_error1_test() ->
    {module_not_found, i_do_not_exist} =
        (catch forms:read(i_do_not_exist)).

read_from_binary_error2_test() ->
    %% Ensure no_debug_info module is compiled without the +debug_info
    %% option
    Dir = filename:dirname(code:which(no_debug_info)),
    {ok, no_debug_info} = compile:file(filename:join(Dir, "no_debug_info"),
                                       [{outdir,Dir}]),
    {forms_not_found, no_debug_info} =
        (catch forms:read(no_debug_info)).

identity_transform1_test() ->
    Forms = forms:read(dummy_module2),
    Forms = forms:map(fun(Form) -> Form end, Forms).

identity_transform2_test() ->
    Forms = forms:read(dummy_module2),
    Forms = forms:map(fun(Form) -> Form end, Forms, [forms_only]).

reduce_test() ->
    Forms = forms:read(dummy_module2),
    7 = forms:reduce(
          fun({var, _Line, '_'}, Acc) ->
                  Acc + 1;
             (_Form, Acc) ->
                  Acc
          end,
          0,
          Forms).

mr_test() ->
    Forms = forms:read(dummy_module2),
    {7, Forms} = forms:mr(
                    fun({var, _Line, '_'} = Form, Acc) ->
                            {Acc + 1, Form};
                       (Form, Acc) ->
                            {Acc, Form}
                    end,
                    0,
                    Forms).

reduce_next_test() ->
    Forms = forms:read(dummy_module2),
    Forms = forms:map(
              fun(Form) ->
                      {next, Form}
              end,
              Forms).

mr_next_test() ->
    Forms = forms:read(dummy_module2),
    {_Count, Forms} = forms:mr(
              fun(Form, Acc) ->
                      {Acc + 1, {next, Form}}
              end,
              0,
              Forms).

map_next_test() ->
    Forms = forms:read(dummy_module2),
    Forms = forms:map(
              fun({call, _L, _MF, _A} = Form) ->
                      {next, Form};
                 (Form) ->
                      Form
              end,
              Forms).

any_true_test() ->
    Forms = forms:read(dummy_module2),
    true = forms:any(fun({function, _, foo, 0, _}) -> true;
                        (_) -> false end,
                     Forms).

any_false_test() ->
    Forms = forms:read(dummy_module2),
    false = forms:any(fun({function, _, doesnotexist, 2, _}) -> true;
                         (_) -> false end,
                      Forms).


%% ========================================================================
%%  Auxiliary functions
%% ========================================================================

erl_from_beam(Module) ->
    BeamFile = code:which(Module),
    filename:dirname(BeamFile) ++ atom_to_list(Module) ++ ".erl".
