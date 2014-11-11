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

forms_from_source_test() ->
    Module = forms,
    CompileInfo = Module:module_info(compile),
    SrcFile = proplists:get_value(source, CompileInfo),
    Forms = forms:read(SrcFile),

    {attribute, 1, file, {SrcFile, 1}} = hd(Forms).

forms_from_binary_test() ->
    Module = forms,
    Forms = forms:read(Module),

    {attribute, 1, file, {"src/forms.erl", 1}} = hd(Forms).

identity_transform_test() ->
    Module = forms,
    Forms = forms:read(Module),

    Forms = forms:map(fun(Form) -> Form end, Forms).

count_vars_test() ->
    Module = forms,
    Forms = forms:read(Module),

    4 = forms:reduce(
          %% Count how many variables named
          %% 'Module' are in this module.
          fun({var, _Line, 'Module'}, Acc) ->
                  Acc + 1;
             (_Form, Acc) ->
                  Acc
          end,
          0,
          Forms).

mr_test() ->
    Module = forms,
    Forms = forms:read(Module),

    {4, Forms} = forms:mr(
                    %% Count how many variables named
                    %% 'Module' are in this module.
                    fun({var, _Line, 'Module'} = Form, Acc) ->
                            {Acc + 1, Form};
                       (Form, Acc) ->
                            {Acc, Form}
                    end,
                    0,
                    Forms).

next1_test() ->
    Module = forms,
    Forms = forms:read(Module),

    Forms = forms:map(
              fun(Form) ->
                      {next, Form}
              end,
              Forms).

next2_test() ->
    Module = forms,
    Forms = forms:read(Module),

    {_Count, Forms} = forms:mr(
              fun(Form, Acc) ->
                      {Acc + 1, {next, Form}}
              end,
              0,
              Forms).

next3_test() ->
    Module = forms,
    Forms = forms:read(Module),

    Forms = forms:map(
              fun({call, _L, _MF, _A} = Form) ->
                      {next, Form};
                 (Form) ->
                      Form
              end,
              Forms).
