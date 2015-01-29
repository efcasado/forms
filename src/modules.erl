%%%========================================================================
%%% File: modules.erl
%%%
%%% Built on top of the functionality offered by the 'forms' module, this
%%% module features a collection of functions to manipulate Erlang modules.
%%%
%%% Author: Enrique Fernandez <enrique.fernandez@gmail.com>
%%% Date:   January, 2015
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
-module(modules).

%% API
-export(
   [
    has_export_attr/1,
    add_function/3,
    export_function/3,
    unexport_function/3,
    function/3
   ]).


%% ========================================================================
%%  API
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Check if the given module has any '-export([])' attribute.
%% @end
%%-------------------------------------------------------------------------
-spec has_export_attr(forms:forms()) -> boolean().
has_export_attr(Module) ->
    lists:any(fun({attribute,_,export,_}) ->  true;
                 (_) -> false end,
              Module).

%%-------------------------------------------------------------------------
%% @doc
%% Adds the provided function to the given module. If the exportet flag is
%% set to true, the function will feature in the list of exported
%% functions.
%% @end
%%-------------------------------------------------------------------------
-spec add_function(forms:form(), forms:forms(), boolean()) -> forms:forms().
add_function({function, _, Name, Arity, _} =  Fun, Mod, true = _Exp) ->
    Mod1 = '_add_function'(Fun, Mod),
    export_function(Name, Arity, Mod1);
add_function(Fun, Mod, false = _Exp) ->
    '_add_function'(Fun, Mod).

'_add_function'(Function, Module) ->
    [{eof, _} = EOF| Forms0] = lists:reverse(Module),
    lists:reverse([EOF, Function| Forms0]).

%%-------------------------------------------------------------------------
%% @doc
%% Add the function Name/Arity to the list of exported functions.
%% @end
%%-------------------------------------------------------------------------
-spec export_function(atom(), integer(), forms:forms()) -> forms:forms().
export_function(Name, Arity, Module) ->
    [FileAttr, ModuleAttr| Forms] = Module,
    ExportAttr = {attribute, 0, export, [{Name, Arity}]},
    [FileAttr, ModuleAttr, ExportAttr| Forms].

%%-------------------------------------------------------------------------
%% @doc
%% Remove the function Name/Arity from the list of exported functions.
%% @end
%%-------------------------------------------------------------------------
-spec unexport_function(atom(), integer(), forms:forms()) -> forms:forms().
unexport_function(Name, Arity, Module) ->
    forms:map(fun({attribute, L, export, ExpFuns}) ->
                      ExpFuns1 = lists:delete({Name, Arity}, ExpFuns),
                      {attribute, L, export, ExpFuns1};
                 (Other) ->
                      Other
              end,
              Module).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the requested function from the given module.
%%
%% If the function is not found, it throws a
%% '{function_not_found, {Name, Arity}}' exception.
%% @end
%%-------------------------------------------------------------------------
-spec function(atom(), integer(), forms:forms()) -> forms:form().
function(Name, Arity, []) ->
    throw({function_not_found, {Name, Arity}});
function(Name, Arity, [{function, _, Name, Arity, _} = Fun| _Forms] = _Mod) ->
    Fun;
function(Name, Arity, [_Other| Forms] = _Mod) ->
    function(Name, Arity, Forms).
