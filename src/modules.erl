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
    module_name/1,
    has_export_attr/1,
    add_function/3, add_function/4,
    export_function/3,
    unexport_function/3,
    function/3,
    calling_functions/3
   ]).

%% Type specifications
-type mod() :: module() | forms:forms().


%% ========================================================================
%%  API
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Given the forms of a module, return its name.
%% @end
%%-------------------------------------------------------------------------
-spec module_name(forms:forms()) -> atom().
module_name([]) ->
    throw({invalid_module});
module_name([{attribue, _, module, ModuleName}]) ->
    ModuleName;
module_name([_F| Forms]) ->
    module_name(Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the given module has any '-export([])' attribute.
%% @end
%%-------------------------------------------------------------------------
-spec has_export_attr(mod()) -> boolean().
has_export_attr(Module)
  when is_atom(Module) ->
    has_export_attr(load_forms(Module));
has_export_attr(Module) ->
    lists:any(fun({attribute,_,export,_}) ->  true;
                 (_) -> false end,
              Module).

%%-------------------------------------------------------------------------
%% @doc
%% Adds the provided function to the given module. If the exportet flag is
%% set to true, the function will feature in the list of exported
%% functions.
%%
%% When specifying a module name instead of a modules' forms, these are
%% the options one can set: force and binary.
%% When 'force' is used, this function will modify modules even if they are
%% in a sticky directory.
%%     force  - Modifies the module, even if it is located in a sticky
%%              dire
%%     binary -
%% @end
%%-------------------------------------------------------------------------
-spec add_function(forms:form(), boolean(), module(), list()) -> mod().
add_function(Function, Exp, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = add_function(Function, Exp, OldForms),
    ok = apply_changes(Mod, NewForms, Opts),
    Mod.

-spec add_function(forms:form(), boolean(), forms:forms()) -> forms:forms().
add_function({function, _, Name, Arity, _} =  Fun, true = _Exp, Mod) ->
    Mod1 = '_add_function'(Fun, Mod),
    export_function(Name, Arity, Mod1);
add_function(Fun, false = _Exp, Mod) ->
    '_add_function'(Fun, Mod).

'_add_function'(Function, Module) ->
    [{eof, _} = EOF| Forms0] = lists:reverse(Module),
    lists:reverse([EOF, Function| Forms0]).

%%-------------------------------------------------------------------------
%% @doc
%% Add the function Name/Arity to the list of exported functions.
%% @end
%%-------------------------------------------------------------------------
-spec export_function(atom(), integer(), mod()) -> mod().
export_function(Name, Arity, Module)
  when is_atom(Module) ->
    export_function(Name, Arity, load_forms(Module));
export_function(Name, Arity, Module) ->
    [FileAttr, ModuleAttr| Forms] = Module,
    ExportAttr = {attribute, 0, export, [{Name, Arity}]},
    [FileAttr, ModuleAttr, ExportAttr| Forms].

%%-------------------------------------------------------------------------
%% @doc
%% Remove the function Name/Arity from the list of exported functions.
%% @end
%%-------------------------------------------------------------------------
-spec unexport_function(atom(), integer(), mod()) -> mod().
unexport_function(Name, Arity, Module)
  when is_atom(Module) ->
    unexport_function(Name, Arity, load_forms(Module));
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
-spec function(atom(), integer(), mod()) -> forms:form().
function(Name, Arity, Mod)
  when is_atom(Mod) ->
    function(Name, Arity, load_forms(Mod));
function(Name, Arity, []) ->
    throw({function_not_found, {Name, Arity}});
function(Name, Arity, [{function, _, Name, Arity, _} = Fun| _Forms] = _Mod) ->
    Fun;
function(Name, Arity, [_Other| Forms] = _Mod) ->
    function(Name, Arity, Forms).


%% TODO: Find also remote calls within the same module. E.g., calls
%% to lists:member/2 within the lists module.
-spec calling_functions(atom(), integer(), mod()) -> list().
calling_functions(Name, Arity, Mod)
  when is_atom(Mod) ->
    calling_functions(Name, Arity, load_forms(Mod));
calling_functions(Name, Arity, Forms) ->
    OtherFunctions = '_other_functions'(Name, Arity, Forms),
    lists:foldl(
      fun({function, _, Name2, Arity2, _} = Function, Acc) ->
              case '_is_calling_function'(Name, Arity, Function) of
                  true ->
                      [{Name2, Arity2}| Acc];
                  false ->
                      Acc
              end
      end,
      [],
      OtherFunctions).

'_is_calling_function'(Name1, Arity1, Function) ->
    forms:any(fun({call, _, {atom, _, Name2}, Args})
                    when Name1 == Name2 andalso
                         length(Args) == Arity1 ->
                      true;
                 (_) -> false
              end,
              [Function]).

'_other_functions'(Name1, Arity1, Forms) ->
    lists:filter(
      fun({function, _, Name2, Arity2, _})
            when Name1 /= Name2 orelse Arity1 /= Arity2 ->
              true;
         (_) ->
              false
      end,
      Forms).
%% ========================================================================
%% Local functions
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Load the forms of the specified module.
%% @end
%%-------------------------------------------------------------------------
-spec load_forms(module()) -> forms:forms().
load_forms(Module) ->
    try
        forms:read(Module)
    catch
        _:_ ->
            throw({cannot_load_forms, Module})
    end.

%%-------------------------------------------------------------------------
%% @doc
%% Replace the specified modules implementation by the binary resulting
%% of compiling the provided forms.
%% @end
%%-------------------------------------------------------------------------
-spec apply_changes(module(), forms:forms(), list()) -> 'ok'.
apply_changes(Module, Forms, Opts) ->
    File = module_file(Module),
    Dir = filename:dirname(File),

    Sticky = code:is_sticky(Module),
    Sticky andalso forced(Opts) andalso throw({sticky_dir, Dir}),

    Bin = compile_module(Module, Forms),

    Sticky andalso code:unstick_dir(Dir),
    code:purge(Module),
    case load_as_binary(Opts) of
        true ->
            code:load_binary(Module, File, Bin);
        false ->
            case file:write_file(File, Bin) of
                ok ->
                    code:load_file(File);
                Error ->
                    Sticky andalso code:stick_dir(Dir),
                    throw(Error)
            end
    end,
    Sticky andalso code:stick_dir(Dir),
    ok.

forced(Opts) ->
    proplists:get_value(force, Opts, false).

load_as_binary(Opts) ->
    proplists:get_value(binary, Opts, false).

-spec module_file(module()) -> string().
module_file(Module) ->
    case code:is_loaded(Module) of
        false ->
            {module, Module} = code:load_file(Module),
            {file, File} = code:is_loaded(Module),
            File;
        {file, File} ->
            File
    end.

-spec compile_module(module(), forms:forms()) -> binary().
compile_module(Module, Forms) ->
    %% TODO: We may want to preserve some options from the original forms.
    Opts = [debug_info],
    case compile:forms(Forms, Opts) of
        {ok, Module, Binary} ->
            Binary;
        _ ->
            throw({compile_error, Module})
    end.
