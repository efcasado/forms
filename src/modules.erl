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
-module(modules).

%% API
-export(
   [
    module_name/1,
    has_export_attr/1,
    add_function/3, add_function/4,
    rm_function/4, rm_function/5,
    rm_spec/3, rm_spec/4,
    export_function/3,
    unexport_function/3, unexport_function/4,
    function/3,
    calling_functions/3,
    type/3,
    is_type_exported/2
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
    throw(invalid_module);
module_name([{attribute, _, module, ModuleName}| _]) ->
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
%% the options one can set: force and permanent.
%% When 'force' is used, this function will modify modules even if they are
%% in a sticky directory.
%%     force  - Modifies the module, even if it is located in a sticky
%%              directory
%%     permanent - The change will persist even after a node restart
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
%% Remove the specified function for the provided module.
%%
%% If the third argument is set to true, all functions calling the
%% specified function will also be removed.
%%
%% When specifying a module name instead of a modules' forms, these are
%% the options one can set: force and binary.
%% When 'force' is used, this function will modify modules even if they are
%% in a sticky directory.
%%     force  - Modifies the module, even if it is located in a sticky
%%              directory
%%     permanent - The change will persist even after a node restart
%% @end
%%-------------------------------------------------------------------------
-spec rm_function(atom(), integer(), boolean(), module(), list())
                 -> forms:forms().
rm_function(F, A, RmAll, Mod, Opts) ->
    OldForms = load_forms(Mod),
    NewForms = rm_function(F, A, RmAll, OldForms),
    ok = apply_changes(Mod, NewForms, Opts),
    Mod.

-spec rm_function(atom(), integer(), boolean(), forms:forms()) -> forms:forms().
rm_function(F1, A1, false, Forms) ->
    Forms1 = unexport_function(F1, A1, Forms),
    Forms2 = rm_spec(F1, A1, Forms1),
    lists:foldr(fun({function, _, F2, A2, _}, Acc)
                            when F1 == F2 andalso
                                 A1 == A2 ->
                              Acc;
                         (Other, Acc) ->
                              [Other| Acc]
                      end,
                      [],
                      Forms2);
rm_function(F1, A1, true, Forms) ->
    Forms1 = rm_function(F1, A1, false, Forms),
    CallingFunctions = calling_functions(F1, A1, Forms1),
    lists:foldl(fun({F2, A2}, AccForms) ->
                        rm_function(F2, A2, true, AccForms)
                end,
                Forms1,
                CallingFunctions).

%%-------------------------------------------------------------------------
%% @doc
%% Remove the function specification for the provided function, if any.
%%
%% When specifying a module name instead of a modules' forms, these are
%% the options one can set: force and binary.
%% When 'force' is used, this function will modify modules even if they are
%% in a sticky directory.
%%     force  - Modifies the module, even if it is located in a sticky
%%              directory
%%     permanent - The change will persist even after a node restart
%% @end
%%-------------------------------------------------------------------------
-spec rm_spec(atom(), integer(), module(), list()) -> module().
rm_spec(F, A, Mod, Opts) ->
    OldForms = load_forms(Mod),
    NewForms = rm_spec(F, A, OldForms),
    ok = apply_changes(Mod, NewForms, Opts),
    Mod.

-spec rm_spec(atom(), integer(), forms:forms()) -> forms:forms().
rm_spec(F1, A1, Forms) ->
    lists:foldr(fun({attribute, _, spec, {{F2, A2}, _}}, Acc)
                      when F1 == F2 andalso
                           A1 == A2 ->
                        Acc;
                   (Other, Acc) ->
                        [Other| Acc]
                end,
                [],
                Forms).

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
%%
%% When specifying a module name instead of a modules' forms, these are
%% the options one can set: force and binary.
%% When 'force' is used, this function will modify modules even if they are
%% in a sticky directory.
%%     force  - Modifies the module, even if it is located in a sticky
%%              directory
%%     permanent - The change will persist even after a node restart
%% @end
%%-------------------------------------------------------------------------
-spec unexport_function(atom(), integer(), module(), list()) -> module().
unexport_function(Name, Arity, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = unexport_function(Name, Arity, OldForms),
    ok = apply_changes(Mod, NewForms, Opts),
    Mod.

-spec unexport_function(atom(), integer(), forms:forms()) -> mod().
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

%%-------------------------------------------------------------------------
%% @doc
%% Get a list of all the functions with static calls to the provided
%% function.
%% @end
%%-------------------------------------------------------------------------
-spec calling_functions(atom(), integer(), mod()) -> list().
calling_functions(Name, Arity, Mod)
  when is_atom(Mod) ->
    calling_functions(Name, Arity, load_forms(Mod));
calling_functions(F, A, Forms) ->
    CallingFunctions1 =
        lists:foldl(
          fun({function, _, Name2, Arity2, _} = Function, Acc) ->
                  case '_is_calling_function'(F, A, Function) of
                      true ->
                          [{Name2, Arity2}| Acc];
                      false ->
                          Acc
                  end
          end,
          [],
          '_other_functions'(Forms, [{F, A}])),
    M = module_name(Forms),
    CallingFunctions2 =
        lists:foldl(
          fun({function, _, Name2, Arity2, _} = Function, Acc) ->
                  case '_is_calling_function'(M, F, A, Function) of
                      true ->
                          [{Name2, Arity2}| Acc];
                      false ->
                          Acc
                  end
          end,
          [],
          '_other_functions'(Forms, [{F, A}| CallingFunctions1])),
    lists:append([CallingFunctions1, CallingFunctions2]).

'_is_calling_function'(M1, F1, A1, Function) ->
    forms:any(fun({call, _, {remote, _, {atom, _, M2}, {atom, _, F2}}, Args})
                    when M1 == M2 andalso
                         F1 == F2 andalso
                         A1 == length(Args) ->
                      true;
                 (_) -> false
              end,
              [Function]).

'_is_calling_function'(Name1, Arity1, Function) ->
    forms:any(fun({call, _, {atom, _, Name2}, Args})
                    when Name1 == Name2 andalso
                         length(Args) == Arity1 ->
                      true;
                 (_) -> false
              end,
              [Function]).

'_other_functions'(Forms, Ignore) ->
    lists:filter(
      fun({function, _, Name, Arity, _}) ->
              not lists:member({Name, Arity}, Ignore);
         (_) ->
              false
      end,
      Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the specified type.
%% @end
%%-------------------------------------------------------------------------
-spec type(atom(), arity(), mod()) -> {forms:form(), list(atom())}.
type(Name, Arity, Module)
  when is_atom(Module) ->
    type(Name, Arity, load_forms(Module));
type(Name, Arity, []) ->
    throw({type_not_found, {Name, Arity}});
type(Name, Arity, [{attribute, _, type, {Name, _, Args}} = Type| _Forms])
  when length(Args) == Arity ->
    {Type, dependant_types(Type)};
type(Name, Arity, [_| Forms]) ->
    type(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the provided type is exported in the given module.
%% @end
%%-------------------------------------------------------------------------
-spec is_type_exported({atom(), integer()}, mod()) -> boolean().
is_type_exported(Type, Module)
  when is_atom(Module) ->
    is_type_exported(Type, load_forms(Module));
is_type_exported(_Type, []) ->
    false;
is_type_exported(Type, [{attribute, _, export_type, ExpTypes}| Forms]) ->
    case lists:member(Type, ExpTypes) of
        true ->
            true;
        false ->
            is_type_exported(Type, Forms)
    end;
is_type_exported(Type, [_| Forms]) ->
    is_type_exported(Type, Forms).


%% ========================================================================
%%  Local functions
%% ========================================================================

%%-------------------------------------------------------------------------
%% @doc
%% Check if the provided type is a built-in type, or not.
%% @end
%%-------------------------------------------------------------------------
-spec is_builtin_type(atom()) -> boolean().
is_builtin_type(any) ->
    true;
is_builtin_type(none) ->
    true;
is_builtin_type(pid) ->
    true;
is_builtin_type(port) ->
    true;
is_builtin_type(reference) ->
    true;
is_builtin_type(nil) ->
    true;
is_builtin_type(atom) ->
    true;
is_builtin_type(binary) ->
    true;
is_builtin_type('fun') ->
    true;
is_builtin_type(integer) ->
    true;
is_builtin_type(range) ->
    true;
is_builtin_type(list) ->
    true;
is_builtin_type(maybe_improper_list) ->
    true;
is_builtin_type(nonempty_improper_list) ->
    true;
is_builtin_type(nonempty_list) ->
    true;
is_builtin_type(map) ->
    true;
is_builtin_type(tuple) ->
    true;
is_builtin_type(union) ->
    true;
is_builtin_type(term) ->
    true;
is_builtin_type(bitstring) ->
    true;
is_builtin_type(boolean) ->
    true;
is_builtin_type(byte) ->
    true;
is_builtin_type(char) ->
    true;
is_builtin_type(number) ->
    true;
is_builtin_type(string) ->
    true;
is_builtin_type(nonempty_string) ->
    true;
is_builtin_type(iodata) ->
    true;
is_builtin_type(function) ->
    true;
is_builtin_type(module) ->
    true;
is_builtin_type(mfa) ->
    true;
is_builtin_type(arity) ->
    true;
is_builtin_type(node) ->
    true;
is_builtin_type(timeout) ->
    true;
is_builtin_type(no_return) ->
    true;
is_builtin_type(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% Return a list of dependant types for a given form.
%%
%% Built-in and remote types are left out.
%% @end
%%-------------------------------------------------------------------------
-spec dependant_types(forms:form()) -> list(atom()).
dependant_types(Form) ->
    lists:usort(
      forms:reduce(fun({type, _, union, _}, Acc) ->
                           Acc;
                      ({type, _, Name, Args}, Acc) ->
                           case is_builtin_type(Name) of
                               false ->
                                   [{Name, length(Args)}| Acc];
                               true ->
                                   Acc
                           end;
                      (_, Acc) ->
                           Acc
                   end,
                   [],
                   [Form])).

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
    Sticky andalso not forced(Opts) andalso throw({sticky_dir, Dir}),

    Bin = compile_module(Module, Forms),

    Sticky andalso code:unstick_dir(Dir),
    code:purge(Module),
    case permanent(Opts) of
        false ->
            code:load_binary(Module, File, Bin);
        true ->
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

permanent(Opts) ->
    proplists:get_value(permanent, Opts, false).

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
