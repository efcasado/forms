%%%========================================================================
%%% File: meta.erl
%%%
%%% Built on top of the functionality offered by the 'forms' module, this
%%% module features a collection of functions to manipulate Erlang modules.
%%%
%%% Most of the functions implemented in this module will throw an
%%% exception in case of not succeeding. For instance, the `function/3`
%%% function will throw a `{function_not_found, {Name, Arity}}`
%%% exception in case it cannot find the requested function. Similar
%%% execeptions are thrown by functions such as `function_spec/3`,
%%% `type/3`, `record/2`, etc.
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
-module(meta).

%% API
-export(
   [
    module_name/1,
    has_attr/2,
    has_function/3,
    has_record/2,
    has_type/3,
    add_function/3, add_function/4,
    rm_function/4, rm_function/5,
    rm_spec/3, rm_spec/4,
    is_function_exported/3,
    export_function/3,
    unexport_function/3, unexport_function/4,
    function/3,
    function_spec/3,
    calling_functions/3,
    type/3,
    is_type_exported/2,
    record/2,
    apply_changes/2, apply_changes/3
   ]).

%% Type specifications
-type mod() :: module() | forms:forms().
-type abs_type()     :: forms:form().
-type abs_spec()     :: forms:form().
-type abs_record()   :: forms:form().
-type abs_function() :: forms:form().
-type type()         :: atom() | {'record', atom()}.

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
%% Check if the given module has the specified attribute.
%% @end
%%-------------------------------------------------------------------------
-spec has_attr(atom(), mod()) -> boolean().
has_attr(Name, Module)
  when is_atom(Module) ->
    has_attr(Name, load_forms(Module));
has_attr(Name, Module) ->
    lists:any(fun({attribute,_,AttrName,_}) when AttrName == Name->  true;
                 (_) -> false end,
              Module).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the specified function exists in the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec has_function(atom(), arity(), mod()) -> boolean().
has_function(Name, Arity, Module)
  when is_atom(Module) ->
    has_function(Name, Arity, load_forms(Module));
has_function(_Name, _Arity, []) ->
    false;
has_function(Name, Arity, [{function, _, Name, Arity, _}| _]) ->
    true;
has_function(Name, Arity, [_| Forms]) ->
    has_function(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the specified record exists in the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec has_record(atom(), mod()) -> boolean().
has_record(Name, Module)
  when is_atom(Module) ->
    has_record(Name, load_forms(Module));
has_record(_Name, []) ->
    false;
has_record(Name, [{attribute, _, record, {Name, _}}| _]) ->
    true;
has_record(Name, [_| Forms]) ->
    has_record(Name, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the specified type exists in the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec has_type(type(), arity(), mod()) -> boolean().
has_type(Name, Arity, Module)
  when is_atom(Module) ->
    has_type(Name, Arity, load_forms(Module));
has_type(_Name, _Arity, []) ->
    false;
has_type(Name, Arity, [{attribute, _, type, {Name, _, Args}}| _])
  when length(Args) ==  Arity ->
    true;
has_type(Name, Arity, [_| Forms]) ->
    has_type(Name, Arity, Forms).

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
-spec add_function(abs_function(), boolean(), module(), list()) -> mod().
add_function(Function, Exp, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = add_function(Function, Exp, OldForms),
    apply_changes(Mod, NewForms, Opts).

-spec add_function(abs_function(), boolean(), forms:forms()) -> forms:forms().
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
rm_function(F, A, RmAll, Module, Opts)
  when is_atom(Module) ->
    OldForms = load_forms(Module),
    NewForms = rm_function(F, A, RmAll, OldForms),
    apply_changes(Module, NewForms, Opts).

-spec rm_function(atom(), integer(), boolean(), mod()) -> forms:forms().
rm_function(F, A, RmAll, Module)
  when is_atom(Module) ->
    rm_function(F, A, RmAll, Module, []);
rm_function(F, A, RmAll, Forms) ->
    '_rm_function'(F, A, RmAll, Forms).

'_rm_function'(F1, A1, false, Forms) ->
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
'_rm_function'(F1, A1, true, Forms) ->
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
    apply_changes(Mod, NewForms, Opts).

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
%% Check if the provided function is exported by the given module.
%% @end
%%-------------------------------------------------------------------------
-spec is_function_exported(atom(), arity(), mod()) -> boolean().
is_function_exported(Name, Arity, Module)
  when is_atom(Module) ->
    is_function_exported(Name, Arity, load_forms(Module));
is_function_exported(_Name, _Arity, []) ->
    false;
is_function_exported(Name, Arity, [{attribute, _, export, ExpFuns}| Forms]) ->
    case lists:member({Name, Arity}, ExpFuns) of
        true ->
            true;
        false ->
            is_function_exported(Name, Arity, Forms)
    end;
is_function_exported(Name, Arity, [_| Forms]) ->
    is_function_exported(Name, Arity, Forms).

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
    apply_changes(Mod, NewForms, Opts).

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
-spec function(atom(), integer(), mod())
              -> {abs_function(), list()} |
                 {abs_function(), abs_spec(), list(), list()}.
function(Name, Arity, Mod)
  when is_atom(Mod) ->
    function(Name, Arity, load_forms(Mod));
function(Name, Arity, Forms) ->
    {Fun, Records} = '_function'(Name, Arity, Forms),
    case catch function_spec(Name, Arity, Forms) of
        {spec_not_found, {Name, Arity}} ->
            {Fun, Records};
        {Spec, Types} ->
            {Fun, Spec, Records, Types}
    end.

'_function'(Name, Arity, []) ->
    throw({function_not_found, {Name, Arity}});
'_function'(Name, Arity, [{function, _, Name, Arity, _} = Fun| _Forms]) ->
    {Fun, dependant_records(Fun)};
'_function'(Name, Arity, [_Other| Forms] = _Mod) ->
    '_function'(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the requested function specification from the
%% given module.
%%
%% If the function specification is not found, a
%% {spec_not_found, {Name, Arity}} exception is thrown.
%% @end
%%-------------------------------------------------------------------------
-spec function_spec(atom(), arity(), mod())
                   -> {abs_spec(), list()} | no_return().
function_spec(Name, Arity, Module)
  when is_atom(Module) ->
    function_spec(Name, Arity, load_forms(Module));
function_spec(Name, Arity, []) ->
    throw({spec_not_found, {Name, Arity}});
function_spec(Name, Arity,
              [{attribute, _, spec, {{Name, Arity}, _}} = Spec| _Forms]) ->
    {Spec, dependant_types(Spec)};
function_spec(Name, Arity, [_| Forms]) ->
    function_spec(Name, Arity, Forms).


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
-spec type(atom(), arity(), mod()) -> {abs_type(), list({atom(), arity()})}.
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
%% Check if the provided type is exported by the given module.
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


%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the specified record.
%% @end
%%-------------------------------------------------------------------------
-spec record(atom(), mod()) -> {abs_record(), abs_type(), list(), list()}.
record(Name, Module)
  when is_atom(Module) ->
    record(Name, load_forms(Module));
record(Name, Forms) ->
    {Record, RecordType} = '_record'(Name, Forms, {undefined, undefined}),
    {Record, RecordType,
     dependant_types(RecordType),
     dependant_records(RecordType)}.

%% The record type seems to go after the record specification, always.
'_record'(Name, [], {_Record, _RecordType}) ->
    throw({record_not_found, Name});
'_record'(Name, [{attribute, _, record, {Name, _}} = Record| Forms],
          {_, RecordType}) ->
    '_record'(Name, Forms, {Record, RecordType});
'_record'(Name, [{attribute, _, type, {{record, Name}, _, _}} = RecordType| _],
          {Record, _}) ->
    {Record, RecordType};
'_record'(Name, [_| Forms], Acc) ->
    '_record'(Name, Forms, Acc).


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
is_builtin_type(product) ->
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
-spec dependant_types(forms:form()) -> list({atom(), arity()}).
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
%% Return a list of dependant records for a given form.
%% @end
%%-------------------------------------------------------------------------
-spec dependant_records(forms:form()) -> list(atom()).
dependant_records(Form) ->
    lists:usort(
      forms:reduce(fun({record, _, Name, _}, Acc) -> [Name| Acc];
                      (_Other, Acc) -> Acc end,
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
%%
%% This function accepts two options, which have to do with whether changes
%% to a module's AST should be transient or permanent, and whether the
%% AST of protected modules can be modified, or not.
%% By default, changes are transient (i.e., they do not survive a node
%% restart) and cannot be applied to protected modules. If one wants to
%% change this behaviour, one has to set the 'permanent' and/or 'force'
%% options, respectively.
%%
%% When changes are set to be permanent, this function returns the name of
%% the module the changes have been applied to. When changes are transient,
%% this function returns the AST of the module, so that one can keep on
%% manipulating it.
%% @end
%%-------------------------------------------------------------------------
-spec apply_changes(forms:forms(), list()) -> mod().
apply_changes(Forms, Opts) ->
    apply_changes(module_name(Forms), Forms, Opts).

-spec apply_changes(module(), forms:forms(), list()) -> mod().
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
                    code:load_file(Module);
                Error ->
                    Sticky andalso code:stick_dir(Dir),
                    throw(Error)
            end
    end,

    Sticky andalso code:stick_dir(Dir),

    case permanent(Opts) of
        true ->
            Module;
        false ->
            Forms
    end.

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
