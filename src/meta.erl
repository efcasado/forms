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
%%% execeptions are thrown by functions such as `spec/3`,
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
    has_spec/3,
    behaviours/1,
    callbacks/1,
    add_function/3, add_function/4,
    rm_function/4, rm_function/5,
    rename_function/5, rename_function/6,
    replace_function/4, replace_function/5,
    rm_spec/3, rm_spec/4,
    is_function_exported/3,
    export_function/3,
    unexport_function/3, unexport_function/4,
    functions/1,
    function/3, function/4,
    specs/1,
    spec/3, spec/4,
    calling_functions/3,
    types/1,
    type/3, type/4,
    is_type_exported/2,
    records/1,
    record/2, record/3,
    add_forms/2, add_forms/3,
    apply_changes/1, apply_changes/2, apply_changes/3
   ]).

%% Type specifications
-type meta_module()       :: module() | forms:forms().
-type meta_type()         :: atom() | {'record', atom()}.
-type meta_abs_type()     :: forms:form().
-type meta_abs_spec()     :: forms:form().
-type meta_abs_record()   :: forms:form().
-type meta_abs_function() :: forms:form().
-type meta_opt()          :: 'force' | 'permanent'.
-type meta_opts()         :: list(meta_opt()).


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
-spec has_attr(atom(), meta_module()) -> boolean().
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
-spec has_function(atom(), arity(), meta_module()) -> boolean().
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
-spec has_record(atom(), meta_module()) -> boolean().
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
-spec has_type(meta_type(), arity(), meta_module()) -> boolean().
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
%% Check if the specified function specification exists in the provided
%% module.
%% @end
%%-------------------------------------------------------------------------
-spec has_spec(atom(), arity(), meta_module()) -> boolean().
has_spec(Name, Arity, Module)
  when is_atom(Module) ->
    has_spec(Name, Arity, load_forms(Module));
has_spec(_Name, _Arity, []) ->
    false;
has_spec(Name, Arity, [{attribute, _, spec, {{Name, Arity}, _}}| _]) ->
    true;
has_spec(Name, Arity, [_| Forms]) ->
    has_spec(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Get a list featuring the name of all the behaviours used by the provided
%% module.
%% @end
%%-------------------------------------------------------------------------
-spec behaviours(meta_module()) -> list().
behaviours(Module)
  when is_atom(Module) ->
    behaviours(load_forms(Module));
behaviours(Forms) ->
    lists:zf(fun({attribute, _, behaviour, Behaviour}) ->
                     {true, Behaviour};
                (_) ->
                     false
             end,
             Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Get a list featuring the name and arity of all the callbacks defined
%% in the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec callbacks(meta_module()) -> list().
callbacks(Module)
  when is_atom(Module) ->
    callbacks(load_forms(Module));
callbacks(Forms) ->
    lists:zf(fun({attribute, _, callback, {Callback, _}}) ->
                     {true, Callback};
                (_) ->
                     false
             end,
             Forms).

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
-spec add_function(meta_abs_function(), boolean(), forms:forms())
                  -> meta_module().
add_function(Function, Export, Module) ->
    add_function(Function, Export, Module, _Opts = []).

-spec add_function(meta_abs_function(), boolean(), module(), meta_opts())
                  -> meta_module().
add_function(Function, Exp, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = add_function(Function, Exp, OldForms),
    apply_changes(Mod, NewForms, Opts);
add_function(Function, Export, Forms, _Opts) ->
    '_add_function'(Function, Export, Forms).

'_add_function'({function, _, Name, Arity, _} =  Function, true, Forms) ->
    Forms2 = add_forms([Function], Forms),
    export_function(Name, Arity, Forms2);
'_add_function'(Function, false, Forms) ->
    add_forms([Function], Forms).

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
-spec rm_function(atom(), integer(), boolean(), module(), meta_opts())
                 -> forms:forms().
rm_function(F, A, RmAll, Module, Opts)
  when is_atom(Module) ->
    OldForms = load_forms(Module),
    NewForms = rm_function(F, A, RmAll, OldForms),
    apply_changes(Module, NewForms, Opts).

-spec rm_function(atom(), integer(), boolean(), meta_module()) -> forms:forms().
rm_function(F, A, RmAll, Module)
  when is_atom(Module) ->
    rm_function(F, A, RmAll, Module, []);
rm_function(F, A, RmAll, Forms) ->
    '_rm_function'(F, A, RmAll, Forms).

'_rm_function'(F1, A1, false, Forms) ->
    Forms1 = rm_spec(F1, A1, Forms),
    lists:foldr(fun({function, _, F2, A2, _}, Acc)
                      when F1 == F2 andalso
                           A1 == A2 ->
                        Acc;
                   (Other, Acc) ->
                        [Other| Acc]
                end,
                [],
                Forms1);
'_rm_function'(F1, A1, true, Forms) ->
    Forms1 = unexport_function(F1, A1, Forms),
    Forms2 = rm_function(F1, A1, false, Forms1),
    CallingFunctions = calling_functions(F1, A1, Forms2),
    lists:foldl(fun({F2, A2}, AccForms) ->
                        rm_function(F2, A2, true, AccForms)
                end,
                Forms2,
                CallingFunctions).

%%-------------------------------------------------------------------------
%% @doc
%% Rename the specified function.
%%
%% If set to true, all references to the renamed function (e.g., function
%% specifications, (static) function calls, etc.) will also be updated.
%% @end
%%-------------------------------------------------------------------------
-spec rename_function(atom(), arity(), atom(), boolean(), meta_module(),
                      meta_opts()) -> meta_module().
rename_function(Name, Arity, NewName, RenameAll, Module, Opts)
  when is_atom(Module) ->
    Forms0 = load_forms(Module),
    Forms1 = rename_function(Name, Arity, NewName, RenameAll, Forms0),
    apply_changes(Module, Forms1, Opts).

-spec rename_function(atom(), arity(), atom(), boolean(), meta_module())
                     -> forms:forms().
rename_function(Name, Arity, NewName, RenameAll, Module)
  when is_atom(Module) ->
    Forms0 = load_forms(Module),
    Forms1 = rename_function(Name, Arity, NewName, RenameAll, Forms0),
    apply_changes(Module, Forms1);
rename_function(Name, Arity, NewName, false, Forms) ->
    lists:map(
      fun({function, Line, N, A, Clauses})
         when N == Name andalso
              A == Arity ->
              {function, Line, NewName, Arity, Clauses};
         (Other) ->
              Other
      end,
      Forms);
rename_function(Name, Arity, NewName, true, Forms) ->
    Module = module_name(Forms),
    forms:map(
      fun({attribute, Line, export, ExpFuns0}) ->
              %% Export
              ExpFuns1 = lists:map(fun({N, A}) when N == Name andalso
                                                    A == Arity ->
                                           {NewName, Arity};
                                      (Other) ->
                                           Other
                                   end,
                                   ExpFuns0),
              {attribute, Line, export, ExpFuns1};
         ({function, Line, N, A, Clauses})
            when N == Name andalso
                 A == Arity ->
              %% Function definition
              {function, Line, NewName, Arity, Clauses};
         ({attribute, Line, spec, {{N, A}, Clauses}})
            when N == Name andalso
                 A == Arity ->
              %% Function specification
              {attribute, Line, spec, {{NewName, Arity}, Clauses}};
         ({call, L1, {atom, L2, N}, Args})
            when N == Name andalso
                 length(Args) == Arity ->
              %% Call
              {call, L1, {atom, L2, NewName}, Args};
         ({call, L1, {remote, L2, {atom, L3, M}, {atom, L4, N}}, Args})
            when M == Module andalso
                 N == Name andalso
                 length(Args) == Arity ->
              %% Remote call
              {call, L1,
               {remote, L2, {atom, L3, M}, {atom, L4, NewName}},
               Args};
         (Other) ->
              Other
      end,
      Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Replace the specified function by the provided function.
%%
%% A wrong_arity exception is thrown if the artity of the provided
%% function does not match the specified arity.
%% @end
%%-------------------------------------------------------------------------
-spec replace_function(atom(), integer(), meta_abs_function(), meta_module(),
                       meta_opts()) -> meta_module().
replace_function(Name, Arity, Function, Module, Opts)
  when is_atom(Module) ->
    Forms0 = load_forms(Module),
    Forms1 = replace_function(Name, Arity, Function, Forms0),
    apply_changes(Module, Forms1, Opts).

-spec replace_function(atom(), integer(), meta_abs_function(), meta_module())
                      -> meta_module().
replace_function(Name, Arity, Function, Module)
  when is_atom(Module) ->
    replace_function(Name, Arity, Function, Module, []);
replace_function(_Name, Arity, {function, _, _, A, _}, _Forms)
  when Arity =/= A ->
    throw(wrong_arity);
replace_function(Name, Arity, Function, Forms) ->
    Forms1 = rm_function(Name, Arity, false, Forms),
    add_function(Function, true, Forms1).

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
-spec rm_spec(atom(), integer(), module(), meta_opts()) -> meta_module().
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
-spec is_function_exported(atom(), arity(), meta_module()) -> boolean().
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
-spec export_function(atom(), integer(), meta_module()) -> meta_module().
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
-spec unexport_function(atom(), integer(), module(), meta_opts())
                       -> meta_module().
unexport_function(Name, Arity, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = unexport_function(Name, Arity, OldForms),
    apply_changes(Mod, NewForms, Opts).

-spec unexport_function(atom(), integer(), forms:forms()) -> meta_module().
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
%% Get a list featuring the name and arity of all the functions defined in
%% the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec functions(meta_module()) -> list().
functions(Module)
  when is_atom(Module) ->
    functions(load_forms(Module));
functions(Forms) ->
    lists:zf(fun({function, _, Name, Arity, _}) ->
                     {true, {Name, Arity}};
                (_) ->
                     false
             end,
             Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the requested function from the given module.
%%
%% Note that a function might depend on other forms such as records and
%% type and function specifications.
%%
%% If the function is not found, it throws a
%% '{function_not_found, {Name, Arity}}' exception.
%% @end
%%-------------------------------------------------------------------------
-spec function(atom(), integer(), meta_module())
              -> {meta_abs_function(), 'undefined' | meta_abs_spec(), list()}.
function(Name, Arity, Module) ->
    function(Name, Arity, Module, [direct_only, reference]).

-spec function(atom(), integer(), meta_module(), list())
              -> {meta_abs_function(), 'undefined' | meta_abs_spec(), list()}.
function(Name, Arity, Module, Opts)
  when is_atom(Module) ->
    function(Name, Arity, load_forms(Module), Opts);
function(Name, Arity, Forms, Opts) ->
    Function = '_function'(Name, Arity, Forms),
    Spec = case catch spec(Name, Arity, Forms) of
               {spec_not_found, {Name, Arity}} ->
                   undefined;
               {Form, _} ->
                   Form
           end,
    Deps = handle_dependencies([Function, Spec], Forms, Opts),
    {Function,
     Spec,
     Deps}.

'_function'(Name, Arity, []) ->
    throw({function_not_found, {Name, Arity}});
'_function'(Name, Arity, [{function, _, Name, Arity, _} = Function| _Forms]) ->
    Function;
'_function'(Name, Arity, [_Other| Forms] = _Mod) ->
    '_function'(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Get a list featuring the name and arity of all the function
%% specifications defined in the provided module.
%% @end
%%-------------------------------------------------------------------------
-spec specs(meta_module()) -> list().
specs(Module)
  when is_atom(Module) ->
    specs(load_forms(Module));
specs(Forms) ->
    lists:zf(fun({attribute, _, spec, {{Name, Arity}, _}}) ->
                     {true, {Name, Arity}};
                (_) ->
                     false
             end,
             Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the requested function specification from the
%% given module.
%%
%% If the function specification is not found, a
%% {spec_not_found, {Name, Arity}} exception is thrown.
%% @end
%%-------------------------------------------------------------------------
-spec spec(atom(), arity(), meta_module())
                   -> {meta_abs_spec(), list(), list()} | no_return().
spec(Name, Arity, Module) ->
    spec(Name, Arity, Module, [direct_only, reference]).

-spec spec(atom(), arity(), meta_module(), list())
                   -> {meta_abs_spec(), list(), list()} | no_return().
spec(Name, Arity, Module, Opts)
  when is_atom(Module) ->
    spec(Name, Arity, load_forms(Module), Opts);
spec(Name, Arity, Forms, Opts) ->
    Spec = '_spec'(Name, Arity, Forms),
    Deps = handle_dependencies(Spec, Forms, Opts),
    {Spec, Deps}.


'_spec'(Name, Arity, []) ->
    throw({spec_not_found, {Name, Arity}});
'_spec'(Name, Arity,
              [{attribute, _, spec, {{Name, Arity}, _}} = Spec| _Forms]) ->
    Spec;
'_spec'(Name, Arity, [_| Forms]) ->
    '_spec'(Name, Arity, Forms).


%%-------------------------------------------------------------------------
%% @doc
%% Get a list of all the functions with static calls to the provided
%% function.
%% @end
%%-------------------------------------------------------------------------
-spec calling_functions(atom(), integer(), meta_module()) -> list().
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
%% Get a list featuring the name and arity of all the types defined in the
%% provided module.
%% @end
%%-------------------------------------------------------------------------
-spec types(meta_module()) -> list().
types(Module)
  when is_atom(Module) ->
    types(load_forms(Module));
types(Forms) ->
    lists:zf(fun({attribute, _, type, {{record, _}, _, _}}) ->
                     %% Automatically added by Erlang's preprocessor when
                     %% a "typed record" is found.
                     false;
                ({attribute, _, type, {Name, _, Args}}) ->
                     {true, {Name, length(Args)}};
                (_) ->
                     false
             end,
             Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the specified type.
%% @end
%%-------------------------------------------------------------------------
-spec type(atom(), arity(), meta_module())
          -> {meta_abs_type(), list()}.
type(Name, Arity, Module) ->
    type(Name, Arity, Module, [direct_only, reference]).

-spec type(atom(), arity(), meta_module(), list())
          -> {meta_abs_type(), list()}.
type(Name, Arity, Module, Opts)
  when is_atom(Module) ->
    type(Name, Arity, load_forms(Module), Opts);
type(Name, Arity, Forms, Opts) ->
    Type = '_type'(Name, Arity, Forms),
    Deps = handle_dependencies(Type, Forms, Opts),
    {Type, Deps}.

'_type'(Name, Arity, []) ->
    throw({type_not_found, {Name, Arity}});
'_type'(Name, Arity, [{attribute, _, type, {Name, _, Args}} = Type| _Forms])
  when length(Args) == Arity ->
    Type;
'_type'(Name, Arity, [_| Forms]) ->
    '_type'(Name, Arity, Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Check if the provided type is exported by the given module.
%% @end
%%-------------------------------------------------------------------------
-spec is_type_exported({atom(), integer()}, meta_module()) -> boolean().
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
%% Get a list featuring the name of all the records defined in the
%% provided module.
%% @end
%%-------------------------------------------------------------------------
-spec records(meta_module()) -> list().
records(Module)
  when is_atom(Module) ->
    records(load_forms(Module));
records(Forms) ->
    lists:zf(fun({attribute, _, record, {Name, _}}) -> {true, Name};
                (_) -> false end,
             Forms).

%%-------------------------------------------------------------------------
%% @doc
%% Fetch the abstract code of the specified record.
%% @end
%%-------------------------------------------------------------------------
-spec record(atom(), meta_module())
            -> {meta_abs_record(), meta_abs_type(), list(), list()}.
record(Name, Module) ->
    record(Name, Module, [direct_only, reference]).

record(Name, Module, Opts)
  when is_atom(Module) ->
    record(Name, load_forms(Module), Opts);
record(Name, Forms, Opts) ->
    {Record, RecordType} = '_record'(Name, Forms, {undefined, undefined}),
    Deps = handle_dependencies([Record, RecordType], Forms, Opts),
    {Record, RecordType, Deps}.

%% The record type seems to go after the record specification, always.
'_record'(Name, [], {undefined, undefined}) ->
    throw({record_not_found, Name});
'_record'(_Name, [], Record) ->
    Record;
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
      forms:reduce(
        fun({type, _, union, _}, Acc) ->
                Acc;
           ({type, _, record, [{atom, _, _}]}, Acc) ->
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
      forms:reduce(fun({record, _, Name, _}, Acc) ->
                           [Name| Acc];
                      ({type, _, record, [{atom, _, Record}]}, Acc) ->
                           [Record| Acc];
                      (_Other, Acc) ->
                           Acc
                   end,
                   [],
                   [Form])).

handle_dependencies(Forms, ModuleForms, Opts)
  when is_list(Forms) ->
    DirectDeps =
        lists:usort(
          lists:foldl(fun(Form, Acc) ->
                              lists:append([
                                            Acc,
                                            dependant_types(Form),
                                            dependant_records(Form)
                                           ])
                      end,
                      [],
                      Forms)
         ),
    '_handle_dependencies'(DirectDeps, ModuleForms, Opts);
handle_dependencies(Form, ModuleForms, Opts) ->
    DirectDeps = dependant_types(Form) ++ dependant_records(Form),
    '_handle_dependencies'(DirectDeps, ModuleForms, Opts).

'_handle_dependencies'(Deps0, Forms, Opts) ->
    Deps1 = case direct_only(Opts) of
                true ->
                    Deps0;
                false ->
                    AllDeps0 = lists:usort(
                          lists:flatten([ dependencies(D, Forms)
                                          || D <- Deps0 ])),
                    %% Filter out all types featuring as dependencies,
                    %% since we are not linearising them
                    AllDeps1 = lists:foldl(fun({I, Ds0}, Acc) ->
                                                   Ds1 = lists:filter(
                                                             fun({_,_}) -> false;
                                                                (_) -> true
                                                             end,
                                                             Ds0),
                                                   [{I, Ds1}| Acc]
                                           end,
                                           [],
                                           AllDeps0),

                    {AllTs, AllRs} =
                        lists:foldl(fun({{_, _} = T, _}, {Ts, Rs}) ->
                                            %% A type's dependencies can be
                                            %% ignored, since we are not
                                            %% linearising them.
                                            {[T| Ts], Rs};
                                       (R, {Ts, Rs}) -> {Ts, [R| Rs]} end,
                                    {[], []},
                                    AllDeps1),
                    %% All type definitions are placed after all record
                    %% definitions in order to prevent the Erlang compiler
                    %% from complaining about records not being defined.
                    AllTs ++ sort(AllRs)
            end,
    case reference(Opts) of
        true ->
            Deps1;
        false ->
            lists:flatmap(fun({Name, Arity}) ->
                                  {T, _} = type(Name, Arity, Forms),
                                  [T];
                             (Record) ->
                                  {R, RT, _} = record(Record, Forms),
                                  [R, RT]
                          end,
                          Deps1)
    end.

sort(List) ->
    %% Cycles cause infinite loops. Note, however, that we do
    %% not have to worry about cycles, since we are only sorting
    %% "records" and Erlang does not allow cycles in record
    %% definitions.
    List1 = [K || {K, _V} <- List],
    '_sort'(List1, [], List).

'_sort'([], SortedList, _Deps) ->
    SortedList;
'_sort'([H| Tail], Acc, Deps) ->
    NewAcc0 = [H| lists:delete(H, Acc)],
    NewAcc = lists:foldl(fun(X, NewAcc1) ->
                                 '_sort'([X], NewAcc1, Deps)
                         end,
                         NewAcc0,
                         proplists:get_value(H, Deps, [])),
    '_sort'(Tail, NewAcc, Deps).

direct_only(Opts) ->
    proplists:get_value(direct_only, Opts, false).

reference(Opts) ->
    proplists:get_value(reference, Opts, false).

dependencies(FormRef, Forms) ->
    lists:usort('_dependencies'(FormRef, [], Forms)).

'_dependencies'({Name, Arity} = Type, Acc, Forms) ->
    {_, Deps} = type(Name, Arity, Forms, [direct_only, reference]),
    NewAcc1 = lists:foldl(fun(FormRef, NewAcc0) ->
                                  dependencies(FormRef, Forms) ++ NewAcc0
                          end,
                          Acc,
                          Deps),
    [{Type, Deps}| NewAcc1];
'_dependencies'(Record, Acc, Forms)
  when is_atom(Record) ->
    {_, _, Deps} = record(Record, Forms, [direct_only, reference]),
    NewAcc1 = lists:foldl(fun(FormRef, NewAcc0) ->
                                  dependencies(FormRef, Forms) ++ NewAcc0
                          end,
                          Acc,
                          Deps),
    [{Record, Deps}| NewAcc1].

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
%% Add the provided forms at the end of the specified module.
%% @end
%%-------------------------------------------------------------------------
add_forms(Forms, Module) ->
    add_forms(Forms, Module, _Opts = []).

add_forms(Forms, Module, Opts)
  when is_atom(Module) ->
    Forms1 = add_forms(Forms, load_forms(Module), Opts),
    apply_changes(Module, Forms1, Opts);
add_forms(Forms, ModuleForms, _Opts) ->
    ReversedForms = lists:reverse(ModuleForms),
    ModuleForms2 =
        case ReversedForms of
            [{eof, _} = EOF| ModuleForms1] ->
                lists:append([
                              [EOF],
                              Forms,
                              ModuleForms1
                             ]);
            _ ->
                lists:append([
                              Forms,
                              ReversedForms
                             ])
        end,
    lists:reverse(ModuleForms2).


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
-spec apply_changes(forms:forms()) -> meta_module().
apply_changes(Forms) ->
    apply_changes(Forms, []).

-spec apply_changes(forms:forms(), meta_opts()) -> meta_module().
apply_changes(Forms, Opts) ->
    apply_changes(module_name(Forms), Forms, Opts).

-spec apply_changes(module(), forms:forms(), meta_opts()) -> meta_module().
apply_changes(Module, Forms, Opts) ->
    File = module_file(Module),
    Dir = filename:dirname(File),

    Sticky = code:is_sticky(Module),
    Sticky andalso not forced(Opts) andalso throw({protected, Module}),

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
