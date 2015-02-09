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
    add_function/3, add_function/4,
    rm_function/4, rm_function/5,
    rename_function/5, rename_function/6,
    replace_function/4, replace_function/5,
    rm_spec/3, rm_spec/4,
    is_function_exported/3,
    export_function/3,
    unexport_function/3, unexport_function/4,
    function/3,
    spec/3,
    calling_functions/3,
    type/3,
    is_type_exported/2,
    record/2,
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
-spec add_function(meta_abs_function(), boolean(), module(), meta_opts())
                  -> meta_module().
add_function(Function, Exp, Mod, Opts)
  when is_atom(Mod) ->
    OldForms = load_forms(Mod),
    NewForms = add_function(Function, Exp, OldForms),
    apply_changes(Mod, NewForms, Opts).

-spec add_function(meta_abs_function(), boolean(), forms:forms())
                  -> forms:forms().
add_function(Fun, Arity, Module)
  when is_atom(Module) ->
    add_function(Fun, Arity, Module, []);
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
              -> {meta_abs_function(), list()} |
                 {meta_abs_function(), meta_abs_spec(), list(), list()}.
function(Name, Arity, Mod)
  when is_atom(Mod) ->
    function(Name, Arity, load_forms(Mod));
function(Name, Arity, Forms) ->
    {Fun, Records} = '_function'(Name, Arity, Forms),
    case catch spec(Name, Arity, Forms) of
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
-spec spec(atom(), arity(), meta_module())
                   -> {meta_abs_spec(), list(), list()} | no_return().
spec(Name, Arity, Module)
  when is_atom(Module) ->
    spec(Name, Arity, load_forms(Module));
spec(Name, Arity, Forms) ->
    Spec = '_spec'(Name, Arity, Forms),
    {DepTypes, DepRecords} = dependant_types(Spec),
    {Spec, DepTypes, nested_records(DepRecords, Forms)}.


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
%% Fetch the abstract code of the specified type.
%% @end
%%-------------------------------------------------------------------------
-spec type(atom(), arity(), meta_module())
          -> {meta_abs_type(), list({atom(), arity()}), list()}.
type(Name, Arity, Module)
  when is_atom(Module) ->
    type(Name, Arity, load_forms(Module));
type(Name, Arity, Forms) ->
    Type = '_type'(Name, Arity, Forms),
    {Ts, Rs} = dependant_types(Type),
    {Type, Ts, nested_records(Rs, Forms)}.

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
%% Fetch the abstract code of the specified record.
%% @end
%%-------------------------------------------------------------------------
-spec record(atom(), meta_module())
            -> {meta_abs_record(), meta_abs_type(), list(), list()}.
record(Name, Module)
  when is_atom(Module) ->
    record(Name, load_forms(Module));
record(Name, Forms) ->
    {Record, RecordType} = '_record'(Name, Forms, {undefined, undefined}),

    DepRecords1 = dependant_records(Record),
    DepRecords2 = dependant_records(RecordType),
    {DepTypes, DepRecords3} = dependant_types(RecordType),

    DepRecords = lists:usort(lists:append([DepRecords1,
                                           DepRecords2,
                                           DepRecords3])),
    {Record,
     RecordType,
     DepTypes,
     nested_records(DepRecords, Forms)}.

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
    {Ts, Rs} =
        forms:reduce(
          fun({type, _, union, _}, Acc) ->
                  Acc;
             ({type, _, record, [{atom, _, Record}]}, {Ts, Rs}= _Acc) ->
                  {Ts, [Record| Rs]};
             ({type, _, Name, Args}, {Ts, Rs} = Acc) ->
                  case is_builtin_type(Name) of
                      false ->
                          {[{Name, length(Args)}| Ts], Rs};
                      true ->
                          Acc
                  end;
             (_, Acc) ->
                  Acc
          end,
          {[], []},
          [Form]),
    {lists:usort(Ts), lists:usort(Rs)}.

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

nested_records(Records, Module) ->
    '_nested_records'(Records, [], Module).

'_nested_records'([], Acc, _Modul) ->
    Acc;
'_nested_records'([R| Records], Acc, Module) ->
    case lists:member(R, Acc) of
        true ->
            '_nested_records'(Records, Acc, Module);
        false ->
            {_, _, _, X} = record(R, Module),
            '_nested_records'(X ++ Records, [R| Acc], Module)
    end.

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
