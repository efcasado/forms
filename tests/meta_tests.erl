-module(meta_tests).

-compile({parse_transform, forms_pt}).

-include_lib("eunit/include/eunit.hrl").


%% ========================================================================
%%  Unit tests
%% ========================================================================

module_name_test() ->
    Forms = forms:read(dummy_module),
    dummy_module = meta:module_name(Forms).

has_attr_true_test() ->
    true = meta:has_attr(module, dummy_module).

has_attr_false_test() ->
    false = meta:has_attr(export, dummy_module).

is_function_exported_test() ->
    false = meta:is_function_exported(donotexist, 0, dummy_module).

add_exported_function_transient_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),
    %% Alternatively, we could have used
    %% FooFunction = forms:to_abstract("foo() -> foo."),

    Forms0 = forms:read(dummy_module),

    false = meta:is_function_exported(foo, 0, Forms0),

    Forms1 = meta:add_function(FooFunction, true, Forms0),
    meta:apply_changes(Forms1), %% transient

    %% The function has been added and its callable, but it has not
    %% beam injected to dummy_module's beam file.
    true = meta:is_function_exported(foo, 0, Forms1),
    foo = dummy_module:foo(),
    false = meta:is_function_exported(foo, 0, dummy_module).

add_remove_exported_function_permanent_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),

    false = meta:is_function_exported(foo, 0, dummy_module),

    meta:add_function(FooFunction, true, dummy_module, [permanent]),

    true = meta:is_function_exported(foo, 0, dummy_module),

    foo = dummy_module:foo(),

    meta:rm_function(foo, 0, true, dummy_module, [permanent]),
    {'EXIT', {undef, _}} = (catch dummy_module:foo()).

rename_function_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),

    Forms1 = meta:add_function(FooFunction, true, dummy_module, []),
    foo = dummy_module:foo(),

    Forms2 = meta:rename_function(foo, 0, bar, true, Forms1),
    meta:apply_changes(Forms2),
    {'EXIT', {undef, _}} = (catch dummy_module:foo()),
    foo = dummy_module:bar().

add_function_protected_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),

    ListsForms0 = forms:read(lists),
    ListsForms1 = meta:add_function(FooFunction, _Export = true, ListsForms0),
    meta:apply_changes(ListsForms1, [force]),

    foo = lists:foo(),

    %% Clean-up
    meta:apply_changes(ListsForms0, [force]),
    {'EXIT', {undef, _}} = (catch lists:foo()).

add_function_protected_error_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),

    ListsForms0 = forms:read(lists),
    ListsForms1 = meta:add_function(FooFunction, _Export = true, ListsForms0),
    {protected, lists} = (catch meta:apply_changes(ListsForms1)).

rename_function_protected_test() ->
    NewLookUp = forms:function(lookup,
                               fun(_Key, _List) ->
                                       hijacked
                               end,
                               []),

    PropListsForms0 = forms:read(proplists),

    PropListsForms1 =
        meta:rm_function(lookup, 2, _RmAll = false, PropListsForms0),
    PropListsForms2 =
        meta:add_function(NewLookUp, _Export = true, PropListsForms1),
    meta:apply_changes(PropListsForms2, [force]),

    hijacked = proplists:lookup(foo, []),

    %% Clean-up
    meta:apply_changes(PropListsForms0, [force]),
    none = proplists:lookup(foo, []).

replace_function_protected_test() ->
    NewLookUp = forms:function(lookup,
                               fun(_Key, _List) ->
                                       hijacked
                               end,
                               []),

    PropListsForms0 = forms:read(proplists),

    meta:replace_function(lookup, 2, NewLookUp, proplists, [force]),

    hijacked = proplists:lookup(foo, []),

    %% Clean-up
    meta:apply_changes(PropListsForms0, [force]),
    none = proplists:lookup(foo, []).


replace_function_wrong_arity_test() ->
    NewGetValue = forms:function(get_value,
                                 fun(Key, List, Oops) ->
                                         get_value(Key, List, hijacked)
                                 end,
                                 []),

    wrong_arity = (catch meta:replace_function(get_value,
                                               2,
                                               NewGetValue,
                                               proplists,
                                               [force])).
