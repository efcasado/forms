-module(meta_tests).

-compile({parse_transform, forms_pt}).

-include_lib("eunit/include/eunit.hrl").


module_name_test() ->
    Forms = forms:read(dummy_module),
    dummy_module = meta:module_name(Forms).

has_attr_true_test() ->
    true = meta:has_attr(module, dummy_module).

has_attr_false_test() ->
    false = meta:has_attr(export, dummy_module).

add_exported_function_transient_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),
    %% Alternatively, we could have used
    %% FooFunction = forms:to_abstract("foo() -> foo."),

    false = meta:has_attr(export, dummy_module),

    Forms = meta:add_function(FooFunction, true, dummy_module, []),

    true = meta:has_attr(export, Forms),

    foo = dummy_module:foo().

add_remove_exported_function_permanent_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),

    false = meta:has_attr(export, dummy_module),

    meta:add_function(FooFunction, true, dummy_module, [permanent]),

    true = meta:has_attr(export, dummy_module),

    foo = dummy_module:foo(),

    meta:rm_function(foo, 0, true, dummy_module),
    {'EXIT', {undef, _}} = (catch dummy_module:foo()).
