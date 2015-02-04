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

add_exported_function_test() ->
    FooFunction = forms:function(foo,
                                 fun() -> foo end,
                                 []),
    %% Alternatively, we could have used
    %% FooFunction = forms:to_abstract("foo() -> foo."),

    false = meta:has_attr(export, dummy_module),

    Forms = meta:add_function(FooFunction, true, dummy_module, []),

    true = meta:has_attr(export, Forms),

    foo = dummy_module:foo().
