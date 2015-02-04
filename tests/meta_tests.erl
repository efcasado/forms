-module(meta_tests).

-include_lib("eunit/include/eunit.hrl").

module_name_test() ->
    Forms = forms:read(dummy_module),
    dummy_module = meta:module_name(Forms).

has_attr_true_test() ->
    true = meta:has_attr(module, dummy_module).

has_attr_false_test() ->
    false = meta:has_attr(export, dummy_module).
