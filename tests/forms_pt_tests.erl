-module(forms_pt_tests).

-include_lib("eunit/include/eunit.hrl").

function_injection_test() ->
    "one" = dummy_module:int_to_text(1).
