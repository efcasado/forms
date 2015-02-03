-module(dummy_module).

-compile({parse_transform, dummy_transform}).

-export([int_to_text/1]).

int_to_text(I) when is_integer(I) ->
    to_text(I).
