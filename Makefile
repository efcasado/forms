PROJECT             = forms

TEST_DIR            = tests

TEST_COMPILE_FIRST  = dummy_transform
TEST_ERLC_EXCLUDE   = no_debug_info

TEST_ERLC_OPTS0    := -o $(TEST_DIR) $(TEST_ERLC_OPTS)
TEST_ERLC_OPTS     += -pa $(TEST_DIR) +debug_info

include erlang.mk

test-build::
	@erlc -v $(TEST_ERLC_OPTS0) tests/no_debug_info.erl
