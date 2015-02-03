###========================================================================
### File: Makefile
###
### Author: Enrique Fernandez <enrique.fernandez (at) erlang-solutions.com>
### Date:   January, 2015
###
###-- LICENSE -------------------------------------------------------------
### The MIT License (MIT)
###
### Copyright (c) 2015 Enrique Fernandez
###
### Permission is hereby granted, free of charge, to any person obtaining
### a copy of this software and associated documentation files (the
### "Software"), to deal in the Software without restriction, including
### without limitation the rights to use, copy, modify, merge, publish,
### distribute, sublicense, and/or sell copies of the Software,
### and to permit persons to whom the Software is furnished to do so,
### subject to the following conditions:
###
### The above copyright notice and this permission notice shall be included
### in all copies or substantial portions of the Software.
###
### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
### IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
### CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
### TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
### SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
###========================================================================
ERL               = $(shell which erl)
ERLC              = $(shell which erlc)
ERLC_OPTS         = -o $(BIN_DIR) +debug_info
TEST_ERLC_OPTS    = -o $(TST_DIR) -DTEST -pa $(BIN_DIR) -pa $(TST_DIR) +debug_info
BIN_DIR           = ebin
SRC_DIR           = src
TST_DIR           = tests
SRC_FILES         = $(notdir $(shell find $(SRC_DIR) -type f -name *.erl))
BIN_FILES         = $(patsubst %.erl,$(BIN_DIR)/%.beam,$(SRC_FILES))
BUILD_TARGETS     = $(BIN_FILES)
TST_COMPILE_FIRST = dummy_transform
TST_COMPILE_FIRST1 = $(patsubst %,$(TST_DIR)/%.beam,$(TST_COMPILE_FIRST))
TST_FILES         = $(patsubst %.erl,%.beam,$(shell find $(TST_DIR) -type f -name *.erl))
TST_BUILD_TARGETS = $(TST_COMPILE_FIRST1) $(filter-out $(TST_COMPILE_FIRST1),$(TST_FILES))

VPATH             = $(SRC_DIR) $(TST_DIR)

.PHONY: $(SRC_DIR) $(TST_DIR)

## =========================================================================
##  Targets
## =========================================================================
all: build

build: $(BIN_DIR) $(BUILD_TARGETS)

build-tests: $(TST_BUILD_TARGETS)

$(BIN_DIR):
	mkdir $(BIN_DIR)

ebin/%.beam: %.erl
	erlc $(ERLC_OPTS) $<

tests/%.beam: %.erl
	erlc $(TEST_ERLC_OPTS) $<

EUNIT_RUN = erl \
-noshell \
-pa $(BIN_DIR) \
-pa $(TST_DIR) \
-eval 'case eunit:test([{dir,"$(TST_DIR)"}],[verbose]) of ok -> erlang:halt(0); _ -> erlang:halt(1) end.'

test: build build-tests
	$(EUNIT_RUN)

clean: clean-tests
	rm -f $(BIN_DIR)/*

clean-tests:
	rm -f $(TST_DIR)/*.beam
