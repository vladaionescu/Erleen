
# Testing Makefile

TEST_SOURCES=$(wildcard *.erl)
TEST_TARGETS=$(patsubst %.erl, %.beam, $(TEST_SOURCES))
INCLUDES=$(wildcard ../$(INCLUDE_DIR)/*.hrl)

ERLC_OPTS=-I ../$(INCLUDE_DIR) -o ./ -Wall -v +debug_info

ERL_OPTS=-pa ../$(EBIN_DIR) -pa ./ -sname $(EEN_NODE) -noshell

all: test

compile: $(TEST_TARGETS)

%.beam: %.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

test: $(TEST_TARGETS)
	erl $(ERL_OPTS) -s een_SUITE test -s erlang halt

clean:
	rm -f $(TEST_TARGETS)
