
# Main Makefile

export EEN_NODE=een
EEN_AUX_NODES=w1 w2 w3 w4 w5
EEN_TEST_AUX_NODES=

export INCLUDE_DIR=include
export SOURCE_DIR=src/een
export SAMPLES_SOURCE_DIR=src/samples
export EBIN_DIR=ebin
TEST_DIR=test

INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
SAMPLES_SOURCES=$(wildcard $(SAMPLES_SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SOURCES))
SAMPLES_TARGETS=$(patsubst $(SAMPLES_SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam, $(SAMPLES_SOURCES))
ALL_TARGETS=$(TARGETS) $(SAMPLES_TARGETS)
TEST_SOURCES=$(wildcard $(TEST_DIR)/*.erl)
TEST_TARGETS=$(patsubst $(TEST_DIR)/%.erl, $(TEST_DIR)/%.beam, $(TEST_SOURCES))

ERLC_OPTS=-I $(INCLUDE_DIR) -pa $(EBIN_DIR) -o $(EBIN_DIR) -Wall -v +debug_info
ERL_OPTS=-pa $(EBIN_DIR) -pa $(TEST_DIR) -sname $(EEN_NODE)

all: compile

compile: $(ALL_TARGETS)

compile_tests: $(TEST_TARGETS)

run: $(ALL_TARGETS)
	$(MAKE) start_aux_nodes
	erl $(ERL_OPTS)
	$(MAKE) start_aux_nodes

all_tests: $(ALL_TARGETS) $(TEST_TARGETS)
	$(MAKE) EEN_AUX_NODES="$(EEN_AUX_NODES)" start_aux_nodes
	$(MAKE) -C $(TEST_DIR) test
	$(MAKE) EEN_AUX_NODES="$(EEN_AUX_NODES)" stop_aux_nodes

clean:
	rm -f $(ALL_TARGETS)
	$(MAKE) -C $(TEST_DIR) clean

.PHONY: start_aux_nodes
start_aux_nodes: $(TARGETS)
	for node in $(EEN_AUX_NODES) ; do \
	    echo ; \
	    echo "Starting node $$node" ; \
	    echo 'code:add_pathsa(["$(EBIN_DIR)"]), code:add_pathsa(["$(TEST_DIR)"]).' | erl_call -sname $$node -s -e ; \
	    done

.PHONY: stop_aux_nodes
stop_aux_nodes:
	for node in $(EEN_AUX_NODES) ; do \
	    echo "Stopping node $$node" ; \
	    erl_call -sname $$node -q ; \
	    done

##########################################################################
## Internal
##########################################################################

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

$(EBIN_DIR)/%.beam: $(SAMPLES_SOURCE_DIR)/%.erl $(TARGETS) $(INCLUDES)
	erlc -pa $(TARGETS) $(ERLC_OPTS) $<

$(TEST_TARGETS): $(TEST_DIR)

.PHONY: $(TEST_DIR)
$(TEST_DIR):
	$(MAKE) -C $(TEST_DIR) compile
