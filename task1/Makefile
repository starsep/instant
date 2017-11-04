BUILD=build
SHELL=/usr/bin/env bash
GHCFLAGS= #-Wall
GHC=ghc
TMP=tmp
INSTESTER_DIR=./test/instester
INSTESTER=$(INSTESTER_DIR)/dist/build/instester/instester
TEST_DIR=$(BUILD)/test

SHELL_SCRIPTS=insc_jvm insc_llvm usage.sh
FILES_TO_PACK=$(SHELL_SCRIPTS) src lib Makefile README
PACK_NAME=fc359081.tgz

BINARIES=Compiler # TestInstant
SOURCES=Compiler JVM LLVM
LINKED_SOURCES=$(addsuffix .hs,$(addprefix $(BUILD)/,$(SOURCES)))
BNFC_SOURCES_FILES=AbsInstant.hs ErrM.hs LexInstant.hs \
	ParInstant.hs PrintInstant.hs TestInstant.hs
BNFC_SOURCES=$(addprefix $(BUILD)/,$(BNFC_SOURCES_FILES))

.PHONY: all clean pack test testPrepare testAll

all: $(BINARIES)

test: testPrepare
	cd $(TEST_DIR) && \
	./instester . "examples/"

testAll: testPrepare
	ln -sfr test/mrjp_instant_gen/examples/* $(TEST_DIR)/examples && \
	cd $(TEST_DIR) && \
	./instester . "examples/"

testPrepare: $(INSTESTER) pack
	mkdir -p $(TEST_DIR) && \
	mv $(PACK_NAME) $(TEST_DIR) && \
	ln -sfr $(INSTESTER) $(TEST_DIR)/instester && \
	cp -r test/examples $(TEST_DIR)

$(INSTESTER):
	cd "$(INSTESTER_DIR)" && \
	cabal install shelly && \
	cabal configure && \
	cabal build

$(BINARIES): %: $(BNFC_SOURCES) $(LINKED_SOURCES)
	cd $(BUILD) && \
	$(GHC) $(GHCFLAGS) --make $@.hs -o ../$@

$(LINKED_SOURCES): $(BUILD)/%: src/%
	ln -srf $^ $(BUILD)

$(BNFC_SOURCES): src/Instant.cf
	mkdir -p $(BUILD) && \
	cd $(BUILD) && \
	bnfc -haskell ../$< && \
	happy -gca ParInstant.y && \
	alex -g LexInstant.x
	rm -f $(BUILD)/SkelInstant.hs && \
	sed -i "/SkelInstant/d" $(BUILD)/TestInstant.hs

pack:
	tar czvf $(PACK_NAME) $(FILES_TO_PACK)

shellcheck:
	shellcheck $(SHELL_SCRIPTS)

clean:
	rm -rf $(BUILD) $(TMP) $(BINARIES) $(TEST_DIR) *.tgz 
