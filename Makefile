AGDA   ?= agda
RACKET ?= racket
RACO   ?= raco

PAPER_DIR    ?= papers/cola
PAPER_SRC    ?= paper.lagda.tex
PAPER_OUT    ?= latex
LATEX        ?= lualatex
BIBTEX       ?= bibtex
TEX_ENV       = TEXINPUTS=..: BIBINPUTS=..: BSTINPUTS=..:
LATEX_FLAGS  ?= -interaction=nonstopmode -halt-on-error

.DEFAULT_GOAL := help

.PHONY: help all verify setup corpus real-corpus experiments harvest paper paper-agda clean clean-paper

help:
	@echo "Targets:"
	@echo "  make verify       Typecheck the Agda strong-normalization proof (src/pest-sn)"
	@echo "  make experiments  Run both grammar corpora with the Racket prototype"
	@echo "  make corpus       Run the curated (looping) corpus only"
	@echo "  make real-corpus  Run the real Pest-grammar corpus only"
	@echo "  make harvest      Re-download and re-translate the real grammars (needs network)"
	@echo "  make all          verify + experiments"
	@echo "  make paper        Build the literate-Agda PDF (needs a TeX install)"
	@echo "  make paper-agda   Run only the Agda LaTeX back end (no PDF)"
	@echo "  make clean-paper  Remove the paper build artifacts only"
	@echo "  make clean        Remove generated build artifacts"

all: verify experiments

verify:
	@echo ">> typechecking the strong-normalization proof"
	cd src/pest-sn && $(AGDA) src/Pest/MainTheorem.agda
	@echo ">> checking that the development is fully constructive"
	@if grep -rn -e 'postulate' -e '{!' -e 'TERMINATING' -e 'trustMe' src/pest-sn/src; then \
	   echo "ERROR: escape hatches found in the formalization"; exit 1; \
	 else \
	   echo "OK: no postulates, holes, TERMINATING pragmas or trustMe in src/pest-sn"; \
	 fi

setup:
	@if $(RACKET) -e '(collection-path "typed-peg-stack")' >/dev/null 2>&1; then \
	   echo ">> typed-peg-stack collection already installed"; \
	 else \
	   echo ">> installing the typed-peg-stack collection"; \
	   (cd src/racket && $(RACO) pkg install --auto --name typed-peg-stack); \
	 fi
	@if $(RACKET) -e '(collection-path "rosette")' >/dev/null 2>&1; then \
	   echo ">> rosette already installed"; \
	 else \
	   echo ">> installing rosette (bundles its own solver)"; \
	   $(RACO) pkg install --auto rosette; \
	 fi

corpus: setup
	@echo ">> curated corpus (validates rejection of looping grammars)"
	cd src/racket && $(RACKET) corpus/run-corpus.rkt

real-corpus: setup
	@echo ">> real Pest-grammar corpus (from the committed translations)"
	cd src/racket && $(RACKET) corpus/run-real-corpus.rkt

experiments: corpus real-corpus

harvest:
	cd src/racket/corpus && bash harvest.sh

paper-agda:
	@echo ">> running the Agda LaTeX back end on $(PAPER_DIR)/$(PAPER_SRC)"
	cd $(PAPER_DIR) && $(AGDA) --latex --latex-dir=$(PAPER_OUT) \
	    -i . -i ../../src/pest-sn/src $(PAPER_SRC)

paper: paper-agda
	@command -v $(LATEX) >/dev/null 2>&1 || { \
	   echo "$(LATEX) not found; install a TeX distribution to build the PDF"; exit 1; }
	@echo ">> typesetting $(PAPER_DIR)/$(PAPER_OUT)/paper.tex with $(LATEX)"
	cd $(PAPER_DIR)/$(PAPER_OUT) && \
	  $(TEX_ENV) $(LATEX) $(LATEX_FLAGS) paper.tex && \
	  $(TEX_ENV) $(BIBTEX) paper && \
	  $(TEX_ENV) $(LATEX) $(LATEX_FLAGS) paper.tex && \
	  $(TEX_ENV) $(LATEX) $(LATEX_FLAGS) paper.tex
	cp $(PAPER_DIR)/$(PAPER_OUT)/paper.pdf $(PAPER_DIR)/paper.pdf
	@echo ">> wrote $(PAPER_DIR)/paper.pdf"

clean-paper:
	rm -rf $(PAPER_DIR)/$(PAPER_OUT)
	rm -f $(PAPER_DIR)/*.aux $(PAPER_DIR)/*.log $(PAPER_DIR)/*.out \
	      $(PAPER_DIR)/*.bbl $(PAPER_DIR)/*.blg $(PAPER_DIR)/*.spl \
	      $(PAPER_DIR)/*.ptb $(PAPER_DIR)/*.agdai

clean: clean-paper
	rm -rf src/pest-sn/_build
	find src/racket -type d -name compiled -prune -exec rm -rf {} + 2>/dev/null || true
