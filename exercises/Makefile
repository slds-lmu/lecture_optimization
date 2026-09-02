SHELL:=/bin/bash
CHAPTER=$(shell find . -mindepth 2 -maxdepth 2 -type f -name 'Makefile' -exec dirname "{}" \;)
QUARTO_CHAPTER=$(shell find . -mindepth 2 -maxdepth 2 -type d -name 'exercise-*-quarto')
FCHANGED=$(shell git diff --name-only origin/main)

all:
	@echo "Specify target 'changes' / 'all-pdf' (legacy Rnw) or 'quarto-changes' / 'quarto-all' (Quarto)."
	@echo "Quarto helpers: 'init-quarto-chapter NN=06', 'sync-math'."

# Scaffold a new exercise-NN-quarto/ chapter dir.
# Usage: make init-quarto-chapter NN=06
init-quarto-chapter:
	@if [ -z "$(NN)" ]; then echo "ERROR: usage: make init-quarto-chapter NN=06"; exit 1; fi
	@if [ -d "exercise-$(NN)-quarto" ]; then echo "ERROR: exercise-$(NN)-quarto already exists"; exit 1; fi
	@mkdir -p "exercise-$(NN)-quarto/_quarto"
	@cp _quarto/latex-math.qmd "exercise-$(NN)-quarto/_quarto/latex-math.qmd"
	@echo 'include ../quarto.mk' > "exercise-$(NN)-quarto/Makefile"
	@echo "Initialized exercise-$(NN)-quarto/ (Makefile + _quarto/latex-math.qmd)."
	@echo "Next: author <topic>.qmd, sol_<topic>_R.ipynb, sol_<topic>_py.ipynb, sol_<topic>_texts.ipynb"

# Re-copy exercises/_quarto/latex-math.qmd into every chapter-local _quarto/.
# Run this after editing the project-level macros.
sync-math:
	@for d in exercise-*-quarto; do \
		if [ -d "$$d/_quarto" ]; then \
			echo "==> syncing $$d/_quarto/latex-math.qmd"; \
			cp _quarto/latex-math.qmd "$$d/_quarto/latex-math.qmd"; \
		fi; \
	done

quarto-all:
	@for d in $(QUARTO_CHAPTER); do \
		echo "==> rendering $$d"; \
		$(MAKE) -C $$d all || exit 1; \
	done

quarto-changes:
	@echo "Hello from the $$(git rev-parse --abbrev-ref HEAD) branch"
	@for d in $(QUARTO_CHAPTER); do \
		dn=$${d#./}; \
		glc=$$(echo "$(FCHANGED)" | tr ' ' '\n' | grep "^exercises/$$dn/" | wc -l); \
		if [ $$glc -gt 0 ]; then \
			echo "==> rendering $$d ($$glc changed files)"; \
			$(MAKE) -C $$d all || exit 1; \
		else \
			echo "    skip $$d (no changes)"; \
		fi; \
	done

changes:
	@echo "Hello from the $(shell git rev-parse --abbrev-ref HEAD) branch"
	@for i in $(CHAPTER); do\
		DN=$$(echo $$i | sed -e "s/\.//g");\
		GLC=$$(echo $$FCHANGED | grep "exercises"$$DN | grep "[.]Rnw" | wc -l);\
		echo "processing $$DN";\
		if [[ $$GLC != 0 ]]; then\
			cd $$i;\
			echo "cd into $$(pwd)";\
			make exercises-pdf;\
			cd ..;\
		else\
			echo "nothing todo for $$i";\
		fi;\
	done

all-pdf:
	@for i in $(CHAPTER); do\
		echo "processing $$i";\
		cd $$i;\
		echo "cd into $$(pwd)";\
		make exercises-pdf;\
		cd ..;\
	done


### OLD:
#EX = $(shell find . -maxdepth 2 -type f \( -iname "ex_*.Rnw" -o -iname "sol_*.Rnw" -o -iname "collection_*.Rnw" \))
#EXS = $(EX:%.Rnw=%.pdf)

#all: texclean $(EXS) texclean copy

#$(EXS): %.pdf: %.Rnw
	#Rscript -e 'setwd("$(dir $<)"); knitr::knit2pdf("$(notdir $<)")'

#copy:
	#find . -maxdepth 2 -type f \( -iname "ex_*.pdf" -o -iname "sol_*.pdf" \) -exec cp {}  ../exercises-pdf \;

#texclean:
	#rm -rf *.out
	#rm -rf *.dvi
	#rm -rf *.log
	#rm -rf *.aux
	#rm -rf *.bbl
	#rm -rf *.blg
	#rm -rf *.ind
	#rm -rf *.idx
	#rm -rf *.ilg
	#rm -rf *.lof
	#rm -rf *.lot
	#rm -rf *.toc
	#rm -rf *.nav
	#rm -rf *.snm
	#rm -rf *.vrb
	#rm -rf *.synctex.gz
	#rm -rf *-concordance.tex
	#rm -rf *.tex
