.PHONY: all sol ex prep execute solhtml exhtml solpdf expdf clean copy help

# Each chapter Makefile must set TEXTS to the texts notebook(s) in this folder.
# Default: all sol_*_texts.ipynb files in the current directory.
TEXTS ?= $(wildcard sol_*_texts.ipynb)

# Repo-relative path back to exercises/scripts/replace_texts.py from a chapter dir
SCRIPTS := ../scripts

help:
	@echo "Quarto exercise build targets:"
	@echo "  prep      : run replace_texts.py for every TEXTS file -> populates inserted/"
	@echo "  execute   : run inserted/sol_*.ipynb in-place to cache cell outputs"
	@echo "              (R notebooks need IRkernel registered as 'ir'; failures are non-fatal)"
	@echo "  solhtml   : render solution HTML (needs prep + execute first)"
	@echo "  exhtml    : render exercise-only HTML"
	@echo "  solpdf    : render solution PDF"
	@echo "  expdf     : render exercise-only PDF"
	@echo "  copy      : copy *.html and *.pdf in inserted/ to ../../exercises-pdf/"
	@echo "  clean     : remove inserted/ and quarto build artifacts"

all: execute prep solhtml exhtml solpdf expdf

prep:
	@if [ -z "$(TEXTS)" ]; then \
		echo "ERROR: no sol_*_texts.ipynb files found in $$(pwd)"; exit 1; \
	fi
	@for txt in $(TEXTS); do \
		echo "==> replace_texts.py --texts $$txt"; \
		python $(SCRIPTS)/replace_texts.py . --texts $$txt || exit 1; \
	done

execute:
	@for nb in sol_*.ipynb; do \
		case "$$nb" in *_texts.ipynb) continue;; esac; \
		echo "==> executing $$nb"; \
		python -m jupyter execute --inplace $$nb \
			|| echo "WARN: could not execute $$nb (kernel missing or runtime error). Continuing."; \
	done

solhtml: prep
	@for f in inserted/*.qmd; do \
		case "$$f" in inserted/sol_*) continue;; esac; \
		base=$$(basename $${f%.qmd}); \
		quarto render $$f --profile=solution --to html || exit 1; \
		mv "inserted/$${base}.html" "$${base}_all.html"; \
	done

exhtml: prep
	@for f in inserted/*.qmd; do \
		case "$$f" in inserted/sol_*) continue;; esac; \
		base=$$(basename $${f%.qmd}); \
		quarto render $$f --to html || exit 1; \
		mv "inserted/$${base}.html" "$${base}_ex.html"; \
	done

solpdf: prep
	@for f in inserted/*.qmd; do \
		case "$$f" in inserted/sol_*) continue;; esac; \
		quarto render $$f --profile=solution --to pdf --output "$$(basename $${f%.qmd})_all.pdf" || exit 1; \
	done

expdf: prep
	@for f in inserted/*.qmd; do \
		case "$$f" in inserted/sol_*) continue;; esac; \
		quarto render $$f --to pdf --output "$$(basename $${f%.qmd})_ex.pdf" || exit 1; \
	done

copy:
	@cp *.html ../../exercises-pdf/ 2>/dev/null || true
	@cp *.pdf ../../exercises-pdf/ 2>/dev/null || true

clean:
	@rm -rf inserted/ *_files/ .quarto/ *.rmarkdown
