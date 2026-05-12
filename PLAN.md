# Plan — notes for later

Items deferred during the foundations chapter cleanup. Don't fix yet, but
revisit when we do a follow-up sweep.

## Macros and notation

- **Matrix-macro convention is inconsistent.** In `latex-math/`:
  - `\Amat`, `\A`, `\H`, `\V` are defined as `\mathbf{...}` (upright bold)
  - `\id` is defined as `\bm{I}` (bold-italic) — the outlier
  Decide one convention and align all macros + the math rules.
  - Option A: matrices = `\mathbf`, vectors = `\bm`. Change `\id` to `\mathbf{I}` and update `rules-math.md` to say so explicitly.
  - Option B: everything bold-italic via `\bm`. Change `\Amat`/`\A`/`\H`/`\V`. Bigger visual change in rendered slides.

- **Candidates for new latex-math macros** (used >once across chapter 01):
  - `\hv = \bm{h}` (step / direction vector)
  - `\av = \bm{a}` (anchor / expansion point in Taylor)
  - Consider also `\B = \bm{B}` once matrix convention is decided

- **Remove `\littleo` macro from `latex-math/basic-math.tex`.** It's defined as `{o}`, which is the same as just writing `o` — no benefit. Remove the macro and use bare `o` everywhere. (`\bigO` is a different story since it expands to `\mathcal{O}`.)

## Style

- Decide whether `\fx` (= `f(\xv)`) should fully replace inline `f(\xv)` everywhere, or whether the spelled-out form is OK in proofs for readability.
- `\frac{\partial f}{\partial x_i}` vs the `\pd{f}{x_i}` macro — pick one.
- `\mathcal{S}` written out vs the `\S` macro — sweep across slides after foundations.

## Slide-level cleanups (across foundations)

- `\emph{...}` / `\textit{...}` → `\textbf{...}` per slide rules (multiple spots).
- Vectors written in plain italic for non-macro symbols (`x_0`, `x(t)` in level-curve proof) — render bold.
- `\left`/`\right` around plain tuples (no `\frac`/`\sum` inside) — drop.
- Single-character braces (`_{1}`, `_{2}`) → `_1`, `_2`.

## Content notes from W&R coverage audit

(See `slides/01-foundations/coverage-vs-wright-recht.md` for the full report; remaining items below.)

- Confirm the **descent lemma** statement in `01-diff.tex` is actually proved or restated in chapter 04 (multivariate first-order).
- The minimizer-variants frame in `04-optcond.tex` has an off-the-shelf counterexample for "strict ≠ isolated" — decide if the example pulls focus or stays.
- Coverage report references slide line numbers that shifted after content additions; refresh if/when the audit is revisited.
