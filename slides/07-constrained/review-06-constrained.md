# Review notes: Chapter 06 – Constrained Optimization

Lecture: `lecture_optimization/slides/06-constrained/`
Status: in progress (files 1–4 reviewed; files 5–9 pending)

---

## `slides-constrained-1-intro.tex`

**Summary:** Motivates constrained optimization with examples (MLE, Lasso/Ridge, constrained Lasso, SVM dual), gives the general problem definition, and sketches the hierarchy of convex program classes.

**Issues**

1. **"Box constraints" is used but never defined** (SVM-dual slide, line 70; also used in ch. 2 and in `optim-rest-7-constrained-solvers` without definition). A one-line definition suffices: per-coordinate lower and upper bounds $l_i \le x_i \le u_i$, here $0 \le \alpha_i \le C$.
2. **"Convex inequality constraints" is stated as a requirement without explanation.** Convex *functions* are defined in `01-foundations/03-convex.tex`, but the slide never explains why "$g_i$ convex, $h_j$ affine" is the right condition: sublevel sets $\{g_i \le 0\}$ of convex functions are convex, level sets $\{h_j = 0\}$ are convex only for affine $h_j$, and intersections of convex sets are convex, hence the feasible set $\mathcal S$ is convex. Without this, the link to the ch. 2 definition of a convex problem ("$f$ convex and $\mathcal S$ convex") is missing. Terminology also flips between "linear" and "affine" constraints.
3. **SOCP / SDP / CP are name-dropped without definitions** (lines 103–111) and never used again in the lecture. Either reduce to a further-reading remark, or add one slide with a one-line definition of each and the inclusion chain LP ⊂ QP ⊂ SOCP ⊂ SDP ⊂ CP, since "hierarchy of convex problems" is a declared learning goal.

---

## `slides-constrained-2-lp.tex`

**Summary:** LP examples from statistics (quantile regression, Dantzig selector), standard form and conversion to it, geometric interpretation (half-spaces, polytopes, convexity), the three solution cases.

**Issues**

1. **The Dantzig selector example is not intuitive**, and — more importantly — neither it nor quantile regression is shown to *be* an LP. The slides are titled "LP examples", but the reformulation (splitting the check function / the $\ell_\infty$ norm into linear constraints) is never given; the $\ell_1$/$\ell_\infty$ tricks only appear later in `2-lp-simplex`.
2. **Slide 5/12, "No non-negativity constraint":** the substitution $x_i = x_i^+ - x_i^-$ with $x_i^\pm \ge 0$ is stated without saying what problem it solves (free variables in a form that requires $\xv \ge 0$) and without an example. The whole conversion slide is a list of tricks with no worked case.
3. **Slides 8/12 and 9/12 ("Polytopes", "Convexity") are text-only.** A figure showing facets/edges/vertices of a 2-D or 3-D polytope would help.
4. The claim "the polytope $\{\xv: \Amat\xv \le \bv\}$ is an $n$-simplex" (line 130) is false in general (a polytope defined by $m$ half-spaces is usually not a simplex).
5. Standard form is `max` here but `min` in `2-lp-simplex`.

---

## `slides-constrained-2-lp-simplex.tex`

**Summary:** Repeats LP definition and geometry, introduces general/standard/equality forms and slack variables, vertex partitions, and a formal derivation of the simplex algorithm via the Lagrangian (pivoting, minimum ratio test, pivoting rules, worked example).

**Issues**

1. **Largely redundant with `2-lp`:** repeats the LP definition, the geometric interpretation, the "3 cases" slide with the same figure, and the $n$-simplex claim. The LP-forms material should be merged into `2-lp` and removed here.
2. **General form, equality form and slack variables are not properly introduced.** They are dropped in as formulas without explaining why equality form is needed (the simplex operates on it) or what slack variables mean geometrically.
3. **The simplex derivation relies on the Lagrangian and complementary slackness** ($\muv \odot \xv = 0$, line 160), which are only introduced in `5-nonlinear-lagrangian` and `optim-rest-6-regularity-conditions`. Nothing before this point in the lecture mentions the Lagrangian.
4. **The simplex treatment is complete but dense and formal, with no intuitive introduction.** The content itself is the most thorough in the chapter (partitions, optimality via $\muv_{\mathcal V}$, pivoting, ratio test, Dantzig's/Bland's rules, worked example); what is missing is intuition, prerequisites, and the right order — the intuitive version lives in `3-lp-solvers`, which comes *after* this file. Recommendation: merge `2-lp-simplex` and `3-lp-solvers` into one self-contained simplex chapter in the order intuition → Phase I/II → formal derivation.
5. Notation errors in the worked example (lines 245–262): $\Amat_{\mathcal V}$ should be $\Amat$; $\bm\lambda = \Amat_{\mathcal B}^{-1}\mathbf c_{\mathcal B}$ should be $\Amat_{\mathcal B}^{-T}\mathbf c_{\mathcal B}$; stray minus sign in "$-\Amat_{\mathcal B}^{-1}\Amat_{\{q\}} = (1,2)^T$". The numerical results ($\xv_{\mathcal B}=(1,8)$, $\bm\lambda=(0,-\tfrac12)$, $\muv_{\mathcal V}=(1,\tfrac12)$, $\xv^*=(0,1,8,0)$) are correct.
6. Line 107: "rows of $\Amat$ linearly independent and $m \le n$ to form a bounded non-empty feasible set" — these assumptions do not guarantee boundedness or non-emptiness.

---

## `slides-constrained-3-lp-solvers.tex`

**Summary:** Informal introduction to the simplex algorithm (walking along edges of the polytope), Phase I / Phase II, a four-step visual example.

**Issues**

1. **Introduces the simplex algorithm a second time**, with a better intuition than `2-lp-simplex` but placed after it. Merge with `2-lp-simplex` into one simplex chapter (see above).
2. "Georg Dantzig" → George Dantzig (line 25).

---

## `slides-constrained-4-lp-duality.tex`

**Summary:** Bakery example, lower-bound intuition, primal/dual correspondence table, weak and strong duality, alternative LP formulation leading to the Lagrangian.

**Issues**

1. **"Primal" is used before it is defined.** The term already appears in ch. 2 (`slides-problems-2-constrained.tex:206`, "two primal, and one dual one (we will see later what 'dual' means)"), and the overview slide here (line 28) refers to "your original 'primal' problem" in scare quotes, but it is only properly named on the "Duality" slide (line 183). An explicit one-liner should come first: "the original problem is called the *primal* problem, the derived lower-bound problem the *dual* problem".
2. The final remarks (line 272) claim the simplex algorithm was skipped, although two files on it precede this one.
3. Strong duality theorem (line 218): "has a constrained solution" should be "has a finite optimal solution".
4. Attribution "1940s (works of Tucker and Wolfe)" (line 26) is off: LP duality is usually credited to von Neumann and Gale–Kuhn–Tucker; Wolfe duality dates from 1961.

---

## `slides-constrained-5-nonlinear-lagrangian.tex`

_pending_

## `slides-constrained-6-fenchel-duality.tex`

_pending_

## `slides-optim-rest-6-regularity-conditions.tex`

_pending_

## `slides-optim-rest-7-constrained-solvers.tex`

_pending_
