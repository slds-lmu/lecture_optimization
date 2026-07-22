# Newton's Method on Linear-Predictor Models: The X^T D X Pattern

## Setup

Fix a dataset $\{(x_i, y_i)\}_{i=1}^n$, $x_i \in \mathbb{R}^d$, stacked into the design matrix $X \in \mathbb{R}^{n\times d}$. We restrict attention to models of the form

$$L(w) = \sum_{i=1}^n \ell(z_i;\, y_i) + \frac{\lambda}{2}\|w\|^2, \qquad z_i := w^\top x_i = (Xw)_i.$$

Two structural assumptions, and *only* these two, drive everything:

- **(A1) Linearity:** $w$ enters each term only through the scalar $z_i = w^\top x_i$.
- **(A2) Separability:** the loss is a sum over examples; example $i$ sees only $z_i$.

Keep track of where each assumption is used — that tells you exactly when the pattern breaks.

## Step 1 — Gradient

Chain rule on one term: since $\nabla_w z_i = x_i$,

$$\nabla_w\, \ell(z_i) = \ell'(z_i)\, x_i.$$

Sum over $i$ and define the **residual vector** $r \in \mathbb{R}^n$, $r_i := \ell'(z_i; y_i)$:

$$\boxed{\;\nabla L(w) = X^\top r + \lambda w\;}$$

Shape logic: the loss "lives" in example space $\mathbb{R}^n$; $X^\top$ pulls it back to parameter space $\mathbb{R}^d$. (A1) gave $x_i$ as the inner derivative; (A2) let us sum independent terms.

## Step 2 — Hessian

Differentiate the gradient. The only $w$-dependence in $X^\top r$ sits inside $r_i = \ell'(z_i)$:

$$\nabla_w\, r_i = \ell''(z_i)\, x_i.$$

Therefore

$$\nabla^2 L(w) = \sum_i \ell''(z_i)\, x_i x_i^\top + \lambda I = \boxed{\;X^\top D X + \lambda I\;}, \qquad D := \mathrm{diag}\big(\ell''(z_1), \dots, \ell''(z_n)\big).$$

- **Why $D$ is diagonal:** (A2). Cross-derivatives $\partial^2 L / \partial z_i \partial z_j$ vanish for $i \ne j$ because no term couples two examples. The Hessian *in $z$-space* is diagonal; $X^\top(\cdot)X$ is the pullback of that diagonal object to $w$-space.
- **Why rank-one blocks $x_i x_i^\top$:** (A1). A scalar bottleneck $z_i$ means each example's curvature contribution is curvature-in-one-direction, namely along $x_i$.

The pattern is not a coincidence of specific losses — it is a theorem about the model class. The loss never touches the *structure* $X^\top D X$; it only fills in the diagonal.

## Step 3 — The Newton Step Is Weighted Ridge Regression (IRLS)

Newton: $w^+ = w - (\nabla^2 L)^{-1}\nabla L$, i.e.

$$(X^\top D X + \lambda I)\, w^+ = X^\top D X w - X^\top r.$$

Assume $D_{ii} > 0$ and define the **working response** $\tilde z := Xw - D^{-1} r$. Then

$$(X^\top D X + \lambda I)\, w^+ = X^\top D\, \tilde z \quad\Longleftrightarrow\quad w^+ = \arg\min_v\; \sum_i D_{ii}\,\big(\tilde z_i - v^\top x_i\big)^2/\,2 \;+\; \tfrac{\lambda}{2}\|v\|^2.$$

**Each Newton iterate solves a weighted least-squares problem** with weights $D_{ii} = \ell''(z_i)$ and pseudo-targets $\tilde z_i = z_i - \ell'(z_i)/\ell''(z_i)$ (each target is itself a scalar Newton step on $\ell$). This is exactly IRLS. Newton on this model class = "repeatedly fit weighted ridge regressions."

## Step 4 — Instantiation

Only two scalar derivatives are ever computed.

**Least squares:** $\ell(z) = \tfrac12(z-y)^2$. Then $\ell' = z - y$, $\ell'' = 1$, so $D = I$, the Hessian $X^\top X + \lambda I$ is constant in $w$, the quadratic model is exact, and Newton converges in **one step** — recovering the normal equations.

**Logistic regression** ($y \in \{0,1\}$, $p_i = \sigma(z_i)$): $\ell' = p_i - y_i$, $\ell'' = p_i(1-p_i)$. So $D_{ii} = p_i(1-p_i) \in (0, \tfrac14]$: confident examples get near-zero weight. Working response $\tilde z_i = z_i - (p_i - y_i)/(p_i(1-p_i))$ — the classical IRLS for GLMs.

**L2-SVM** ($y \in \{\pm 1\}$): $\ell(z) = \max(0, 1 - yz)^2$, $\ell' = -2y(1-yz)_+$, and away from the kink $\ell'' = 2\cdot\mathbf{1}[y_i z_i < 1]$. Caveat: $\ell$ is $C^1$ but not $C^2$ at the margin, so $D$ is a **generalized** Hessian. Newton reduces to $(2 X_A^\top X_A + \lambda I)$ where $A$ = current margin violators — an active-set weighted least squares.

## Step 5 — When the Pattern Holds, and When It Doesn't

Holds for anything satisfying (A1)+(A2): probit, Poisson regression, Huber, any smooth GLM. Breaks down predictably:

- **Drop (A2)** — pairwise/ranking losses $\ell(z_i - z_j)$: still $X^\top H X$, but $H$ is no longer diagonal (graph-Laplacian-like structure on coupled pairs).
- **Drop (A1)** — nonlinear predictor $z_i = f(w; x_i)$ (neural nets): Hessian $= J^\top D J + \sum_i \ell'(z_i)\nabla^2 f_i$, $J$ the Jacobian. Keeping only $J^\top D J$ is the **Gauss–Newton approximation** — the pattern survives as an approximation, with $X$ replaced by a local linearization.
- **Multiclass:** $z_i \in \mathbb{R}^K$; $D$ becomes block-diagonal with $K\times K$ blocks ($\mathrm{diag}(p) - pp^\top$ for softmax) — same pullback structure, fatter bottleneck.

## Step 6 — Implications (Overview)

1. **Convexity.** $X^\top D X \succeq 0 \iff \ell'' \ge 0$ pointwise. Convexity of the $d$-dimensional problem reduces to convexity of a scalar function.
2. **Curvature = influence.** $D_{ii}$ says how much example $i$ shapes local geometry. Logistic anneals easy points away; L2-SVM discards them exactly (→ fast semismooth Newton / TRON solvers).
3. **Conditioning.** $\mathrm{rank}(X^\top D X) \le \min(d, \#\{i : D_{ii} > 0\})$. Separable data drives logistic's $p_i \to \{0,1\}$, hence $D \to 0$: Hessian collapses, $\|w\|\to\infty$, and $\lambda I$ keeps Newton well-posed.
4. **Computation.** Hessian-vector product: $v \mapsto X^\top(D(Xv)) + \lambda v$ — two matvecs; enables Hessian-free Newton-CG at scale.
5. **Statistics.** For GLMs with canonical link, $\ell''(z_i) = \mathrm{Var}(y_i \mid x_i)$ at the current fit, so Newton = Fisher scoring, and at the optimum $(X^\top D X)^{-1}$ is the asymptotic covariance of $\hat w$.

---

# Extension I: The Pattern Across the Model Zoo

The recipe is mechanical: pick $\ell$, compute two scalar derivatives, read off $D$. Each failure mode is as instructive as each success.

**Poisson regression (counts, canonical link).** $\ell(z) = e^z - yz$, so $\ell' = \mu - y$, $\ell'' = e^z = \mu$. Hence $D_{ii} = \mu_i$: the predicted mean *is* the weight, which is exactly the Poisson variance.

**Any canonical-link GLM.** For an exponential family with log-partition $b(\cdot)$: $\ell(z_i) = b(z_i) - y_i z_i$, so $\ell' = b'(z) - y = \mu - y$ and $\ell'' = b''(z) = \mathrm{Var}(y\mid x)$. The *entire GLM family* instantiates the pattern with $D = \mathrm{diag}(\text{conditional variances})$. Least squares is the constant-variance special case — which is *why* its Hessian is constant and Newton one-steps.

**Probit.** $\ell(z) = -\log\Phi(yz)$. Observed $\ell''$ is messy (Mills-ratio terms); the *expected* curvature (Fisher scoring) gives $D_{ii} = \varphi(z_i)^2 / [\Phi(z_i)(1-\Phi(z_i))]$. First instance where Newton and Fisher scoring split — non-canonical link.

**Exponential loss / boosting.** $\ell(z) = e^{-yz}$: $\ell'' = e^{-yz}$, so $D_{ii} = e^{-y_i z_i}$ — precisely AdaBoost's example weights. **LogitBoost is this lecture in function space:** replace "column of $X$" by "weak learner"; each round fits a weighted least-squares regression with weights $p_i(1-p_i)$ and working response $(y_i - p_i)/(p_i(1-p_i))$ — a functional Newton step.

**Huber regression.** $\ell'' = \mathbf{1}[|z - y| \le \delta]$: binary weights, outliers get zero curvature. Caveat: classical robust-statistics IRLS uses weights $\psi(r_i)/r_i$ (fixed-point scheme, globally stable for redescending $\psi$), not $\ell'' = \psi'(r_i)$ (Newton). Two different diagonal matrices, same $X^\top D X$ skeleton.

**Tukey biweight (nonconvex robustness).** $\ell'' < 0$ in regions → $D$ has negative entries → indefinite Hessian. Pattern survives; convexity doesn't. Trust regions/damping needed; the diagonal of $D$ identifies *which examples* cause indefiniteness.

**Quantile regression — degenerate boundary.** Pinball loss is piecewise linear: $\ell'' = 0$ a.e., so $D = 0$ and Newton is vacuous. Fix: convolution-smoothed ("conquer") quantile regression with bandwidth $h$ resurrects $D_{ii} = \frac{1}{h}K\!\big(\tfrac{z_i - y_i}{h}\big)$. Moral: the pattern requires *some* curvature; smoothing buys it.

**Multiclass softmax.** Vector bottleneck $z_i \in \mathbb{R}^K$: $D$ block-diagonal with blocks $\mathrm{diag}(p_i) - p_i p_i^\top$; Hessian as $(dK)$-dim quadratic form via Kronecker structure.

**Kernelized version.** Representer theorem: $w = X^\top\alpha$, objective $\sum_i \ell((K\alpha)_i) + \tfrac{\lambda}{2}\alpha^\top K \alpha$. Hessian in $\alpha$: $K D K + \lambda K$ — same pullback, $K$ playing $X$'s role.

**Cox partial likelihood — where (A2) genuinely dies.** Each term $\log\frac{e^{z_i}}{\sum_{j \in R_i} e^{z_j}}$ couples examples through risk sets $R_i$. Result: $X^\top H X$ with non-diagonal $H$ (softmax-covariance blocks over risk sets). The pullback form survives (needed only A1); *diagonality* was (A2)'s contribution.

**What breaks what:**

| Condition | Consequence |
|---|---|
| Keep (A1)+(A2), smooth $\ell$ | full pattern, diagonal $D$ |
| Drop smoothness (hinge, pinball) | $D$ degenerate → smooth or use generalized Hessians |
| Drop convexity (Tukey) | $D \not\succeq 0$, structure intact |
| Drop (A2) (Cox, ranking) | $X^\top H X$, $H$ structured non-diagonal |
| Drop (A1) (deep nets) | Gauss–Newton $J^\top D J$ as surviving approximation |

---

# Extension II: Step 6 in Depth

## B.1 Convexity → convergence speed: generalized self-concordance

Classical Newton theory wants self-concordance, $|\ell'''| \le 2(\ell'')^{3/2}$ — logistic **fails** this. But logistic satisfies $|\ell'''| \le \ell''$ (check: $\ell'' = p(1-p)$, $\ell''' = p(1-p)(1-2p)$, ratio bounded by 1). This "generalized self-concordance" (Bach 2010) proves: damped Newton has a global linear phase, then quadratic convergence in a basin whose radius depends on $\lambda_{\min}(X^\top D X + \lambda I)$. Practical reading: pure IRLS *can* diverge (overshoot when some $p_i(1-p_i) \approx 0$); IRLS + line search / Levenberg damping is provably safe.

## B.2 Curvature = influence, made quantitative

Define the **weighted hat matrix**

$$S = D^{1/2} X (X^\top D X + \lambda I)^{-1} X^\top D^{1/2}.$$

- **Leverage.** $S_{ii} \in [0,1]$ is example $i$'s self-sensitivity at the current fit — classical regression leverage transplanted to logistic/SVM via $D$.
- **Effective degrees of freedom.** $\mathrm{df} = \mathrm{tr}(S)$; for ridge $\sum_j \sigma_j^2/(\sigma_j^2 + \lambda)$. Feeds AIC/GCV-style model selection for any loss in the family.
- **Influence functions.** Deleting example $i$ changes $\hat w$ by $\approx (X^\top DX + \lambda I)^{-1} x_i\, \ell'(z_i)$ — Cook's distance generalizes verbatim; the identical formula is what data-attribution work (Koh & Liang 2017) computes at scale with $J^\top D J$ standing in.

## B.3 Conditioning: separation, existence of the MLE, preconditioning

- **Existence (Albert–Anderson 1984).** Unregularized logistic MLE exists iff data are *not* (quasi-)completely separated. Under separation, $w \to tw$ drives $L \to 0$ monotonically; no minimizer. Hessian symptom: along the separating direction all $D_{ii} \to 0$ exponentially. Optimization pathology and statistical non-existence are the same fact.
- **Effective condition number.** Newton's local rate depends on $\kappa(D^{1/2}X)$, not $\kappa(X)$. After a few IRLS steps the *problem being solved* has changed conditioning → recompute/adapt CG preconditioners, e.g. $\mathrm{diag}(X^\top DX) + \lambda$.
- **Role of $\lambda$:** spectrum floor, $\kappa \le (\sigma_{\max}(D^{1/2}X)^2 + \lambda)/\lambda$ — regularization converts a possibly nonexistent problem into a uniformly well-posed one with explicit guarantee.

## B.4 Computation: what the Gram structure buys at scale

$X^\top D X = \tilde X^\top \tilde X$ with $\tilde X = D^{1/2}X$ — a Gram matrix of the reweighted design. This unlocks:

- **Newton-CG cost accounting.** Per CG iteration: $Xv$, scale by $D$, $X^\top(\cdot)$ → $O(\mathrm{nnz}(X))$. Per Newton step: $O(\mathrm{nnz}(X)\cdot \#\text{CG})$; inexact-Newton theory allows crude early tolerances (forcing sequences).
- **Sketching / subsampling** (Newton sketch, Pilanci–Wainwright; subsampled Newton). Replace $\tilde X$ by a sketch $S\tilde X$ with $m \ll n$ rows, retaining spectral approximation w.h.p. — row-sampling proportional to *leverage scores of $\tilde X$*, exactly the $S_{ii}$ from B.2.
- **Distributed.** $X^\top D X = \sum_k X_k^\top D_k X_k$: local Gram matrices sum; one reduce per Newton step (basis of DANE/DiSCO-style communication-efficient methods).
- **L2-SVM:** only the active set $A$ enters; per-step cost $O(\mathrm{nnz}(X_A))$, shrinking as the fit improves — LIBLINEAR's TRON engine.

## B.5 Statistics: observed vs. expected information, misspecification

- **Newton vs. Fisher scoring.** Newton: observed information, $D_{\mathrm{obs},ii} = \ell''(z_i)$. Fisher: expected information $\mathbb{E}_y[\ell'']$. **Canonical links: they coincide** ($\ell'' = b''(z)$ contains no $y$: logistic's $p(1-p)$, Poisson's $e^z$). Probit: they differ → software runs Fisher scoring with Mills-ratio weights. Fisher's $D \succeq 0$ always — automatic positive-definiteness Newton lacks off-canonical-link.
- **Correct model:** $\widehat{\mathrm{Cov}}(\hat w) = (X^\top \hat DX)^{-1}$.
- **Misspecified — the sandwich:**
$$ (X^\top \hat D X)^{-1}\, \big(X^\top \mathrm{diag}(\ell'(z_i)^2)\, X\big)\, (X^\top \hat D X)^{-1} $$
Hessian bread, score-outer-product filling. Both are byproducts of the final IRLS iteration ($D$, $r$ in memory) — robust standard errors are computationally free. Bread-vs-filling gap = specification diagnostic (information-matrix test).

## B.6 Two more identifications

- **Natural gradient.** Fisher information in $w$ is $X^\top D X$, so the natural-gradient step $F^{-1}\nabla L$ *equals* the Fisher-scoring/Newton step. Second-order optimization and information-geometric steepest descent are the same algorithm here; K-FAC-type methods approximate the $J^\top D J$ analogue for deep nets.
- **Laplace approximation.** Gaussian prior $\mathcal{N}(0, \lambda^{-1}I)$ → Laplace posterior $\mathcal{N}\big(\hat w,\; (X^\top \hat D X + \lambda I)^{-1}\big)$. The matrix factorized in the final Newton step *is* the posterior precision — the workhorse of Bayesian logistic regression and marginal-likelihood ($\log\det$) model selection.

## Closing

One matrix, six jobs: $X^\top D X (+\lambda I)$ is simultaneously

1. the curvature Newton inverts,
2. the weight matrix of an equivalent ridge regression (IRLS),
3. the source of leverage / influence / effective-df diagnostics,
4. a sum-of-Grams object that sketching and distributed solvers exploit,
5. the (observed or expected) information whose inverse is the estimator's covariance,
6. the Laplace posterior precision.

The scalar function $\ell''$ is the only input; everything else is the linear-algebra consequence of a scalar bottleneck plus separability.
