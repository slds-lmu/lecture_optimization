# 1 -- Newton Raphson

Motivation? We somehow motivate it in chapter 4 (slides-multivar-first-order-4-weaknesses-curvature.tex):
  - Lines 73/81: "Ideally, perform large step along v_min but small step along v_max" — this is precisely what Newton's method does (it rescales steps by H⁻¹), but GD cannot.
  - Line 87: "GD is not aware of curvatures and can only walk along g" — the explicit statement of GD's blind spot, which 2nd order methods fix.
--> this is far away and students may not have this in mind anymore

BB comments (slides-multivar-second-order-1-newton-raphson.tex, lines 30–32 and 158ff.) raise the same issue from two angles:
- Lines 30–32: add a "demotivating" slide (or even a dedicated chunk) on why 2nd order methods are bad in high dimensions, with reference to large data and a speed comparison
- Lines 158ff.: at minimum, Problem 2 on the limitations slide should be more precise about the mechanism — not just "computationally expensive" but:
  - Storing H requires O(n²) memory, already intractable for large n
  - Computing the Newton direction means solving the linear system H·d = −∇f (not inverting H directly), which costs O(n³) even with stable solvers (Cholesky, CG) --> e.g., for NNs, where n = #params, this is already intractable for large networks
  - Cholesky/CG only work when H is positive definite, linking back to Problem 1

X 2 Motivation slides (instead of current first one)
X  - convergence speed of GD not so nice
X  - can use curvature information
/  - maybe one-dimensional Newton method (?)

X  Computational cost:
X  - big O statement (-> solve linear system)
X  - effort per iteration vs. convergence speed

/  state (or better show) convergence speed

X  divergence example (where we are far away from the optimum)

X  look into wright & recht -> does not have NR

Open points:
- connection to advanced first order methods, pre-conditioning perspective / comparison to them (also chapter order?)
- other problems of NR:
  - Hessian may not be invertible: singular/near-singular ∇²f ⇒ Newton step undefined or explodes.
    Happens for rank-deficient data (d > n, collinear features) and near degenerate optima. Not covered anywhere yet.
  - Indefinite Hessian, saddle-attraction half missing: non-descent-direction consequence is covered (Limitations slide +
    worked example), but not that Newton solves ∇f = 0 and is therefore attracted to ANY critical point incl. saddles —
    and saddles dominate high-dim non-convex landscapes ⇒ fatal for DNNs independent of cost.

From Aggarwal:
- maybe steal some motivation from 5.4.0
- use quadratic bowl intuition (under eq. 5.10)
- check if Fig. 5.7 is in there
- Bernd wants an animation where we can see the quadratic approximation, step, and next step etc. (preferably 2D (two input dimensions))
--> something like 5.8 but in two dimensions
--> one good and one bad example
- more on line search --> search in other literature
 --> minimum: show bad case animation with line search
- check whether we have the ascent/descent direction analysis/formula on the slides
- analyze divergence when start point is too far away from optimum
- 5.6.1 Singular and Indefinite Hessian --> we want to discuss this (also in context of descent direction and also that regularization helps, ill-conditioning should be mentioned)
- 5.6.2 sadle points:
  - show what happens in 2D with a visualization (different bowls in each direction)
  - important point: gradient descent can escape saddle points, but Newton cannot (because it is attracted to ANY critical point); note: we also need to discuss this better in the GD chapter (that it CAN escape)
- we want a more detailed analysis of convergence (maybe with proof) for Newton
- we could do trust regions but it is not enough stuff in Aggarwal (not understandable, not enough)


- potentially: one short slide on newton-raphson for least squares regression --> how hessian looks like, point-wise hessian (see lemma), result

- maybe we want to discuss regularization (definiteness, singularity, etc.) in general (that we do not have to repeat this always)


- in ch. 5.5.2 we can see that a abstract "reweighted least squares" view of Newton-Raphson is possible --> can we derive like a general scheme from that (abstract)? [maybe connect to the logistic regression example and fisher scoring]
  - see BB notes from Fable
  - discuss self-concordance for Newton
  - see BB's chat: newton_xtdx_pattern_lecture.md
  - see 5.5.4 in Aggarwal
  - not too much on logistic regression, we do this later

- check if we have an analysis of logistic regression (we should already have that in gradient descent (compute gradient, and hessian for convexity check)) --> if yes: derive diagonal matrix and put this in form derived in reweighted least squares chapter (+ use already derived gradient and hessian for logistic regression)


Re-order Chapters:
1. Classical First-order (gradient, stepsize, convergence)
2. second-order (better convergence, but expensive, esp. in ML regime where we have many parameters)
3. advanced first-order (momentum, Nesterov, Adam, etc. -> can be seen as approximating 2nd order information, but cheaper)

---------

# 2 -- Quasi-Newton methods

Generally rethink if this is a good didactic way to introduce quasi-Newton method (a lot of "open strings" at the beginning)
- not really clear why we want to do it that way
- better done here: https://www.stat.cmu.edu/~ryantibs/convexopt-F13/lectures/11-QuasiNewton.pdf
- currently somewhat repetitive but not really? maybe rethink how to design this slideset

Add implementations that use BFGS to show how relevant it is?

Sherman-Morrison formula on the SR1 slide (slides-multivar-second-order-2-quasi-newton.tex, line 104) appears wrong:
- Slide shows: $(\A + \beta \bm{u}\bm{u}^T)^{-1} = \A + \beta \frac{\bm{u}\bm{u}^T}{1 + \beta\bm{u}^T\bm{u}}$
- Correct formula: $(\A + \beta \bm{u}\bm{u}^T)^{-1} = \A^{-1} - \beta \frac{\A^{-1}\bm{u}\bm{u}^T\A^{-1}}{1 + \beta\bm{u}^T\A^{-1}\bm{u}}$
- RHS has $\A$ where it should have $\A^{-1}$ — needs verification before fixing
- BB flagged this too (comment at line 107)

add motivation slide
- that we want to have a p.d. matrix
- ideally: less expensive than computing H (computationally cheaper update schemes to construct A)

generally: try to make it shorter / more concise

check: are A and B always p.d.?

SR1
BFGS
L-BFGS
maybe DFP (Davidon-Fletcher-Powell) as well?

generally: say for which problems these methods are useful
- 2nd order steps in gradient boosting
- GP optimization

---------

# General

Step-size control in second order methods:
The Hessian already encodes curvature, providing a natural per-direction scaling.
Step size control (damping) is mainly needed for globalization — handling the early iterates where H may not be positive definite — not for the core convergence behavior.