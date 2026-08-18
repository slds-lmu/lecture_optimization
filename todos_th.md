# 04-3 -- GD Deep Dive

- compare our GD convergence analysis with the one in Boyd (page 468) [they do backtracking line search and exact line search, what do we for step size]

# 05-1 -- Newton-Raphson

X 2 Motivation slides (instead of current first one)
X  - convergence speed of GD not so nice
X  - can use curvature information
/  - maybe one-dimensional Newton method (?)  [implicitly included with an example in 1D]

X  Computational cost:
X  - big O statement (-> solve linear system)
X  - effort per iteration vs. convergence speed

X  divergence example (where we are far away from the optimum)

From Aggarwal:
X maybe steal some motivation from 5.4.0
X use quadratic bowl intuition (under eq. 5.10)
X check if Fig. 5.7 is in there
X Bernd wants an animation where we can see the quadratic approximation, step, and next step etc. (preferably 2D (two input dimensions))
  --> something like 5.8 but in two dimensions
  --> one good and one bad example
X more on line search --> search in other literature
  --> minimum: show bad case animation with line search
X check whether we have the ascent/descent direction analysis/formula on the slides
X 5.6.1 Singular and Indefinite Hessian --> we want to discuss this (also in context of descent direction and also that regularization helps, ill-conditioning should be mentioned)
X 5.6.2 saddle points:
  - show what happens in 2D with a visualization (different bowls in each direction)
  - important point: gradient descent can escape saddle points, but Newton cannot (because it is attracted to ANY critical point); note: we also need to discuss this better in the GD chapter (that it CAN escape)

- analyze divergence when start point is too far away from optimum
- include as a slide: affine invariance of the Newton step --> boyd 478
- include as a deep dive slide set: convergence analysis of Newton-Raphson (Boyd & Vandenberghe §9.5.3)
  - first: read through the proof and see if we can understand it and explain it

X in ch. 5.5.2 we can see that a abstract "reweighted least squares" view of Newton-Raphson is possible --> can we derive like a general scheme from that (abstract)? [maybe connect to the logistic regression example and fisher scoring]
  - see BB notes from Fable
  - discuss self-concordance for Newton
  - see BB's chat: newton_xtdx_pattern_lecture.md
  - see 5.5.4 in Aggarwal
  - not too much on logistic regression, we do this later

- potentially: one short slide on newton-raphson for least squares regression --> how hessian looks like, point-wise hessian (see lemma), result
- maybe we want to discuss regularization (definiteness, singularity, etc.) in general (that we do not have to repeat this always)
- check if we have an analysis of logistic regression (we should already have that in gradient descent (compute gradient, and hessian for convexity check)) --> if yes: derive diagonal matrix and put this in form derived in reweighted least squares chapter (+ use already derived gradient and hessian for logistic regression)

- implementations in R and Python of the optimization methods (like optim in R)
- torch and jax? with autodiff implementation and stuff --> short optimization example with torch

Re-order Chapters:
1. Classical First-order (gradient, stepsize, convergence)
2. second-order (better convergence, but expensive, esp. in ML regime where we have many parameters)
3. advanced first-order (momentum, Nesterov, Adam, etc. -> can be seen as approximating 2nd order information, but cheaper)

- connection of second order methods to advanced first order methods, pre-conditioning perspective / comparison to them (also chapter order?)


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
-------------------
