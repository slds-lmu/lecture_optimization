We want to modernize our constrained optimization chapter. The new version (06-constrained-NEW) will be based on the textbook Linear Algebra and Optimization for Machine Learning. Specifically, we are converting the following subchapters:

- 6.2 Primal Gradient Descent Methods as the first section.
- 6.3 Primal Coordinate Descent as the second section.
- 6.4 Lagrangian Relaxation and Duality as the third section.
- 6.5 Penalty-Based and Primal-Dual Methods as the fourth section.
- 6.6 Norm-Constrained Optimization as the fifth section.
- 6.7 Primal Versus Dual Methods as the sixth section.

Each section should have about 10 slides (of course, some a little more, some a little less depending on necessary depth). As the book has few visualizations, please also add fitting, modern ones using R (ggplot2, wherever helpful to ease understanding). We work subchapter-by-subchapter. As context, the new constrained chapter of our Optimization course will cover:
* What a constrained optimization problem is: optimization over a feasible region defined by constraints.
* Why standard unconstrained gradient descent can fail under constraints: an update step can leave the feasible region.
* The two main ways to handle constraints:
  * Primal methods: modify optimization algorithms so iterates stay feasible.
  * Dual methods: use Lagrangian relaxation to convert constraints into dual variables and solve the resulting dual problem.
* That common unconstrained methods such as gradient descent, coordinate descent, and Newton's method can be adapted to constrained settings.
* That constrained problems often depend strongly on the structure of the constraints.
* Two important classes of constraints:
  * Linear and convex constraints
  * Norm constraints, e.g. optimizing 
* The main methods covered:
  * Projected gradient descent
  * Constrained coordinate descent
  * Lagrangian relaxation
  * Penalty methods
  * Barrier methods
* That penalty and barrier methods combine ideas from both primal and dual formulations.
* A comparison of primal vs. dual methods:
  * Primal methods tend to be more interpretable.
  * Dual methods can be advantageous when the number of points is smaller than the number of variables.

When working on the slides, make sure to follow the general conventions of our lecture (e.g., take a look at the old version of our chapter, 06-constrained), i.e., use the custom beamer macros (framev, framei, splitV, etc.) and our LaTeX math macros. This can be an iterative process, the main thing to get right is the content of the slides! We want to stay close to what is covered in the book (see screenshots).

Please work on slides-constrained-6-norm-constrained.tex
