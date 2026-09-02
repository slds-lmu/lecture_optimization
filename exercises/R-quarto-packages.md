# R packages required for Quarto-rendered exercises

Install once per machine:

```r
install.packages(c(
  "ggplot2",
  "gridExtra",
  "RColorBrewer",
  "linprog",   # ex10 LP
  "lpSolve",   # ex10 LP backend
  "CVXR",      # ex11 nonlinear SVM
  "mlr3",      # ex11 moons data generator
  "DiceKriging", # ex13 BO surrogate model
  "knitr",
  "rmarkdown"
))
```

Notes:
- `mlr3::tgen("moons")` is used in ex11 to generate the moons dataset. The Python notebook loads the same data from `exercise-11-quarto/data/moons.csv` (committed) rather than regenerating, to avoid R-vs-sklearn `make_moons` parameter divergence.
- IRkernel registration (one-time, so Quarto can use R kernel inside notebooks):
  ```r
  install.packages("IRkernel")
  IRkernel::installspec(name = "ir", displayname = "R")
  ```
