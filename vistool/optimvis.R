library(R6)
library(rootSolve) # fun: gradient, hessian 
library(checkmate)
library(data.table)
library(colorspace)
options(warn = 2)

# question: do we enforce that x is always 2d? 
# currently not, as we maybe want to benchmark in some d, then show at least 
# y-traces. restriction seems to have no value, currently

Optimizer = R6Class("Optimizer",
  public = list(
    step_size = NULL,
    steps = NULL,
    x0 = NULL,
    
    initialize = function(steps, step_size, x0) {
      steps = asInt(steps, lower = 0)
      assert_number(step_size, lower = 0)      
      x0 = assert_numeric(x0, len = 2L) 
      self$steps = steps
      self$step_size = step_size
      self$x0 = x0
    }
  )
)

OptimizerGD = R6Class("OptimizerGD", inherit = Optimizer,
  public = list(
    initialize = function(steps, step_size, x0) {
      super$initialize(steps, step_size, x0)  # assert in super
    },
    
    optimize = function(obj) {
      assert_r6(obj, "Objective")
      x_old = self$x0
      for (step in 1:self$steps) {
        d = obj$grad(x_old)
        dn = norm(d, type = "2")
        x_new = x_old - self$step_size * d / dn
        y_new = obj$eval_store(x_new)
        x_old = x_new
      }
    }
  )
)

# Newton raphson
OptimizerNR = R6Class("OptimizerNR", inherit = Optimizer,
  public = list(
    initialize = function(steps, step_size, x0) {
      super$initialize(steps, step_size, x0)  # assert in super
    },
    
    optimize = function(obj) {
      assert_r6(obj, "Objective")
      # implement NR algorithm here
      x_old = self$x0
      for (step in 1:self$steps) {
        d = t(solve(obj$hess(x_old)) %*% obj$grad(x_old))
        dn = norm(d, type = "2")
        x_new = x_old - self$step_size * d / dn
        y_new = obj$eval_store(x_new)
        x_old = x_new
      }
    }
  )
)



Objective = R6Class("Objective",
  public = list(
    id = NULL,
    fun = NULL,
    # store evals as (x, fval)
    # x = listcol, unclear whether thats best, we can always add an unwrapper
    archive = NULL,
    xdim = NULL,
  
    initialize = function(id = "f", fun, xdim = 2) {
      self$id = assert_string(id)
      self$fun = assert_function(fun) 
      testx = rep(0, xdim)
      assert_number(self$fun(testx)) # check that fun works as expected
      self$archive = data.table()
      self$xdim = xdim
    },
    
    #FIXME: wir sollten hier auch immer den grad und dessen norm immer speichern
    
    eval_store = function(x) {
      assert_numeric(x, len = self$xdim)
      fval = self$fun(x)
      self$archive = rbind(self$archive, list(x = list(x), fval = fval))
      return(fval)
    },
    
    grad = function(x) {
      rootSolve::gradient(self$fun, x)[1,] 
    },
    
    hess = function(x) {
      rootSolve::hessian(f=self$fun, x)
    }
  )
)


Visualizer = R6Class("Visualizer", 
  public = list(
    obj = NULL,
    run_archs = NULL, 
    x1lab = NULL,
    x2lab = NULL,
    flab = NULL,
    n_grid = NULL,
    x1lim = NULL,
    x2lim = NULL,
    
    initialize = function(obj, run_archs = NULL, x1lim = c(0, 1), x2lim = c(0, 1), 
      n_grid = 50L, x1lab = "x1", x2lab = "x2", flab = "f", col = NULL) {
      
      self$obj = obj
      if (is.data.table(run_archs))
        run_archs = list(run_archs)
      self$run_archs = assert_list(run_archs, types = "data.table")
      self$run_archs = run_archs 
      self$x1lim = assert_numeric(x1lim, len = self$xdim)
      self$x2lim = assert_numeric(x2lim, len = self$xdim)
      self$n_grid = asInt(n_grid, lower = 2)
      self$x1lab = assert_string(x1lab)
      self$x2lab = assert_string(x2lab)
      self$flab = assert_string(flab)
      self$bg_2d_col = terrain_hcl
    },

    get_grid = function() { 
      x1seq = seq(self$x1lim[1], self$x1lim[2], length = self$n_grid)
      x2seq = seq(self$x2lim[1], self$x2lim[2], length = self$n_grid)
      g = expand.grid(x1seq, x2seq) # changes var1 first
      fmat = apply(g, 1, self$obj$fun)
      # orders by col in result mat; this means: rows = x1, cols = x2
      fmat = matrix(fmat, self$n_grid, self$n_grid) 
      list(x1seq = x1seq, x2seq = x2seq, fmat = fmat)
    },
    
    plot_rbase_contour = function(nlevels = 15) {
      # FIXME: reduce margins in plot?
      g = self$get_grid()

      #par(mar = c(4.1, 4.1, 1.1, 1.1))
      image(g$x1seq, g$x2seq, g$fmat, col = self$bg_2d_col, xlab = self$x1lab, ylab = self$x2lab) 
      contour(g$x1seq, g$x2seq, g$fmat, nlevels = nlevels, add = TRUE) 
      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        for (j in 1:nrow(a)) {
          p = a$x[[j]]
          if (j > 1) {
            q = a$x[[j-1]]
            lines(c(p[1], q[1]), c(p[2], q[2]))
            points(q[1], q[2], pch = 16, col = "black")
          }
          points(p[1], p[2], pch = 16, col = "black")
        }
      }
    },
    
    
    # label z axis
    plot_rbase_3dsurf = function(theta = 40, phi = 40, ticktype = "detailed", 
      pers_lwd = 0.5, run_lwd = 3) {
      
      # FIXME: reduce margins in plot?
      g = self$get_grid()
      #par(mar = c(4.1, 4.1, 1.1, 1.1))
      m = nrow(g$fmat); n = ncol(g$fmat)
      zfacet = g$fmat[-1, -1] + g$fmat[-1, -n] + g$fmat[-m, -1] + g$fmat[-m, -n] # FIXME what happens here?
      # FIXME: check what happens here with col and doc
      facetcol = cut(zfacet, length(self$bg_2d_col))
      pmat = persp(g$x1seq, g$x2seq, g$fmat, col = self$bg_2d_col[facetcol], theta = theta, phi = phi, 
        ticktype = ticktype, xlab = self$x1lab, ylab = self$x2lab, zlab = self$flab, lwd = pers_lwd)

      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        for (j in 1:nrow(a)) {
          p = a$x[[j]]
          pf = a$fval[j]
          t3d1 = trans3d(p[1], p[2], pf, pmat) 
          if(j > 1) {
            q = a$x[[j-1]]
            qf = a$fval[j-1]
            t3d2 = trans3d(q[1], q[2], qf, pmat)
            lines(c(t3d1$x, t3d2$x), c(t3d1$y, t3d2$y), lwd = run_lwd) 
            points(x = t3d2$x, y = t3d2$y, pch = 16, col = "black") 
          }
          points(x = t3d1$x, y = t3d1$y, pch = 16, col = "black")
        }
      }
    },
    
    
    # FIXME: y und gradnorm zeigen. nochwas?
    plot_y_trace = function() { 
      plot(1, type="n", xlab="Steps", ylab="y", 
           xlim=c(1,length(self$obj$archive$fval)), ylim=c(0,max(self$obj$archive$fval)))
      lines(self$obj$archive$fval, pch=16, col="blue")
      
    }
    
  ),
  
  active = list(
    bg_2d_col = function(col) {
      if (missing(col)) return(private$.bg_2d_col)
      if (is.function(col)) # create n^2 size colmap we can index later
        col = col(self$n_grid * self$n_grid) 
      private$.bg_2d_col = col
    }
  ),
  
  private = list(
    .bg_2d_col = NULL
  )
)



# f = function(x) { sum(x * x)}
# obj = Objective$new(fun = f)
# optim = OptimizerGD$new(steps = 5L, step_size = 0.1, x0 = c(10, 10))
# optim$optimize(obj)
# print(obj$archive)
# vis = Visualizer$new(obj, run_archs = list(obj$archive),
#   x1lim = c(-15, 15), x2lim = c(-15, 15))
# vis$plot_rbase_contour(2)
# vis$plot_rbase_3dsurf()



                    
