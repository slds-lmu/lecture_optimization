library(R6)
library(rootSolve) # fun: gradient, hessian 
library(checkmate)
library(data.table)
library(colorspace)
library(mlr3misc) # nice helper stuff
library(TestFunctions)
options(warn = 2)


l2norm = function(x) sqrt(sum(x*x))

# question: do we enforce that x is always 2d? 
#   currently not, as we maybe want to benchmark in some d, then show at least 
#   y-traces. restriction seems to have no value, currently

Optimizer = R6Class("Optimizer",
  public = list(
    step_size = NULL,
    steps = NULL,
    x0 = NULL,
    
    initialize = function(steps, step_size, x0) {
      steps = asInt(steps, lower = 0)
      assert_number(step_size, lower = 0)      
      x0 = assert_numeric(x0, min.len = 1) 
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
    
    #FIXME: implement a generic step size control mechanism here.
    # constant, armijo, downschedule
    optimize = function(obj) {
      assert_r6(obj, "Objective")
      x_old = self$x0
      for (step in 1:self$steps) {
        ee = obj$eval_store(x_old)
        d = -ee$grad
        
        alpha = self$step_size
        while (TRUE)
          x_test = x_old + alpha * d
          obj$eval() > obj$eval(x_old) + self$gamma * alpha * (grad %*% t(d))[1]) {
          alpha = alpha * self$tau
        }
        
        x_new = x_old + self$step_size * d
        x_old = x_new
      }
    }
  )
)

OptimizerNR = R6Class("OptimizerNR", inherit = Optimizer,
  public = list(
    gamma = NULL,
    tau = NULL,
    initialize = function(steps, step_size, x0, gamma, tau) {
      super$initialize(steps, step_size, x0)  # assert in super
      self$gamma = gamma
      self$tau = tau 
    },
    
    optimize = function(obj) {
      assert_r6(obj, "Objective")
      x_old = self$x0

      for (step in 1:self$steps) {
        # FIXM: all bad here
        ee = obj$eval_store(x_old)
        hess = obj$hess(x_old)
        grad = obj$grad(x_old)
        # FIXME: transpose seems uneccassary? H symmetric?
        # FIXME: we can take hessian from eval_store
        d = t(solve(hess, grad))
        # d = d / l2norm(d)

        # Step size through Armijo rule
        alpha = self$step_size

        # while (obj$eval(x_old + alpha * d) > obj$eval(x_old) + self$gamma * alpha * (grad %*% t(d))[1]) {
        #   alpha = alpha * self$tau
        # }

        # print(alpha)

        # dn = l2norm(d)
        x_new = x_old - d #/ dn
        x_old = x_new
      }
    }
  )
)



Objective = R6Class("Objective",
  public = list(
    id = NULL,
    label = NULL,
    fun = NULL,
    # store evals as (x, fval)
    # x = listcol, unclear whether thats best, we can always add an unwrapper
    archive = NULL,
    xdim = NULL,
  
    initialize = function(id, fun, label = "f", xdim = 2) {
      self$id = assert_string(id)
      self$label = assert_string(label)
      self$fun = assert_function(fun) 
      xdim = asInt(xdim, na.ok = TRUE, lower = 1)
      # using testx=0 that might break if origin is outofbound? 
      # OTOH unlikely and we dont really consider bounds atm
      testx = rep(0, ifelse(is.na(xdim), 2, xdim)) # dim=2 if fun has arbitrary dim
      assert_number(self$fun(testx)) # check that fun works as expected
      self$archive = data.table()
      self$xdim = xdim
    },
    
    eval = function(x) {
      assert_numeric(x, len = self$xdim)
      fval = self$fun(x)
      return(fval)
    },
    
    eval_store = function(x) {
      assert_numeric(x, len = self$xdim)
      fval = self$fun(x)
      grad = rootSolve::gradient(self$fun, x)[1,]
      gnorm = l2norm(grad)
      self$archive = rbind(self$archive, list(x = list(x), fval = fval, 
        grad = list(grad), gnorm = gnorm))
      list(fval = fval, grad = grad, gnorm = gnorm)
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
    logscale = NULL,
    
    #FIXME: use base instead of flag for logscale?
    initialize = function(obj, run_archs = NULL, x1lim = c(0, 1), x2lim = c(0, 1), 
      n_grid = 50L, x1lab = "x1", x2lab = "x2", flab = "f", col = NULL, logscale = FALSE) {
      
      # FIXME: col arg in signature not used? typo?
      self$obj = obj
      assert_r6(obj, "Objective")
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
      self$logscale = assert_flag(logscale)
    },

    get_grid = function() { 
      x1seq = seq(self$x1lim[1], self$x1lim[2], length = self$n_grid)
      x2seq = seq(self$x2lim[1], self$x2lim[2], length = self$n_grid)
      g = expand.grid(x1seq, x2seq) # changes var1 first
      fmat = apply(g, 1, self$obj$fun)
      # orders by col in result mat; this means: rows = x1, cols = x2
      fmat = matrix(fmat, self$n_grid, self$n_grid) 
      if (self$logscale)
        fmat = log(fmat)
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
            lines(c(p[1], q[1]), c(p[2], q[2]), col = i + 1)
            points(q[1], q[2], pch = 16, col = i + 1)
          }
          points(p[1], p[2], pch = 16, col = i + 1)
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
            lines(c(t3d1$x, t3d2$x), c(t3d1$y, t3d2$y), lwd = run_lwd, col = i + 1) 
            points(x = t3d2$x, y = t3d2$y, pch = 16, col = i + 1) 
          }
          points(x = t3d1$x, y = t3d1$y, pch = 16, col = i + 1)
        }
      }
    },
    
    
    # FIXME: y und gradnorm zeigen. nochwas?
    plot_y_trace = function() { 
      arch = self$run_archs[[1]]$fval
      plot(1, type="n", xlab="Steps", ylab="y", 
         xlim=c(1, length(arch)), ylim=c(0,max(arch)))
    
      for (i in 1:length(self$run_archs)) {
        a = self$run_archs[[i]]
        points(x = 1:length(a$fval), y = a$fval, pch = 16, col = i + 1)
        lines(a$fval, pch=16, col=i+1)
      }
      
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


###################### testfuns and dictionary

tfun_dict = R6Class("DictionaryObjective", inherit = Dictionary, 
  cloneable = FALSE)$new()

as.data.table.DictionaryObjective = function(x, ..., objects = FALSE) {

  setkeyv(map_dtr(x$keys(), function(key) {
    t = x$get(key)
    insert_named(
      list(key = key, label = t$label, xdim = t$xdim),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}

#FIXME: add all funs here
# FIXME: check that we can work with funs of arbitrary xdim
tfun_dict$add("TF_branin", Objective$new(fun = TestFunctions::branin,                   
  id = "TF_branin", label = "branin", xdim = 2L))
tfun_dict$add("TF_banana", Objective$new(fun = TestFunctions::banana,                   
  id = "TF_banana", label = "banana",xdim = 2L))
tfun_dict$add("TF_gaussian1", Objective$new(fun = TestFunctions::gaussian1,                   
  id = "TF_gaussian1", label = "gaussian1", xdim = NA))

print(as.data.table(tfun_dict))

tf = tfun_dict$get("TF_branin")
oo = OptimizerGD$new(steps = 10, step_size = 0.0001, x0=rep(1, 2))
oo$optimize(tf)
a = tf$archive
print(a)
v = Visualizer$new(tf, run_archs = a)
v$x1lim = c(-0, 2)
v$x2lim = c(-0, 2)
v$logscale = TRUE
v$plot_rbase_contour() 


