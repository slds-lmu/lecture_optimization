
# do a local perturbation of the board, for SA, return perturbed board
#
# select random queen, then:
# method "1step": move queen 1 random step to on the board (8 dirs, or even stay)
# method: "geom": sample movement in x and y by adding geometric distribution: G(0.5) - G(0.5)
# at end: if we have left the board --> clip to board boundary
getPerturbedBoard = function(b, op = "geom") {
  # select random queen
  i = sample(b$n, 1L)
  x = b$xs[i]
  y = b$ys[i]
  # option a) move 1 random step
  if (op == "1step") {
    x = x + sample(c(-1,0,1), 1L)
    y = y + sample(c(-1,0,1), 1L)
  }
  # option b) add geometric distruted steps
  if (op == "geom") {
    h = function() rgeom(1, prob = 0.5) - rgeom(1, prob = 0.5)   
    x = x + h()
    y = y + h()
  } 
  # clip to board and write x,y back to board 
  x = min(max(x, 1), b$n)
  y = min(max(y, 1), b$n)
  b$xs[i] = x
  b$ys[i] = y
  return(b)
}

# simulated annealing for a starting board
# b: board of queens
# maxit: number of SA iterations
# op: operator for local perturbation, see getPerturbedBoard
# t.start: start temperature
# t.factor: used in cooling T <- t.factor * T 
sa = function(b, maxit = 3L, op = "geom", t.start = 100, t.keep = 50L, t.factor = 0.8) {
  # init stuff
  penalty = objective(b)
  penalties = penalty # archive for all evals
  temp = t.start
  iter = 1L
  # iterate if solution is still suboptimal and budget not depleted
  while(penalty > 0 && iter <= maxit) {
    # local perturbation, evaluate it, store the eval 
    b.new = getPerturbedBoard(b, op = op)  
    p.new = objective(b.new)
    penalties = c(penalties,p.new)
    # compute delta diff d, and prob to accept
    d = penalty - p.new 
    prob = exp(d / temp)
    u = runif(1)
    messagef("iter = %4i; cur = %4i,  new = %4i,  prob = %.4f,  t = %g", 
      iter, penalty, p.new, ifelse(d < 0, prob, 0) , temp)
    # accept if we are better, or randomly if we are worse
    if (d > 0 || u < prob) {
      b = b.new
      penalty = p.new
    }

    # decrease temperature
    if (iter %% t.keep == 0)
      temp = temp * t.factor
    iter = iter + 1L
  }
  list(b = b, penalty = penalty, penalties = penalties)
}

