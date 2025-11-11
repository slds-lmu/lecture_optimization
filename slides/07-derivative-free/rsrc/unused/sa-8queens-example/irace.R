n = 6L
nrep = 3L
maxit = 5000L

parameters.table = '
op "" c (1step, geom) 
t.start "" r (1, 10)
t.factor "" r (0.7, 1)
'
parameters = readParameters(text = parameters.table)

instances = replicate(100L, createRandomBoard(n), simplify = FALSE)

hook.run = function(experiment, config = list()) {
  cd = experiment$candidate
  z = sa(experiment$instance, maxit = maxit, op = cd$op, 
    t.start = cd$t.start, t.factor = cd$t.factor)
  ifelse(z$penalty == 0, length(z$penalties), 10*maxit)
}

tc = list(
  hookRun = hook.run,
  instances = instances,
  maxExperiments = 1000,
  logFile = "irace.RData"
)

res = irace(tunerConfig = tc, parameters = parameters)
print(res)
rr = load2("irace.RData")

