# iterate over all queens and check conflicts
# if a queen threatens another one, +1 penalty
# if a queen is on top of another, +n penaly
objective = function(b) {
  n = b$n
  penalty = 0
  # check all ordered pairs of quuens (yes, we then count penalties twice)
  for (i in 1:n) {
    for (j in setdiff(1:n, i)) {
      x1 = b$xs[i]; y1 = b$ys[i]
      x2 = b$xs[j]; y2 = b$ys[j]
      if (x1 == x2 || y1 == y2 || x1 + y1 == x2 + y2 || x1 - y1 == x2 - y2) {
        penalty = penalty + 1
      }
      if (x1 == x2 && y1 == y2)
        penalty = penalty + n
    }
  }
  return(penalty)
}


