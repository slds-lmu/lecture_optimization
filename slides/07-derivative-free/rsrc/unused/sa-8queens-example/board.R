# create a random chessboard of size n x n
# represent board as vectors of x and y coords
# place queens on random coords, we even allow clashes where multiple queens are on the same tile
createRandomBoard = function(n) {
  xs = integer(n)
  ys = integer(n)
  for (i in 1:n) {
    xs[i] = sample(n, 1)
    ys[i] = sample(n, 1)
  }
  list(n = n, xs = xs, ys = ys)
}

# print board on console so we can understand it
printBoard = function(b) {
  n = b$n
  f = matrix(".", n,n)
  for (i in 1:n) { 
    f[b$xs[i], b$ys[i]] = i
  }
  for (i in 1:n) { 
    for (j in 1:n) { 
      cat(f[i,j])
    }
    cat("\n")  
  }
}

