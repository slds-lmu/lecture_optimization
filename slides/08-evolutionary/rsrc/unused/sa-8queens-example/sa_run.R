library(BBmisc)
source("board.R") 
source("objective.R") 
source("sa.R") 

set.seed(5)
b1 = createRandomBoard(8)
printBoard(b1)
z = sa(b1, maxit = 2000, t.factor = 0.8, t.start = 100)
b2 = z$b
printBoard(b1)
print(objective(b1))
catf("")
printBoard(b2)
print(objective(b2))




