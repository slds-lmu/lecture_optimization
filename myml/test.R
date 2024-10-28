library(mlr3)
library(ggplot2)
library(mlr3learners)
library(devtools)
library(mlbench)
library(BBmisc)

load_all(reset = TRUE)

set.seed(1234)
options(width = 160)

# n = 3
# X = as.matrix(iris[1:n, 1:2])
# y = iris[1:n, 3]


# dd = as.data.frame(X)
# dd$y = y
# mm = lm(y ~ ., data = dd)
# beta0 = coef(mm)
# print(beta0)

# beta1 = mylm3_c(X, y)
# print(beta1)


n = 200
z = gen_data_classif(n = n)

pl =  ggplot(z$df, aes(x = x.1, y = x.2, col = y))
pl = pl + geom_point(size = 20)
print(pl)

task = TaskClassif$new(id = "foo", backend = z$df, 
  target = "y", positive = "1")
lrn = lrn("classif.log_reg", predict_type = "prob")
m1 = lrn$train(task)
m2 = lrn$model
beta0 = coef(m2)
p = lrn$predict(task)
ms = msr("classif.logloss")
loss0 = p$score(ms)


beta1 = mylogreg_r_gd(z$X, z$y, max_iter = 50, stepsize = 0.01)




# mm = mylogreg_1(z$X, z$y, max_iter = 1e4, stepsize = 0.01  * n)
# #beta1 = mylogreg_r_gd(z$X, z$y, max_iter = 50, stepsize = 0.01)
# beta1 = mm$theta
# prob1 = z$X0 %*% beta1
# prob1 = 1 / (1 + exp(-prob1))
# prob1 = cbind(prob1, 1-prob1)
# colnames(prob1) = c("1", "0")
# loss1 = ms$fun(truth = z$df$y, prob = prob1)
# cat(sprintf("loss0 = %g;   loss1 = %g \n", loss0, loss1))
# met = calc_metrics(beta1, z, task)
# print(met)
# print(mm)
# #print(beta1)
# #print(beta0)



