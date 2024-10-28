library(lintr)

x = lint(
  text = "
  f = function() {}

  foo = function() { 
    f()
  }
  ",
  linters = object_usage_linter()
)
print(x)