
library(rlang)

# just like do.call
invoke(paste, letters)

# cleaner calls
invoke(call_inspect, mtcars)
invoke(call_inspect, letters)

# and stacktraces look better
fn <- function(...) sys.calls()
invoke(fn, list(mtcars))

# compared to do.call()
do.call(call_inspect, mtcars)
do.call(fn, list(mtcars))

# specify the function name by supplying just a string
# id the function (it has to be visible in .env)
invoke("call_inspect", letters)
invoke("paste", letters)
invoke(paste, letters)

# use the .bury argument to change the argument prefix
invoke(call_inspect, mtcars, blahblah = c("inspect!", "col"))
invoke(call_inspect, mtcars)

# holy grail
# Within purr you can do map-reduce types of invoke calls


# invoke a function with a list of arguments
# I see.
invoke(runif, list(n=10))
list(n=10)
runif(10)

# invoke a function with named arguments
invoke(runif, n=10)


# combine the two
invoke(paste, list("01a", "01b"), sep = "-")

# pipe the same process
list("01a", "01b") %>%
  invoke(paste, ., sep = "-")

# invoke a list of functions, each with different arguments
invoke_map(list(runif, rnorm), list(list(n = 10), list(n = 5)))

# invoke with the same inputs
invoke_map(list(runif, rnorm), list(list(n=5)))

invoke_map(list(runif, rnorm), n=5)

runif(rnorm(5))

# or the same function with differfent inputs
invoke_map("runif", list(list(n=5), list(n=10)))

# or as a pipeline
# list type
list(m1 = mean, m2 = median) %>% invoke_map(x = rcauchy(100))
# character type
list(m1 = mean, m2 = median) %>% invoke_map_dbl(x = rcauchy(100))


# the position can be matched explicitly by omitting `.x`
# helpful when argument names of functions are not identical
list(m1 = mean, m2 = median) %>%
  invoke_map(, rcauchy(100))



