## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cayleyR)

## -----------------------------------------------------------------------------
g <- cube_group(3)
set.seed(1)
w <- generate_walk(g, n = 3, n_moves = 6)
w$moves[[1]]
w$solution[[1]]

# the solution really does solve the state
identical(group_apply(g, w$states[1, ], w$solution[[1]]), group_identity(g))

## -----------------------------------------------------------------------------
generate_walk(g, n = 8, n_moves = 10, exact = FALSE)$depth

## ----eval = FALSE-------------------------------------------------------------
# net <- cube_adi_model(g, arch = "resnet", encoding = "piece")
# net

## ----eval = FALSE-------------------------------------------------------------
# net <- cube_adi_train(net, iterations = 200, batch_states = 10000,
#                       max_depth = 20)
# tail(net$history)

## ----eval = FALSE-------------------------------------------------------------
# cube_adi_save(net, "adi_cube3")
# 
# net <- cube_adi_load("adi_cube3", backend = "cpu")
# net <- cube_adi_train(net, iterations = 100)   # carries on training
# cube_adi_save(net, "adi_cube3")

## ----eval = FALSE-------------------------------------------------------------
# set.seed(7)
# s <- generate_walk(g, n = 1, n_moves = 15)$states[1, ]
# 
# greedy <- cube_adi_solve(net, s, budget = 200)
# greedy$solved
# length(greedy$path)
# 
# astar <- cube_adi_astar(net, s, weight = 0.6, batch = 1000)
# astar$solved
# astar$length
# astar$nodes      # states expanded

## ----eval = FALSE-------------------------------------------------------------
# depths <- 4:16
# rate <- sapply(depths, function(d) {
#   w <- generate_walk(g, n = 50, n_moves = d, no_cancel = TRUE)
#   mean(apply(w$states, 1, function(s) cube_adi_solve(net, s)$solved))
# })
# data.frame(depth = depths, solved = rate)

