## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cayleyR)

## -----------------------------------------------------------------------------
cg4 <- cube_group(4)
cg4

## -----------------------------------------------------------------------------
cube_is_reduced(cube_identity(4))
cube_is_reduced(group_apply(cg4, cube_identity(4), "U"))
cube_is_reduced(group_apply(cg4, cube_identity(4), "2x"))

## -----------------------------------------------------------------------------
set.seed(42)
scramble <- generate_state(group = cg4, n_moves = 40)

res <- cube_solve4(scramble)
res$found
length(res$path)
res$stages

## -----------------------------------------------------------------------------
res_k <- cube_solve4(scramble, method = "kociemba")
length(res_k$path)
cube_is_colour_solved(group_apply(cg4, scramble, res_k$path))

## -----------------------------------------------------------------------------
set.seed(1)
short <- generate_state(group = cg4, n_moves = 5)

res4 <- cube_kociemba4(short)
res4$found
length(res4$reduction)   # moves to the reduced cube
length(res4$cube3)       # moves to finish it as a 3x3x3

## ----eval = FALSE-------------------------------------------------------------
# r <- cube_solve4_cascade(scramble)
# r$method      # "reduce" if the search won, "solve4" if it fell back
# r$n_moves
# r$ops         # every step it took, with the time spent on each

## -----------------------------------------------------------------------------
cube_santa_path("f0.-d3.r1", n = 4)
cube_santa_path_out(c("F", "U", "2x"), n = 4)

## -----------------------------------------------------------------------------
substr(cube_santa_path_out(res_k$path, n = 4), 1, 60)

