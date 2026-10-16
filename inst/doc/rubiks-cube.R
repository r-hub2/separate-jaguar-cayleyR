## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cayleyR)

## -----------------------------------------------------------------------------
cg <- cube_group(3)
cg

## -----------------------------------------------------------------------------
s <- group_apply(cg, cube_identity(3), "R U U F'")
s[1:9]                 # the U face: which sticker sits in each place

## -----------------------------------------------------------------------------
face_U <- function(state) matrix(cube_colours(state)[1:9], 3, byrow = TRUE)
face_U(cube_identity(3))
face_U(group_apply(cg, cube_identity(3), "R"))

## -----------------------------------------------------------------------------
s <- group_apply(cg, cube_identity(3), "R M' L'")
cube_is_colour_solved(s)
identical(s, cube_identity(3))

## -----------------------------------------------------------------------------
set.seed(42)
scramble <- generate_state(group = cg, n_moves = 25)
face_U(scramble)

## -----------------------------------------------------------------------------
solvers <- list(
  lbl           = cube_solve_lbl,
  cfop          = cube_solve_cfop,
  old_pochmann  = cube_solve_old_pochmann,
  m2            = cube_solve_m2,
  kociemba      = cube_kociemba
)

res <- lapply(solvers, function(f) f(scramble))

data.frame(
  found  = sapply(res, `[[`, "found"),
  moves  = sapply(res, function(r) length(r$path))
)

## -----------------------------------------------------------------------------
sapply(res, function(r) cube_is_colour_solved(group_apply(cg, scramble, r$path)))

## -----------------------------------------------------------------------------
res$cfop$stages

## -----------------------------------------------------------------------------
cube_kociemba_report()

## -----------------------------------------------------------------------------
cube_move_names(2)
cube_move_names(4)

