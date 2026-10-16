## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(cayleyR)

## -----------------------------------------------------------------------------
tg <- topspin_group(8, k = 4, moves = c("L", "R", "X"))
tg

cg <- cube_group(3)
cg

## -----------------------------------------------------------------------------
group_apply(tg, group_identity(tg), "L L X")

s <- group_apply(cg, group_identity(cg), "R U R' U'")
sum(s != group_identity(cg))   # stickers out of place

## -----------------------------------------------------------------------------
undo <- group_inverse_seq(cg, "R U R' U'")
undo
identical(group_apply(cg, s, undo), group_identity(cg))

## -----------------------------------------------------------------------------
group_order(cg, "R")            # a quarter turn: 4
group_order(cg, "R U R' U'")    # the "sexy move": 6
group_order(cg, "R U")          # 105

group_order(tg, "L")            # going once round the ring

## -----------------------------------------------------------------------------
toy <- perm_group(list(
  a    = c(2L, 3L, 4L, 5L, 1L),
  "a'" = c(5L, 1L, 2L, 3L, 4L),
  b    = c(2L, 1L, 3L, 4L, 5L)
), name = "toy")
toy

group_apply(toy, 1:5, "a a b")
group_inverse_seq(toy, "a a b")
group_order(toy, "a b")

## -----------------------------------------------------------------------------
scrambled <- generate_state(group = cg, n_moves = 20)
length(scrambled)

## -----------------------------------------------------------------------------
m_only <- cube_group(3, moves = c("M", "M'"))
m_only
generate_state(group = m_only, n_moves = 5)

## -----------------------------------------------------------------------------
generate_state(10, k = 4, n_moves = 100)

