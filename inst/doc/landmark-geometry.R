## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(cayleyR)

## -----------------------------------------------------------------------------
lm <- landmark_states(20)
head(lm[, c("id", "name", "state_str")], 5)

## ----eval = FALSE-------------------------------------------------------------
# p <- human_algorithm_to(from, to, k = 4)
# p <- short_path_bfs(p$path, from, k = 4, depth = 5)$path

## -----------------------------------------------------------------------------
lm20 <- landmark_states(20)
i <- match("block_reverse_pairs", lm20$name)
p <- human_algorithm_to(seq_len(20), lm20$state[[i]], k = 4)
ops <- c("1" = "L", "2" = "R", "3" = "X")[p$path]
table(ops)

## -----------------------------------------------------------------------------
cube <- as.matrix(expand.grid(c(0, 1), c(0, 1), c(0, 1)))
convex_hull_3d(cube)[c("area", "volume")]

# the same cube with a point at its centre
withcentre <- rbind(cube, c(0.5, 0.5, 0.5))
h <- enclosing_hull_3d(withcentre)
c(vertices = length(h$vertices), area = h$area, volume = h$volume)

## ----eval = FALSE-------------------------------------------------------------
# # the network between landmarks, and the solid they span
# source(system.file("examples", "demo_landmark_network.R", package = "cayleyR"))
# 
# # the star of paths from the identity out to each landmark
# source(system.file("examples", "demo_landmark_paths.R", package = "cayleyR"))

