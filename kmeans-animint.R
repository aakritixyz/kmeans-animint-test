library(animint2)
library(ggplot2)
library(data.table)

set.seed(1)

# ----------------------------
# Generate data
# ----------------------------
points <- data.table(
  x = c(rnorm(50, 0), rnorm(50, 3)),
  y = c(rnorm(50, 0), rnorm(50, 3))
)

k <- 2
centers_mat <- as.matrix(points[sample(.N, k), .(x, y)])
max_iter <- 6

points_list    <- list()
centers_list   <- list()
objective_list <- list()

# ----------------------------
# K-means iterations
# ----------------------------
for (iter in 1:max_iter) {
  dist_mat <- as.matrix(dist(rbind(points[, .(x, y)], centers_mat)))
  dist_mat <- dist_mat[1:nrow(points), (nrow(points)+1):(nrow(points)+k)]
  cluster <- apply(dist_mat, 1, which.min)
  
  points_iter <- copy(points)
  points_iter[, `:=`(cluster = factor(cluster), iteration = iter)]
  
  centers_df <- points_iter[, .(x = mean(x), y = mean(y)), by = cluster]
  centers_df[, iteration := iter]
  
  obj <- sum((points_iter$x - centers_df$x[as.integer(cluster)])^2 +
               (points_iter$y - centers_df$y[as.integer(cluster)])^2)
  
  objective_list[[iter]] <- data.table(iteration = iter, value = obj)
  points_list[[iter]]  <- points_iter
  centers_list[[iter]] <- centers_df
  
  centers_mat <- as.matrix(centers_df[, .(x, y)])
}

points_df    <- rbindlist(points_list)
centers_df   <- rbindlist(centers_list)
objective_df <- rbindlist(objective_list)

# ----------------------------
# Scatter plot
# ----------------------------
scatter <- ggplot() +
  theme_bw() +
  geom_point(
    aes(x, y, color = cluster),
    data = points_df,
    showSelected = "iteration",
    size = 3
  ) +
  geom_point(
    aes(x, y),
    data = centers_df,
    showSelected = "iteration",
    shape = 21,
    size = 7,
    fill = "white",
    stroke = 2
  )

# ----------------------------
# Objective plot
# ----------------------------
objective_plot <- ggplot() +
  theme_bw() +
  geom_line(aes(iteration, value), data = objective_df, size = 1) +
  geom_tallrect(
    aes(xmin = iteration - 0.5, xmax = iteration + 0.5),
    data = objective_df,
    clickSelects = "iteration",
    alpha = 0.3
  )

# ----------------------------
# Animint object
# ----------------------------
viz <- animint(
  scatter = scatter,
  objective = objective_plot,
  time = list(variable = "iteration", ms = 1500),
  first = list(iteration = 1)
)

# ----------------------------
# Save to Desktop
# ----------------------------
unlink("~/Desktop/kmeans-animint-test", recursive = TRUE) # remove folder if exists
animint2dir(viz, out.dir = "~/Desktop/kmeans-animint-test", open.browser = TRUE)