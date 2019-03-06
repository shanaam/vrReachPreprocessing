library(ggplot2)

# Not sure about processing time in R vs Python

# load file
rawReachDF <- read.csv('data/visible_plane - A/1/S001/example_object_movement_T001.csv', header = TRUE)

p <- ggplot(rawReachDF, aes(pos_x, pos_z)) +
  geom_point() +
  scale_x_continuous(limits = c(-0.10, 0.10)) +
  scale_y_continuous(limits = c(0.15, 0.35))

p

# get a "distance from start position" measure for each point (some apply method)
