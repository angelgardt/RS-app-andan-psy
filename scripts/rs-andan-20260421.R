pkgs <- c("GGally", "fpc", "factoextra", "cluster")
install.packages(pkgs[!pkgs %in% installed.packages()])

library(tidyverse)
theme_set(theme_bw())
theme_update(legend.position = "bottom")
library(factoextra)
library(cluster)

iris
unique(iris$Species)

pairs(iris[, 1:4], col = iris[, 5])
GGally::ggpairs(iris, columns = 1:4, aes(color = Species))

d <- dist(iris[1:4], method = "eucl")

hc_complete <- hclust(d, method = "complete")
plot(hc_complete)

hc_complete_cl2 <- cutree(hc_complete, k = 2)

GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(hc_complete_cl2)),
                upper = NULL, diag = NULL)
table(iris$Species, hc_complete_cl2)

hc_complete_cl3 <- cutree(hc_complete, k = 3)

GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(hc_complete_cl3)),
                upper = NULL, diag = NULL)
table(iris$Species, hc_complete_cl3)


hc_single <- hclust(d, method = "single")
plot(hc_single)

hc_single_cl2 <- cutree(hc_single, k = 2)
GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(hc_single_cl2)),
                upper = NULL, diag = NULL)
table(iris$Species, hc_single_cl2)


hc_average <- hclust(d, method = "average")
plot(hc_average)

hc_average_cl2 <- cutree(hc_average, k = 2)
GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(hc_average_cl2)),
                upper = NULL, diag = NULL)
table(iris$Species, hc_average_cl2)



km2 <- kmeans(iris[1:4], centers = 2)
str(km2)

GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(km2$cluster)),
                upper = NULL, diag = NULL)

set.seed(123)
km3 <- kmeans(iris[1:4], centers = 3)
GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(km3$cluster)),
                upper = NULL, diag = NULL)

km4 <- kmeans(iris[1:4], centers = 4)
GGally::ggpairs(iris, 
                columns = 1:4, 
                aes(color = Species,
                    shape = as.character(km4$cluster)),
                upper = NULL, diag = NULL)
