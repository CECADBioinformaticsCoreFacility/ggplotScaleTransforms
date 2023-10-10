library(ggplot2)

source("R/helpers.R")
source("R/interval_trans.R")


data_intervals <-
  data.frame(from = c(0,  4,  8,  20),
             to   = c(4,  8 , 20,  Inf))

scaling_factors <- c(1,     1 / 2,    1 / 3,   1 / 10)


p_y <- ggplot(tibble::tibble(x = readr::read_tsv("data/xydata_1.tsv")$x,
                             y = readr::read_tsv("data/xydata_1.tsv")$y),
              aes(x = x, y = y)) +
  ##theme_classic() +
  geom_point() +
  
  scale_y_continuous(trans = interval_trans(data_intervals,
                                            scaling_factors),
                     n.breaks = 50
                     )
                     

## ------------------------------------------------------------------------
## 2-axes scaling:
## ------------------------------------------------------------------------
x_data_intervals <-
  data.frame(from = c(-5,  20,  30),
             to   = c(20, 30 , Inf))
x_scaling_factors <- c(1 / 10,     1,    1 / 10)

y_data_intervals <-
  data.frame(from = c(-5,   10,  20),
             to   = c(10,  20 , Inf))
y_scaling_factors <- c(1 / 10,     1,    1 / 10)


p_xy <- ggplot(tibble::tibble(x = readr::read_tsv("data/xydata_2.tsv")$x,
                              y = readr::read_tsv("data/xydata_2.tsv")$y),
               aes(x = x, y = y)) +
  geom_point() +
  
  scale_x_continuous(trans = interval_trans(x_data_intervals,
                                            x_scaling_factors),
                     n.breaks = 50) +
  scale_y_continuous(trans = interval_trans(y_data_intervals,
                                            y_scaling_factors),
                     n.breaks = 50)

