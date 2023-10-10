library(ggplot2)

data <- c(0.1, 1, 2, 2.5, 12, 9.45, 21.07, 91)


data_intervals <-
  data.frame(from = c(0,  4,  8,  20),
             to   = c(4,  8 , 20,  Inf))


scaling_factors <- c(1,     1 / 2,    1 / 3,   1 / 10)


##w <- 10
##Cairo::Cairo(file = "testcase.png", unit = "in", dpi = 300, width = w,
##        height = w * 6/10, type = "png", bg = "white")
p_y <- ggplot(tibble::tibble(x = c(10, 5, 2, 7, 10, 11, 20, 50),
                             y = data),
              aes(x = x, y = y)) +
  ##theme_classic() +
  geom_point() +
  
  scale_y_continuous(trans = interval_trans(data_intervals,
                                            scaling_factors),
                     n.breaks = 50
                     ##expand = c(0, 0) )
                     
                     ##dev.off()
                     
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


x_data <-
  c(-2, 2, 21, 23,  24,   26.3, 26.9, 29, 50.3, 82,  87, 95, 105)
y_data <- c(-4, 1, 2, 2.5, 9.45, 10.1, 12, 15, 41.6, 50.5, 60, 82, 91)

##w <- 10
##Cairo::Cairo(file = "double_scaling.png", unit = "in", dpi = 300, width = w,
##        height = w * 6/10, type = "png", bg = "white")
p_xy <- ggplot(tibble::tibble(x = x_data,
                              y = y_data),
               aes(x = x, y = y)) +
  geom_point() +
  
  scale_x_continuous(trans = interval_trans(x_data_intervals,
                                            x_scaling_factors),
                     n.breaks = 50) +
  scale_y_continuous(trans = interval_trans(y_data_intervals,
                                            y_scaling_factors),
                     n.breaks = 50)
##dev.off()

