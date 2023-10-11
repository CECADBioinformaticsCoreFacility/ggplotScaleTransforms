source("R/helpers.R")
source("R/interval_trans.R")

requires(ggplot2)

get_transfunc <- function(transformation_info) {
  if (!is(transformation_info),
      "data.frame") {
    return(NULL)
  }
  
  if (all(c("from",
            "to",
            "scaling_factor") %in% colnames(x_transformation_info))) {
    transfunc_x <-
      interval_trans(
        x_transformation_info %>% select(from, to),
        x_transformation_info %>% pull(scaling_factor)
      )
  } else {
    NULL
  }
}

draw_plot <- function(x_data,
                      y_data,
                      x_transformation_info = NULL,
                      y_transformation_info = NULL,
                      x_nbreaks=50,
                      y_nbreaks=50) {
  
  transfunc_x <- get_transfunc(x_transformation_info)
  transfunc_y <- get_transfunc(y_transformation_info)


  p <- ggplot(tibble::tibble(x = x_data,
                             y = y_data),
              aes(x = x,
                  y = y)) +
    ##theme_classic() +
    geom_point()
  
  if(!is.null(transfunc_x)) {
    p <- p + scale_x_continuous(trans = transfunc_x)
                       n.breaks = 50
    )
    
  }
                
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

