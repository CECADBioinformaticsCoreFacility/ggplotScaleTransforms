library(dplyr)
library(ggplot2)

source("R/helpers.R")
source("R/draw_plot.R")
source("R/interval_trans.R")


## ------------------------------------------------------------------------
## scale y axis only:
## ------------------------------------------------------------------------
data <-  readr::read_tsv("data/xydata_1.tsv")
data_intervals <- readr::read_tsv("data/y_intervals_1.tsv")


draw_plot(data,
          x_trans=NULL,
          y_trans=interval_trans,
          y_trans_params=list(intervals = data_intervals %>% 
                                select(from,to),
                              scaling_factors = data_intervals %>%
                                pull(scaling_factors)
            
          ))


## ------------------------------------------------------------------------
## scale both axes:
## ------------------------------------------------------------------------
data <-  readr::read_tsv("data/xydata_2.tsv")
x_data_intervals <- readr::read_tsv("data/x_intervals_2.tsv")
y_data_intervals <- readr::read_tsv("data/y_intervals_2.tsv")


draw_plot(data,
          x_trans=interval_trans,
          x_trans_params=list(intervals = x_data_intervals %>% 
                                select(from,to),
                              scaling_factors = x_data_intervals %>%
                                pull(scaling_factors)
                              ),
                              
          y_trans=interval_trans,
          y_trans_params=list(intervals = y_data_intervals %>% 
                                select(from,to),
                              scaling_factors = y_data_intervals %>%
                                pull(scaling_factors)
                              )
                              
          )

## ------------------------------------------------------------------------
## Use a built-in transformer:
## ------------------------------------------------------------------------
draw_plot(readr::read_tsv("data/xydata_1.tsv"),
          x_trans=NULL,
          y_trans="log10" ##"sqrt"      
          )


