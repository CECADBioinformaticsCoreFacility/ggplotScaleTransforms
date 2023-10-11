##source("R/helpers.R")

execute_conditionally <- function(obj, params) {
  if (class(obj) == "character") {
    ## if obj is a string and
    ## it is the name of an existing function,
    ## return it as is
    ifelse(exists(obj) &&
             (class(eval(as.symbol(
               obj
             ))) == "function"),
           obj,
           NULL)
    
  } else if (class(obj) == "function") {
    ## if obj is a function object,
    ## call it with params (if appropriate) and return the result
    
    if (check_parameters(obj, names(params))) {
      do.call(obj, params)
    } else {
      NULL
    }
  } else {
    ## in any other case, return NULL
    
    NULL
  }
}


draw_plot <- function(data,
                      x_trans = NULL,
                      x_trans_params = list(),
                      y_trans = NULL,
                      y_trans_params = list(),
                      x_n.breaks = 50,
                      y_n.breaks = 50,
                      classic = FALSE) {
  if (!check_columns(data,
                     c(x = "numeric", 
                       y = "numeric"))) {
    stop('Expecting y=numeric columns "x" and "y" in "data"!')
  }
  
  x_trans <- execute_conditionally(x_trans, x_trans_params)
  y_trans <- execute_conditionally(y_trans, y_trans_params)
  
  p <- ggplot2::ggplot(data,
                       aes(x = x,
                           y = y)) + geom_point()
  
  if (classic)
    p <- p + theme_classic()
  
  if (!is.null(x_trans)) {
    p <- p + scale_x_continuous(trans = x_trans,
                                n.breaks =  x_n.breaks)
  }
  if (!is.null(y_trans)) {
    p <- p + scale_y_continuous(trans = y_trans,
                                n.breaks =  y_n.breaks)
  }
  
  p
}

