##source("R/helpers.R")

get_transformer <- function(obj, params) {
  if (class(obj) == "character") {
    obj_name <- obj ## later needed for diagnostic message
    
    ## if obj is a string and paste0(obj, "_trans")
    ## is the name of an existing built-in transformer
    ## of the "scales" package, return the transformer:
    
    if (exists(paste0(obj, "_trans"),
               where = asNamespace("scales"),
               mode = "function")) {
      res <- obj
    } else {
      res <- NULL
    }
  } else if (class(obj) == "function") {
    obj_name <- deparse(substitute(obj))
    
    ## if obj is a function object,
    ## call it with params (if appropriate) and return the result:
    
    if (check_parameters(obj, names(params))) {
      res <- do.call(obj, params)
    } else {
      res <- NULL
    }
  } else {
    ## in all other cases, return NULL:
    res <- NULL
  }
  
  if (!is.null(obj) && is.null(res)) {
    message(
      'Your input "',
      obj_name,'"',
      " was nut NULL, but function get_transformer() returned NULL.\n",
      "Possible reasons are:\n",
      "(1) You asked for a built-in transformer which ",
      "does not exist.\n",
      "(2) The function you passed to compute a custom transformer\n",
      "could not handle the supplied parameters."
    )
  }
  res
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
  
  x_trans <- get_transformer(x_trans, x_trans_params)
  y_trans <- get_transformer(y_trans, y_trans_params)
  
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

