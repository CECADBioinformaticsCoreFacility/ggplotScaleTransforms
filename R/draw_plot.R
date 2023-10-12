#' Return a scale transformation function or object
#' 
#' @description `get_transformer` returns an object which can be passed to 
#'`ggplot2::scale_x_continuous` or  `ggplot2::scale_y_continuous` via the 
#'`trans` parameter of these functions. This object can be constructed in two 
#'different ways, dependent on the class and on properties of 
#'`get_transformer`'s  `obj` parameter: (1) If `obj` is a string and if 
#'`paste0(obj, "_trans")` is an existing transformer function of the scales 
#'package, then `obj` is simply returned, because it is is already a valid  
#'value. Otherwise NULL is returned.  (2) If `obj` is a function object, 
#'then `get_transformer` calls it with the arguments in named list `params`. 
#'If the class of the result is  "trans", the result is returned, 
#'otherwise NULL.  This allows to create "trans" objects by custom functions, 
#'which notably can take arbitrary parameters to create parameter dependent 
#'forward and inverse functions within the "trans" object on the fly.
#'If neither (1) nor (2) apply,  `get_transformer` returns NULL. Thus, 
#'a NULL result always indicates that no valid transformer is returned, 
#'for any reason. It is up to the calling function to handle a NULL result
#'in a meaningful way.
#' @param obj Any R object.
#' @param params A named list.
#' @returns An object to be used as value of the "trans" parameter of
#'   ggplot2::scale_x_continuous or ggplot2::scale_y_continuous.
#'   
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
    ## call it with params (if appropriate) and return the result
    ## if it is of class "trans":
    
    if (check_parameters(obj, names(params))) {
      res <- do.call(obj, params)
      if(class(res) != "trans") res <- NULL 
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
      "(2) The function you passed to compute a custom transformation\n",
      "object could not handle the supplied parameters."
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

