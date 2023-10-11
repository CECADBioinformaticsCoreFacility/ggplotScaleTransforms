##library(ggplot2)

## shift a vector by one position to the left or right,
## substituting the emptied rightmost (leftmost) position by "pad"
## and discarding the leftmost (rightmost) position
shift_left <- function(x,pad=0) {
  c(tail(x,-1),
    pad
  )
}

shift_right <- function(x,pad=0) {
    c(pad,
      head(x,-1)
      )
}

## check if data.frame df contains a set of columns
## with the name=class values given in col_classes
check_columns <- function(df, col_classes) {
  if (!is(df, "data.frame")) {
    stop('"df" must have class "data.frame"!')
  }
  
  all(names(col_classes) %in% colnames(df)) && 
    identical(purrr::map_chr(df[,names(col_classes)],
                             class),
              col_classes )
  
  
}

## check whether it makes sense to call a function (by do.call() or 
## something equivalent) with the parameter names in "params"
check_parameters <- function(f, params) {
  if (!is(f, "function")) {
    stop('"f" must have class "function"!')
  }
  
  actual_params <- formalArgs(f)
  
  if ("..." %in%  actual_params) { ## could swallow anything ..
    TRUE
  } else {
    all(params %in% actual_params)
  }
}

find_bins <- function(v,from,to) {
    ## v: a vector of one or more numbers
    ## from, to: two vectors of equal lengths,
    ##           giving inclusive start and
    ##           exclusive end of one or more
    ##           intervals

    bins <- purrr::map_int(1:length(v),
                           function(i) {
                               ## the last right interval boundary
                               ## must be considered INclusive!
                               cmp <- ifelse(i==length(v),
                                             `<=`,
                                             `<`
                                             )
                               b <- which((v[i]>=from) &
                                          cmp(v[i],to)
                                          )
                               if(length(b)==0) {
                                   NA
                               } else {
                                   min(b)
                               }
                           })
    
    ##if(any(is.na(bins))) {
    ##    stop("One or more elements of v ",
    ##         "were not covered by the binning intervals!")
    ##}
    bins
                       
}

