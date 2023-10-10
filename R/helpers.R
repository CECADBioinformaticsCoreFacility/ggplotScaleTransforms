##library(ggplot2)

shift_right <- function(x,pad=0) {
    c(pad,
      head(x,-1)
      )
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

