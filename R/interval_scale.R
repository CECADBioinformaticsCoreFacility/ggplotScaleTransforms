library(ggplot2)

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


interval_trans <- function(intervals,
                           scaling_factors) {
   

    ## intervals: a 2-column data.frame with colnames (in this order)
    ##            from: INclusive start points
    ##                  of a set of intervals in real space
    ##            to: EXclusive end points of these intervals,
    ##                where the "to" value of the last interval
    ##                is considered INclusive
    ##
    ##            The intervals must be consecutive,
    ##            i.e., to[i] == from[i+1].
    ##
    ## scaling_factors: a numeric vector of length nrow(intervals).
    ##                  For data points contained in intervals[i,],
    ##                  the inter-point distances on the scaled
    ##                  axis will be shrunken by the factor
    ##                  scaling_factors[i].

    ## The forward and inverse transformations treat a point
    ## in an interval [a,b[ as a + [1,(b-a)]. a is the "offset".
    ## Offsets are required by both the forward and
    ## the inverse transformations.

    ## The offset of an interval in data space is 
    ## the right (exclusive) boundary of the preceding interval
    ## (the first interval has offset zero).
    data_offsets <- shift_right(intervals$to,
                               pad=0)

    ## The offsets of the scaled intervals (in location space)
    ## can in principle be calculated in the same way,
    ## however because they are needed by the scaling function
    ## itself, they must be precomputed in a little more involved
    ## way.
    ## NOTE that even if the first interval does not start at zero,
    ## it must anyway be treated as if it did, i.e. it contributes
    ## an offset of intervals$to[1].
    scaled_interval_len <- c(intervals$to[1],
                             intervals$to[-1] - intervals$from[-1]
                             ) *  scaling_factors
    
    scaled_offsets <- cumsum(shift_right(scaled_interval_len,
                                         pad=0)
                             )

    ## Define the forward and inverse mapping functions --
    ## NOTE that they take the values of the intervals, the offsets,
    ## and the scaling factors from the environment of interval_trans(). 
    ## Only the values to be mapped to location space
    ## or back to data space are passed as a (single) parameter.
    ## Is this safe?
    ## If not, is it possible for "transform" and "inverse" to take
    ## more than a single parameter?

    ## Map a data value to its scaled location:
    scale_forward <- function(x) {
        b <-find_bins(x,
                      intervals$from,
                      intervals$to
                                        )
        f <- scaling_factors[b]
        
        (x -
         data_offsets[b] +
         (scaled_offsets[b] / f)
        ) * f
        
    }
    ## The inverse function needs the scaled intervals,
    ## in order to find the scaling factors of the scaled locations,
    ## so we need to precompute them using scale_forward:
    scaled_intervals <-
        purrr::map_dfr(c(from=1,to=2),
                       function(.x) scale_forward(intervals[[.x]])
                       )
    

    ## Now we can define the inverse function.
    ## Map a scaled location to the corresponding data value:
    scale_inverse <- function(x) {
        b <-find_bins(x,
                      scaled_intervals$from,
                      scaled_intervals$to
                      )
        f <- scaling_factors[b]
        
        (x-scaled_offsets[b]) / f +
            data_offsets[b]     
    }

    ##browser()
    scales::trans_new("interval_scaling",
                      transform = scale_forward, 
                      inverse  = scale_inverse, 
                      domain=c(min(intervals$from),
                               max(intervals$to)
                               )
                      )
}

## ------------------------------------------------------------------------
## Try it out:
## ------------------------------------------------------------------------
data <- c(0.1, 1, 2,2.5,12,9.45, 21.07,91)


data_intervals <-
    data.frame(from = c(0,  4,  8,  20 ),
               to   = c(4,  8 , 20,  Inf)
               )


scaling_factors <- c(1,     1/2,    1/3,   1/10)


##w <- 10
##Cairo::Cairo(file = "testcase.png", unit = "in", dpi = 300, width = w, 
##        height = w * 6/10, type = "png", bg = "white")
ggplot(tibble::tibble(x=c(10, 5, 2, 7, 10, 11,20, 50),
                      y=data
                      ),
       aes(x=x,y=y)
       ) +
    ##theme_classic() +
    geom_point() +

    scale_y_continuous(trans = interval_trans(data_intervals,
                                              scaling_factors),
                       n.breaks=50
                       ##expand = c(0, 0) 
                      )
    
##dev.off()

## ------------------------------------------------------------------------
## 2-axes scaling:
## ------------------------------------------------------------------------
x_data_intervals <- 
        data.frame(from = c(-5,  20,  30),
                   to   = c(20, 30 , Inf)
                   )
x_scaling_factors <- c(1/10,     1,    1/10)

y_data_intervals <-
    data.frame(from = c(-5,   10,  20),
               to   = c(10,  20 , Inf)
               )
y_scaling_factors <- c(1/10,     1,    1/10)



x_data <- c(-2, 2,21, 23,  24,   26.3, 26.9, 29, 50.3, 82,  87, 95, 105)
y_data <- c(-4, 1, 2, 2.5, 9.45, 10.1, 12, 15, 41.6,50.5,60, 82, 91)

##w <- 10
##Cairo::Cairo(file = "double_scaling.png", unit = "in", dpi = 300, width = w, 
##        height = w * 6/10, type = "png", bg = "white")
p <- ggplot(tibble::tibble(x=x_data,
                      y=y_data
                      ),
       aes(x=x,y=y)
       ) +
    geom_point() +

    scale_x_continuous(trans = interval_trans(x_data_intervals,
                                              x_scaling_factors),
                       n.breaks=50
                      ) +
    scale_y_continuous(trans = interval_trans(y_data_intervals,
                                              y_scaling_factors),
                       n.breaks=50
                       )
##dev.off()
invisible(p)

