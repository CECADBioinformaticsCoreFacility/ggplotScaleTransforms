library(ggplot2)

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
                              pad = 0)
  
  ## The offsets of the scaled intervals (in location space)
  ## can in principle be calculated in the same way,
  ## however because they are needed by the scaling function
  ## itself, they must be precomputed in a little more involved
  ## way.
  ## NOTE that even if the first interval does not start at zero,
  ## it must anyway be treated as if it did, i.e. it contributes
  ## an offset of intervals$to[1].
  scaled_interval_len <- c(intervals$to[1],
                           intervals$to[-1] - intervals$from[-1]) *  scaling_factors
  
  scaled_offsets <- cumsum(shift_right(scaled_interval_len,
                                       pad = 0))
  
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
    b <- find_bins(x,
                   intervals$from,
                   intervals$to)
    f <- scaling_factors[b]
    
    (x -
        data_offsets[b] +
        (scaled_offsets[b] / f)) * f
    
  }
  ## The inverse function needs the scaled intervals,
  ## in order to find the scaling factors of the scaled locations,
  ## so we need to precompute them using scale_forward:
  scaled_intervals <-
    purrr::map_dfr(c(from = 1, to = 2),
                   function(.x)
                     scale_forward(intervals[[.x]]))
  
  
  ## Now we can define the inverse function.
  ## Map a scaled location to the corresponding data value:
  scale_inverse <- function(x) {
    b <- find_bins(x,
                   scaled_intervals$from,
                   scaled_intervals$to)
    f <- scaling_factors[b]
    
    (x - scaled_offsets[b]) / f +
      data_offsets[b]
  }
  
  ##browser()
  scales::trans_new(
    "interval_scaling",
    transform = scale_forward,
    inverse  = scale_inverse,
    domain = c(min(intervals$from),
               max(intervals$to))
  )
}
