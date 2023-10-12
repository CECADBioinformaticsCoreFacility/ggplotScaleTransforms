require(scales)
#' Return a transformation object with interval-dependent inter-point distances
#'  
#'@description
#'`interval_trans` returns a transformation object which can be passed to 
#'`ggplot2::scale_x_continuous` or  `ggplot2::scale_y_continuous` via the 
#'`trans` parameter of these functions. The domain of the transformation 
#'on the real line is assumed to be covered by N consecutive intervals,
#'whose inclusive start points and exclusive endpoints are given by 
#'columns "from" and "to" of the `intervals` parameter. (Note that the 
#'endpoint of the last interval is assumed inclusive).
#'For each interval, a positive numeric scaling factor is given in parameter
#'`scaling_factors`. The transformation is defined as follows:   
#'Assume four data points such that (1) the distance between p_1 and p_2 equals
#'the distance between p_3 and p_4 on the un-scaled axis, (2) p_1 and p_2 are in
#'interval i, with scaling factor s_i, while p_3 and p_4 are in interval j, 
#'with scaling factor s_j. Then on the scaled axis, the distance between 
#'p_1 and p_2 equal s_i/s_j times the distance between p_3 and p_4.
#'@param intervals A 2-column data.frame with colnames (in this order) "from"
#'and "to". 
#'@param scaling_factors A non-negative numeric vector, of length `nrow(intervals)` 
#'@return A transformation object of class "trans".
interval_trans <- function(intervals,
                           scaling_factors) {

  ## The forward and inverse transformations treat a point
  ## in an interval [a,b[ as a + [1,(b-a)]. a is the "offset".
  ## Offsets are required by both the forward and
  ## the inverse transformations.
  
  ## The offset of an interval in data space is simply
  ## the right (exclusive) boundary of the preceding interval
  ## (the first interval has offset zero).
  data_offsets <- shift_right(intervals$to,
                              pad = 0)
  
  ## Pre-compute the offsets of the scaled intervals (in location space)
  ## (offsets in both spaces are needed by both transformation functions):
  ## (NOTE that even if the first interval does not start at zero,
  ##  it must anyway be treated as if it did, i.e. it contributes
  ##  an offset of intervals$to[1]).
  scaled_interval_len <- c(intervals$to[1],
                           intervals$to[-1] - intervals$from[-1]) *  scaling_factors
  
  scaled_offsets <- cumsum(shift_right(scaled_interval_len,
                                       pad = 0))
  
  ## Define the forward and inverse mapping functions --
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
  ## in order to find the scaling factors of the scaled locations.
  ## Precompute them using the already-defined scale_forward function:
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
  
  scales::trans_new(
    "interval_scaling",
    transform = scale_forward,
    inverse  = scale_inverse,
    domain = c(min(intervals$from),
               max(intervals$to))
  )
}
