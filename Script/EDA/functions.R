get_hd <- function(a, b){
  # a is a vector of 2 longitudes, b is a vector of 2 latitudes
  dx <- distm(c(a[1], b[1]), c(a[2], b[1]), fun = distHaversine)*(-1)^(a[1] < a[2])
  dy <- distm(c(a[1], b[1]), c(a[1], b[2]), fun = distHaversine)*(-1)^(b[1] < b[2])
  
  return (list('dx' = dx, 'dy' = dy))
}