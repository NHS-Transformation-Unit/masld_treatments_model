
# Triangular --------------------------------------------------------------

tri_sample <- function(lower, central, upper, n = 100){
  
  rtri(n, min = lower, mode = central, max = upper)
  
}
