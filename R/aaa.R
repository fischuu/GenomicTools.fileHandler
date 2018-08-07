# This function mixes two vectors alternating
mixVectors <- function(x,y){
  unlist(c(rbind(x, y)) )
}

# Get a substring from the right end side of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}