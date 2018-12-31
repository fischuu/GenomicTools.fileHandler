# This function mixes two vectors alternating
mixVectors <- function(x,y){
  unlist(c(rbind(x, y)) )
}

# Get a substring from the right end side of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

ascii <- c("!", '"', "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
Q <- 0:42

ascii_base33 <- data.frame(Q=Q,
                           P=10^(-Q/10),
                           ascii=ascii)
