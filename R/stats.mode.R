
#' Function to compute the mode of a vector
#' 
#' @param vec Input vector for which the mode should be computed
#' @param na.rm a logical evaluating to TRUE or FALSE indicating whether NA values should be stripped before the computation proceeds.
#' @return Mode of the input vector, i.e., its most frequent value
#' @export
csslr.stats.mode <- function(vec, na.rm=FALSE) {
  if (na.rm == TRUE) {
    vec <- vec[!is.na(vec)]
  }

  vecUnique <- unique(vec)
  theMode <- vecUnique[which.max(tabulate(match(vec, vecUnique)))]

  return(theMode)
}
