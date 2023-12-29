calc_bridg_po <- function(confidants){
  N <- nrow(confidants)
  if (N == 1){
    return(FALSE)
  }
  
  for(i in 2:N) {
    confidant <- confidants[i, ]
    if (any(is.na(confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                              "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")[1:(i-1)]]))) {
      return(TRUE)
    }
    if (any(confidant[c("TALKFREQ1", "TALKFREQ2", "TALKFREQ3",
                        "TALKFREQ4", "TALKFREQ5", "TALKFREQ6")[1:(i-1)]]) == "(0) have never spoken to each other)"){
      return(TRUE)
    }
    return(FALSE)
  }
}