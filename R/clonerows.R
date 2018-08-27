
library(tidyverse)
library(rlang)

clonerows <- function(x,f) {

  cloned <- data.frame()
  freqs <- sort(unique(pull(filter(x,!!rlang::sym(f)>0))))
  
  for (i in freqs) {
    therow <- filter(x, !!rlang::sym(f) == i)
    curr <- data.frame()
    for (j in 1:i) {
      curr <- bind_rows(curr, therow)
    }
    cloned <- bind_rows(cloned, curr)
  }
  return(select(cloned, -!!rlang::sym(f)))
}
