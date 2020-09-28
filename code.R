#######################################
#                                     #
#             Parameters              #
#                                     #
#######################################

# BEFORE calculating the sample size based on precision for clustered data, the user needs:
#     p1 = the risk in the Exposed, 
#     p0 = the risk in the Unexposed
#     R = the ratio of Unexposed group size to the Exposed group size
#     Z = the confidence level
#     F = the desired width of the confidence interval 
#     deff = the ratio of the variance in the sample with the clustered design to the variance if the sample had been done using
#             simple random sampling


#######################################
#                                     #
#          Calculating DEFF           #
#                                     #
#######################################

deff <- function(icc, n_cluster) {
  deff_table <- 1 + icc * (n - 1)
  return(deff_table)
}


##############################################
#                                            #
#   Sample size based on precision for RD    #
#                                            #
##############################################


smp_size_rd <- function(p1, p0, r, z, f, deff) {
  require(tidyverse)
  smp_size_tab <- tibble(
    n1 = 4 * z ^ 2 * deff * (r * p1 * (1 - p1) + p0 * (1 - p0)) / (f ^ 2 * r), 
    n0 = n1 * r, 
    N = n1 + n0,
    p1 = p1, 
    p0 = p0
  ) %>% 
    round()
  
  message(paste(
    paste(
    paste(
    paste(
      paste(
        paste(
          paste("sample size based on precision for a CI width of", f, sep = " "), 
                  "a deff of", sep = ", "), 
                   deff, sep = " "), 
                   "a confidence level of", sep = ", "), 
                    z, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
                    r, sep = " "))
      
  
  return(smp_size_tab)
}

smp_size_rd(.4, .3, 3, 1.645, 0.08, 1)


##############################################
#                                            #
#   Sample size based on precision for RR    #
#                                            #
##############################################
