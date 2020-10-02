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
#     F = the desired width of the confidence interval for RD measures and ratio of ULCI / LLCI for relative measures
#     deff = the ratio of the variance in the sample with the clustered design to the variance if the sample had been done using
#             simple random sampling


#######################################
#                                     #
#          Calculating DEFF           #
#                                     #
#######################################
#' @export
deff <- function(icc, n_cluster) {
  deff_table <- 1 + icc * (n_cluster - 1)
  return(deff_table)
}


##############################################
#                                            #
#   Sample size based on precision for RD    #
#                                            #
##############################################

#' @export
smp_size_rd <- function(p1, p0, r, cl, f, deff) {
  require(tidyverse)
  
  z <- qnorm(1 - (1 - cl) / 2)
  
  smp_size_tab <- tibble(
    n1 = 4 * z ^ 2 * deff * (r * p1 * (1 - p1) + p0 * (1 - p0)) / (f ^ 2 * r), 
    n0 = n1 * r, 
    N = n1 + n0) %>% 
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
                    cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
                    r, sep = " "))
      
  
  return(smp_size_tab)
}

smp_size_rd(.4, .3, 3, .90, 0.08, 1) ### checking with Rothman & Greenland's example


##############################################
#                                            #
#   Sample size based on precision for RR    #
#                                            #
##############################################
#' @export
smp_size_rr <- function(p1, p0, r, cl, f_r, deff) {
  require(tidyverse)
  
  z <- qnorm(1 - (1 - cl) / 2)
  
  smp_size_tab <- tibble(
    n1 = 4 * z ^ 2 * deff * (r * p0 * (1 - p1) + p1 * (1 - p0)) / (r * p1 * p0 * log(f_r) ^ 2), 
    n0 = n1 * r, 
    N = n1 + n0
  ) %>% 
    round()
  
  message(paste(
    paste(
      paste(
        paste(
          paste(
            paste(
              paste("sample size based on precision for ratio of ULCI to LLCI of", f_r, sep = " "), 
              "a deff of", sep = ", "), 
            deff, sep = " "), 
          "a confidence level of", sep = ", "), 
        cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
    r, sep = " "))
  
  
  return(smp_size_tab)
}

smp_size_rr(0.4, 0.3, 3, .95, 2, 1) ### checking with Rothman & Greenland's example

###########################################################
#                                                         #
#   Sample size based on precision for Rate Differences   #
#                                                         #
###########################################################
#' @export
smp_size_ird <- function(i1, i0, r, cl, f_r, deff) {
  require(tidyverse)
  
  z <- qnorm(1 - (1 - cl) / 2)
  
  smp_size_tab <- tibble(
    n1 = 4 * z ^ 2 * deff * (r * i0 + i1) / (r * f ^ 2), 
    n0 = n1 * r, 
    N = n1 + n0
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
        cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
    r, sep = " "))
  
  
  return(smp_size_tab)
}


#############################################
#                                           #
#   Sample size based on precision for IR   #
#                                           #
#############################################
#' @export
smp_size_irr <- function(i1, i0, r, cl, f_r, deff) {
  require(tidyverse)
  
  z <- qnorm(1 - (1 - cl) / 2)
  
  
  smp_size_tab <- tibble(
    n1 = 4 * z ^ 2 * deff * (r * i0 + i1) / (r * i1 * i0 * log(f_r) ^ 2), 
    n0 = n1 * r, 
    N = n1 + n0
  ) %>% 
    round()
  
  message(paste(
    paste(
      paste(
        paste(
          paste(
            paste(
              paste("sample size based on precision for ratio of ULCI to LLCI of", f_r, sep = " "), 
              "a deff of", sep = ", "), 
            deff, sep = " "), 
          "a confidence level of", sep = ", "), 
        cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
    r, sep = " "))
  
  
  return(smp_size_tab)
}

#############################################
#                                           #
#   Sample size based on precision for OR   #
#                                           #
#############################################

### note here m1 is the size of the cases group, m0 is the size of the controls group, 
### r is the ratio of controls to cases, p1 is the prevalence of exposure in cases, 
### p0 is the prevalence of exposure in controls.
#' @export
smp_size_or <- function(p1, p0, r, cl, f_r, deff) {
  require(tidyverse)
  
  z <- qnorm(1 - (1 - cl) / 2)
  
  
  smp_size_tab <- tibble(
    m1 = 4 * z ^ 2 * deff * (r * p0 * (1 - p0) + p1 * (1 - p1)) / ((log(f_r) ^ 2) * (r * p1 * p0 * (1 - p1) * (1 - p0))), 
    m0 = m1 * r, 
    M = m1 + m0
  ) %>% 
    round()
  
  message(paste(
    paste(
      paste(
        paste(
          paste(
            paste(
              paste("sample size based on precision for ratio of ULCI to LLCI of", f_r, sep = " "), 
              "a deff of", sep = ", "), 
            deff, sep = " "), 
          "a confidence level of", sep = ", "), 
        cl, sep = " "), "and an unexposed to exposed group size ratio of", sep = ", "),
    r, sep = " "))
  
  
  return(smp_size_tab)
}

