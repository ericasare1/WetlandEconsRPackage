# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

wetlandscape <- function(qsect, t1ws_min, t1wls_max,  t3ws_min, t3wls_max, t6ws_min, t6wls_max,
                         t12ws_min, t12wls_max, t24ws_min, t24wls_max,
                         t1pr_min, t3pr_min, t6pr_min, t12pr_min, t24pr_min,
                         dc_min, dc_max) {
  #wetland configuration: 24:12:6:3:1
  tier5 <- 24
  tier4 <- 12
  tier3 <- 6
  tier2 <- 3
  tier1 <- 1

  total <-  tier1 + tier2 +  tier3 + tier4 + tier5

  prop_24 <- as.integer((tier5/total) * qsect)
  prop_12 <- as.integer((tier4/total) * qsect)
  prop_6 <- as.integer((tier3/total) * qsect)
  prop_3 <- as.integer((tier2/total) * qsect)
  headwater <- as.integer((tier1/total) * qsect)

  total_wl <- prop_24 + prop_12 + prop_6 + prop_3 + headwater #check that the total equals to 14000 QS

  wl_24  <- data.frame( wl= runif(prop_24, t24ws_min, t24wls_max),  # range of wl acres: 0.8 to 2 acres
                        tier = "tier 5",
                        pr = runif(prop_24, t24pr_min, 1))

  wl_12  <- data.frame(wl= runif(prop_12, t12ws_min, t12wls_max),  # range of wl acres: 0.6 to 0.8 acres
                       tier = "tier 4",
                       pr = runif(prop_12, t12pr_min, 1))

  wl_6  <- data.frame(wl= runif(prop_6, t6ws_min, t6wls_max),  # range of wl acres: 0.3 to 0.6 acres
                      tier = "tier 3",
                      pr = runif(prop_6, t6pr_min, 1))

  wl_3  <- data.frame(wl= runif(prop_3, t3ws_min, t3wls_max),  # range of wl acres: 0.1 to 0.3 acres
                      tier = "tier 2",
                      pr = runif(prop_3, t3pr_min, 1))

  wl_1  <- data.frame(wl= runif(headwater, t1ws_min, t1wls_max),  # range of wl acres: 0.05 to 0.09 acres
                      tier = "tier 1",
                      pr = runif(headwater, t1pr_min, 1))

  df_wl1 <- rbind(wl_24, wl_12, wl_6, wl_3, wl_1) # Combining all data

  df_wl <- df_wl1 %>%
     mutate(dc = runif(nrow(df_wl1), dc_min, dc_max))

  return(df_wl)

}

#a <- WetlandEconsRPackage::wetlandscape(qsect = 14000, t1ws_min = 0.1, t1wls_max = 1,  t3ws_min = 0.5, t3wls_max = 2, t6ws_min = 3, t6wls_max = 5,
                    #    t12ws_min=3, t12wls_max=4, t24ws_min=2, t24wls_max=6,
                      #  t1pr_min=0.6, t3pr_min=0.5, t6pr_min=0.6, t12pr_min=0.7, t24pr_min = .9,
                      #  dc_min = 500, dc_max = 1000) %>% View()


