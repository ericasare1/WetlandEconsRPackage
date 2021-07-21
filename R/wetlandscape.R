
#' Wetland landscape generator
#' @importFrom stats runif
#' @param nq Integer. Full name is quarter-section and it is the number of
#' quarter-section (160-acre) farmlands in the sub-basin. The minimum number of quarter sections must be 46.
#' @param w1_min Integer. It is the minimum wetland size (ha) in the tier 1 group of wetlands in the sub-basin.
#' @param w1_max Integer. It is the maximum wetland size (ha) in the tier 1 group of wetlands in the sub-basin.
#' @param w3_min Integer. It is the minimum wetland size (ha) in the tier 3 group of wetlands in the sub-basin.
#' @param w3_max Integer. It is the maximum wetland size (ha) in the tier 3 group of wetlands in the sub-basin.
#' @param w6_min Integer. It is the minimum wetland size (ha) in the tier 6 group of wetlands in the sub-basin.
#' @param w6_max Integer. It is the maximum wetland size (ha) in the tier 6 group of wetlands in the sub-basin.
#' @param w12_min Integer. It is the minimum wetland size (ha) in the tier 12 group of wetlands in the sub-basin.
#' @param w12_max Integer. It is the maximum wetland size (ha) in the tier 12 group of wetlands in the sub-basin.
#' @param w24_min Integer. It is the minimum wetland size (ha) in the tier 24 group of wetlands in the sub-basin.
#' @param w24_max Integer. It is the maximum wetland size (ha) in the tier 24 group of wetlands in the sub-basin.
#' @param p1_min Integer. It is the minimum probability of harvesting in the tier 1 group of wetlands in the sub-basin.
#' It is between 0 and 1.
#' @param p3_min Integer. It is the minimum probability of harvesting in the tier 3 group of wetlands in the sub-basin.
#' It is between 0 and 1.
#' @param p6_min Integer. It is the minimum probability of harvesting in the tier 6 group of wetlands in the sub-basin.
#' It is between 0 and 1.
#' @param p12_min Integer. It is the minimum probability of harvesting in the tier 12 group of wetlands in the sub-basin.
#' It is between 0 and 1.
#' @param p24_min Integer. It is the minimum probability of harvesting in the tier 24 group of wetlands in the sub-basin.
#' It is between 0 and 1.
#' @param dc_min Integer. It is the minimum drainage cost of wetlands, $/ha.
#' @param dc_max Integer. It is the maximum drainage cost of wetlands, $/ha.
#' @param nwl_min Integer. The minimum number of wetlands.
#' @param nwl_max Integer. The maximum number of wetlands.
#'
#' @return The output is a dataframe of wetland characteristics in the sub-basin.
#' @export
#'
#' @examples
#' wetlandscape(nq = 14000, w1_min = 0.1, w1_max = 1,
#' w3_min = 0.5, w3_max = 2, w6_min = 3, w6_max = 5, w12_min=3, w12_max=4,
#' w24_min=2, w24_max=6,p1_min=0.6, p3_min=0.5, p6_min=0.6, p12_min=0.7, p24_min = .9, dc_min = 500, dc_max = 1000,
#' nwl_min =2, nwl_max=4)

wetlandscape <- function(nq, w1_min, w1_max,  w3_min, w3_max, w6_min, w6_max,
                         w12_min, w12_max, w24_min, w24_max,
                         p1_min, p3_min, p6_min, p12_min, p24_min,
                         dc_min, dc_max, nwl_min, nwl_max) {
  #wetland configuration: 24:12:6:3:1
  tier5 <- 24
  tier4 <- 12
  tier3 <- 6
  tier2 <- 3
  tier1 <- 1

  total <-  tier1 + tier2 +  tier3 + tier4 + tier5

  prop_24 <- as.integer((tier5/total) * nq)
  prop_12 <- as.integer((tier4/total) * nq)
  prop_6 <- as.integer((tier3/total) * nq)
  prop_3 <- as.integer((tier2/total) * nq)
  headwater <- as.integer((tier1/total) * nq)

  total_wl <- prop_24 + prop_12 + prop_6 + prop_3 + headwater #check that the total equals to 14000 QS

  wl_24  <- data.frame( wl= runif(prop_24, w24_min, w24_max),  # range of wl acres: 0.8 to 2 acres
                        tier = "tier 5",
                        pr = runif(prop_24, p24_min, 1))

  wl_12  <- data.frame(wl= runif(prop_12, w12_min, w12_max),  # range of wl acres: 0.6 to 0.8 acres
                       tier = "tier 4",
                       pr = runif(prop_12, p12_min, 1))

  wl_6  <- data.frame(wl= runif(prop_6, w6_min, w6_max),  # range of wl acres: 0.3 to 0.6 acres
                      tier = "tier 3",
                      pr = runif(prop_6, p6_min, 1))

  wl_3  <- data.frame(wl= runif(prop_3, w3_min, w3_max),  # range of wl acres: 0.1 to 0.3 acres
                      tier = "tier 2",
                      pr = runif(prop_3, p3_min, 1))

  wl_1  <- data.frame(wl= runif(headwater, w1_min, w1_max),  # range of wl acres: 0.05 to 0.09 acres
                      tier = "tier 1",
                      pr = runif(headwater, p1_min, 1))

  df_wl1 <- rbind(wl_24, wl_12, wl_6, wl_3, wl_1) # Combining all data

 # df_wl1$dc <- runif(nrow(df_wl1), dc_min, dc_max)

 df_wl <- df_wl1 %>%
    dplyr::mutate(
      dc = runif(nrow(df_wl1), dc_min, dc_max),
      nwl = purrr::rdunif(nrow(df_wl1), nwl_min, nwl_max),
      loc_id = purrr::rdunif(nrow(df_wl1), 1, 2),
      wl_location = sample(ifelse(loc_id ==1, "corner", "center"))
  ) %>% dplyr::select(wl, tier, pr, dc, nwl, wl_location)

  return(df_wl)

}

