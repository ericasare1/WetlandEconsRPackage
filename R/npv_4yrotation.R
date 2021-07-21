


#' Net present value of drainage for 4-year crop rotation

#'
#' @param np1 It is the difference between farm revenue and cost of production for crop 1.Unit is $/ha.
#' @param np2 It is the difference between farm revenue and cost of production for crop 2. Unit is $/ha.
#' @param np3 It is the difference between farm revenue and cost of production for crop 3. Unit is $/ha.
#' @param np4 It is the difference between farm revenue and cost of production for crop 4. Unit is $/ha.
#' @param wl Wetland size in ha.
#' @param dc Drainage cost in $.
#' @param r Discount rate in Percentage.
#' @param t Planning horizon.
#'
#' @return Dataframe or single vale of net present value of converted wetland(s).
#' @export
#'
#' @examples
#' npv_4yrotation(np1=10, np2=20, np3=30, np4=40, wl=3, dc=500, r=0.87, t=50)


npv_4yrotation <- function(np1, np2, np3, np4, wl, dc, r, t){
  npv1 = 0
  npv2 = 0
  npv3 = 0
  npv4 = 0

  for (i in seq(from = 0, to = t, by = 4)) {
    current = (wl*np1 * (1 + r)^-i)
    npv1 = npv1 + current
  }

  for (i in seq(from = 1, to = t, by = 4)) {
    current = (wl*np2 * (1 + r)^-i)
    npv2 = npv2 + current
  }

  for (i in seq(from = 2, to = t, by = 4)) {
    current = (wl*np3 * (1 + r)^-i)
    npv3 = npv3 + current
  }

  for (i in seq(from = 3, to = t, by = 4)) {
    current = (wl*np4 * (1 + r)^-i)
    npv4 = npv4 + current
  }

  pv = npv1 + npv2 + npv3 + npv4
  return((pv - dc*wl)/t/wl)
}
