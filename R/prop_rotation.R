

#' Net present value of drainage for proportional crop rotation
#'
#' @param y1np1 Net profit for crop 1 in year 1. Unit is $/ha.
#' @param y1np2 Net profit for crop 2 in year 1. Unit is $/ha.
#' @param y1np3 Net profit for crop 3 in year 1. Unit is $/ha.
#' @param y1np4 Net profit for crop 4 in year 1. Unit is $/ha.
#' @param y1np5 Net profit for crop 5 in year 1. Unit is $/ha.
#' @param y2np1 Net profit for crop 1 in year 2. Unit is $/ha.
#' @param y2np2 Net profit for crop 2 in year 2. Unit is $/ha.
#' @param y2np3 Net profit for crop 3 in year 2. Unit is $/ha.
#' @param y2np4 Net profit for crop 4 in year 2. Unit is $/ha.
#' @param y2np5 Net profit for crop 5 in year 2. Unit is $/ha.
#' @param y3np1 Net profit for crop 1 in year 3. Unit is $/ha.
#' @param y3np2 Net profit for crop 2 in year 3. Unit is $/ha.
#' @param y3np3 Net profit for crop 3 in year 3. Unit is $/ha.
#' @param y3np4 Net profit for crop 4 in year 3. Unit is $/ha.
#' @param y3np5 Net profit for crop 5 in year 3. Unit is $/ha.
#' @param y4np1 Net profit for crop 1 in year 4. Unit is $/ha.
#' @param y4np2 Net profit for crop 2 in year 4. Unit is $/ha.
#' @param y4np3 Net profit for crop 3 in year 4. Unit is $/ha.
#' @param y4np4 Net profit for crop 4 in year 4. Unit is $/ha.
#' @param y4np5 Net profit for crop 5 in year 4. Unit is $/ha.
#' @param y5np1 Net profit for crop 1 in year 5. Unit is $/ha.
#' @param y5np2 Net profit for crop 2 in year 5. Unit is $/ha.
#' @param y5np3 Net profit for crop 3 in year 5. Unit is $/ha.
#' @param y5np4 Net profit for crop 4 in year 5. Unit is $/ha.
#' @param y5np5 Net profit for crop 5 in year 5. Unit is $/ha.
#' @param y1pwlnp1 Proportion of wetland area devoted to crop 1 in year 1. It should be between 0 and 1 inclusive.
#' @param y1pwlnp2 Proportion of wetland area devoted to crop 2 in year 1. It should be between 0 and 1 inclusive.
#' @param y1pwlnp3 Proportion of wetland area devoted to crop 3 in year 1. It should be between 0 and 1 inclusive.
#' @param y1pwlnp4 Proportion of wetland area devoted to crop 4 in year 1. It should be between 0 and 1 inclusive.
#' @param y1pwlnp5 Proportion of wetland area devoted to crop 5 in year 1. It should be between 0 and 1 inclusive.
#' @param y2pwlnp1 Proportion of wetland area devoted to crop 1 in year 2. It should be between 0 and 1 inclusive.
#' @param y2pwlnp2 Proportion of wetland area devoted to crop 2 in year 2. It should be between 0 and 1 inclusive.
#' @param y2pwlnp3 Proportion of wetland area devoted to crop 3 in year 2. It should be between 0 and 1 inclusive.
#' @param y2pwlnp4 Proportion of wetland area devoted to crop 4 in year 2. It should be between 0 and 1 inclusive.
#' @param y2pwlnp5 Proportion of wetland area devoted to crop 5 in year 2. It should be between 0 and 1 inclusive.
#' @param y3pwlnp1 Proportion of wetland area devoted to crop 1 in year 3. It should be between 0 and 1 inclusive.
#' @param y3pwlnp2 Proportion of wetland area devoted to crop 2 in year 3. It should be between 0 and 1 inclusive.
#' @param y3pwlnp3 Proportion of wetland area devoted to crop 3 in year 3. It should be between 0 and 1 inclusive.
#' @param y3pwlnp4 Proportion of wetland area devoted to crop 4 in year 3. It should be between 0 and 1 inclusive.
#' @param y3pwlnp5 Proportion of wetland area devoted to crop 5 in year 3. It should be between 0 and 1 inclusive.
#' @param y4pwlnp1 Proportion of wetland area devoted to crop 1 in year 4. It should be between 0 and 1 inclusive.
#' @param y4pwlnp2 Proportion of wetland area devoted to crop 2 in year 4. It should be between 0 and 1 inclusive.
#' @param y4pwlnp3 Proportion of wetland area devoted to crop 3 in year 4. It should be between 0 and 1 inclusive.
#' @param y4pwlnp4 Proportion of wetland area devoted to crop 4 in year 4. It should be between 0 and 1 inclusive.
#' @param y4pwlnp5 Proportion of wetland area devoted to crop 5 in year 4. It should be between 0 and 1 inclusive.
#' @param y5pwlnp1 Proportion of wetland area devoted to crop 1 in year 5. It should be between 0 and 1 inclusive.
#' @param y5pwlnp2 Proportion of wetland area devoted to crop 2 in year 5. It should be between 0 and 1 inclusive.
#' @param y5pwlnp3 Proportion of wetland area devoted to crop 3 in year 5. It should be between 0 and 1 inclusive.
#' @param y5pwlnp4 Proportion of wetland area devoted to crop 4 in year 5. It should be between 0 and 1 inclusive.
#' @param y5pwlnp5 Proportion of wetland area devoted to crop 5 in year 5. It should be between 0 and 1 inclusive.
#' @param wl Wetland area (ha).
#' @param dc Drainage Cost ($/ha).
#' @param r Discount rate (percentage).
#' @param t Planning horizon (years).
#'
#' @return The output can be a dataframe or single vale of net present value of converted wetland(s).
#' @export
#'
#' @examples
#' prop_rotation <- function(y1np1 =10, y1np2=30, y1np3=40, y1np4=50, y1np5=60,
#' y1pwlnp1=0.2, y1pwlnp2=0.2, y1pwlnp3=0.2, y1pwlnp4=0.2, y1pwlnp5=0.2,
#' y2np1=70, y2np2 =80, y2np3=0, y2np4=0, y2np5=0,
#' y2pwlnp1=0.6, y2pwlnp2=0.4, y2pwlnp3=0, y2pwlnp4=0, y2pwlnp5=0,
#' y3np1=90, y3np2=0, y3np3=0, y3np4=0, y3np5,
#' y3pwlnp1=1, y3pwlnp2=0, y3pwlnp3=0, y3pwlnp4=0, y3pwlnp5=0,
#' y4np1=90, y4np2=0, y4np3=0, y4np4=0, y4np5=0,
#' y4pwlnp1=1, y4pwlnp2=0, y4pwlnp3=0, y4pwlnp4=0, y4pwlnp5=0,
#' y5np1=30, y5np2=30, y5np3=80, y5np4=0, y5np5=0,
#' y5pwlnp1=0.5, y5pwlnp2=0.3, y5pwlnp3=0.2, y5pwlnp4=0, y5pwlnp5=0,
#' wl=10, dc=400, r=0.8, t=50)



prop_rotation <- function(y1np1, y1np2, y1np3, y1np4, y1np5, y2np1, y2np2, y2np3, y2np4, y2np5,
                          y3np1, y3np2, y3np3, y3np4, y3np5, y4np1, y4np2, y4np3, y4np4, y4np5,
                          y5np1, y5np2, y5np3, y5np4, y5np5, y1pwlnp1, y1pwlnp2, y1pwlnp3, y1pwlnp4, y1pwlnp5,
                          y2pwlnp1, y2pwlnp2, y2pwlnp3, y2pwlnp4, y2pwlnp5, y3pwlnp1, y3pwlnp2, y3pwlnp3, y3pwlnp4, y3pwlnp5,
                          y4pwlnp1, y4pwlnp2, y4pwlnp3, y4pwlnp4, y4pwlnp5, y5pwlnp1, y5pwlnp2, y5pwlnp3, y5pwlnp4, y5pwlnp5,
                          wl, dc, r, t){
  npv1 = 0
  npv2 = 0
  npv3 = 0
  npv4 = 0
  npv5 = 0

  yr1tnp = (wl*y1pwlnp1)*y1np1 + (wl*y1pwlnp2)*y1np2 + (wl*y1pwlnp3)*y1np3 + (wl*y1pwlnp4)*y1np4 +(wl*y1pwlnp5)*y1np5
  yr2tnp = (wl*y2pwlnp1)* y2np1 + (wl*y2pwlnp2)*y2np2 + (wl*y2pwlnp3)* y2np3 + (wl*y2pwlnp4)*y2np4 +(wl*y2pwlnp5)*y2np5
  yr3tnp = (wl*y3pwlnp1)* y3np1 + (wl*y3pwlnp2)*y3np2 + (wl*y3pwlnp3)* y3np3 + (wl*y3pwlnp4)*y3np4 +(wl*y3pwlnp5)*y3np5
  yr4tnp = (wl*y4pwlnp1)* y4np1 + (wl*y4pwlnp2)*y4np2 + (wl*y4pwlnp3)* y4np3 + (wl*y4pwlnp4)*y4np4 + (wl*y4pwlnp5)*y4np5
  yr5tnp = (wl*y5pwlnp1)* y5np1 + (wl*y5pwlnp2)*y5np2 + (wl*y5pwlnp3)* y5np3 + (wl*y5pwlnp4)*y5np4 +(wl*y5pwlnp5)*y5np5

  for (i in 0:t) {
    current = (yr1tnp * (1 + r)^-i)
    #update variable storing sum
    npv1 = npv1 + current
  }
  for (i in seq(from = 1, to = t, by = 5)) {
    current = (yr2tnp * (1 + r)^-i)
    #update variable storing sum
    npv2 = npv2 + current
  }
  for (i in seq(from = 2, to = t, by = 5)) {
    current = (yr3tnp * (1 + r)^-i)
    #update variable storing sum
    npv3 = npv3 + current
  }
  for (i in seq(from = 3, to = t, by = 5)) {
    current = (yr4tnp * (1 + r)^-i)
    #update variable storing sum
    npv4 = npv4 + current
  }
  for (i in seq(from = 4, to = t, by = 5)) {
    current = (yr5tnp * (1 + r)^-i)
    #update variable storing sum
    npv5 = npv5 + current
  }
  pv = npv1 + npv2 + npv3 + npv4 + npv5
  return((pv - dc*wl)/t/wl)
}
