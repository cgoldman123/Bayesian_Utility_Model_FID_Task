Money.Piecewise.Linear <- function(FID) {
    if (FID > 80) {
        return(0)
    } else if (FID > 60) {
        return(-4 / 5 * FID + 64)
    } else if (FID > 40) {
        return(-1 / 2 * FID + 46)
    } else if (FID > 20) {
        return(-6 / 5 * FID + 74)
    } else {
        return(50)
    }
}

Money.Linear <- function(FID) {
    if (FID > 80) {
        return(0)
    } else {
        return(- 5 / 8 * FID + 50)
    }
}

# for the FID task administered at LIBR, the reward function is separate for each of the predators.
# It is also different for participants with odd/even IDs (i.e., left/right distributions, respectively)
Money.Exponential.Slow.Right = function(FID) {
  money = (2 * floor(round(1.056**((80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Slow.Left = function(FID) {
  money = (2 * floor(round(1.059**((80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Fast.Right = function(FID) {
  money = (2 * floor(round(1.1077**((80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Fast.Left = function(FID) {
  money = (2 * floor(round(1.14**((80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}


# Money.Exponential = function(FID_color_distribution) {
#   
#   if (distribution == "right") {
#     if (color == "slow") {
#       money = (2 * floor(round(1.056**((80 - FID + 15)))))
#     } 
#     if (color == "fast") {
#       money = (2 * floor(round(1.1077**((80 - FID + 15)))))
#     }
#   }
#   if (distribution == "left") {
#     if (color == "slow") {
#       money = (2 * floor(round(1.059**((80 - FID + 15)))))
#     } 
#     if (color == "fast") {
#       money = (2 * floor(round(1.14**((80 - FID + 15)))))
#     }
#   }
#   
#   if (money <= 100) {
#     return(money)
#   }
#   else {
#     return(100)
#   }
#   
# }
# 



