# For the FID task administered at LIBR, the reward function is separate for each of the predators.
# It is also different for participants with odd/even IDs (i.e., left/right distributions, respectively)
Money.Exponential.Slow.Right = function(FID) {
  money = (2 * floor(round(1.056**(round(80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Slow.Left = function(FID) {
  money = (2 * floor(round(1.059**(round(80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Fast.Right = function(FID) {
  money = (2 * floor(round(1.1077**(round(80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}

Money.Exponential.Fast.Left = function(FID) {
  money = (2 * floor(round(1.14**(round(80 - FID + 15)))))
  if (money <= 100) {
    return(money)
  }
  else {
    return(100)
  }
}





