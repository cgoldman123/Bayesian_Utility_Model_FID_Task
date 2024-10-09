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

