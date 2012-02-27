## ----- Take a look at the README.txt for help
## ----- THIS IS THE PRODUCTION VERSION -----

## functions for interpretations

suspects <- function(dat, m){

  ## get all the max samples
  maxes <- which(dat$Flag %in% m)

  l <- length(maxes)
  comps <- dat$Analyte[maxes]

  x <- NA
  y <- NA
  z <- NA
  rowz <- NA

  ## find grid
  for(i in 1:l){

#    datsub <- subset(dat, Analyte==comps[i])

    r <- dat$Row[maxes[i]]
    col <- dat$Column[maxes[i]]

    y <- append(y, rep(comps[i], 8))
 #   y <- append(y, rep(comps[i], length(unique(dat$Analyte))))

    x <- append(x,paste(LETTERS[which(LETTERS==r)-1], col, sep=''))
    x <- append(x,paste(LETTERS[which(LETTERS==r)+1], col, sep=''))
    x <- append(x,paste(r, col-1, sep=''))
    x <- append(x,paste(r, col+1, sep=''))
    x <- append(x,paste(LETTERS[which(LETTERS==r)-1], col+1, sep=''))
    x <- append(x,paste(LETTERS[which(LETTERS==r)-1], col-1, sep=''))
    x <- append(x,paste(LETTERS[which(LETTERS==r)+1], col+1, sep=''))
    x <- append(x,paste(LETTERS[which(LETTERS==r)+1], col-1, sep=''))

    z <- append(z, rep(as.character(dat$Flag[maxes[i]]), 8))

  }
  x <- x[-1]
  y <- y[-1]
  z <- z[-1]


  results <- data.frame(wells=x, drugs=y, hot=z)
  return(results)
}

flags <- function(dat, z){

  ## I could make a config file that has all this info, but should I?
  if(regexpr('amp', z) > 0){

    kellers <- c('grey95','grey50', 'green', 'orange', 'yellow', 'red','purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 20] <- 'grey95'
      x[ng.mL > 20 & ng.mL <= 200] <- 'grey50'
      x[ng.mL > 200 & ng.mL <= 1000] <- 'green'
      x[ng.mL > 1000 & ng.mL <= 2000] <- 'orange'
      x[ng.mL > 2000 & ng.mL <= 5000] <- 'yellow'
      x[ng.mL > 5000 & ng.mL <= 25000] <- 'red'
      x[ng.mL > 25000] <- 'purple'
      x[IMR.Fail.=='YES' | RT.Fail.=='YES'] <- 'grey95'
      x <- factor(x, levels=kellers)
    })

    ## get the right labels, colors and breaks for the plot
    labs <- c('< 20','20 to 200','200 to 1000', '1000 to 2000','2000 to 5000','5000 to 25,000','> 25,000')
    brks <- kellers

  }else if(regexpr('mtd', z) > 0){
    kellers <- c('grey95','green', 'orange', 'yellow', 'red','purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 10] <- 'grey95'
      x[ng.mL > 10 & ng.mL <= 100] <- 'green'
      x[ng.mL > 100 & ng.mL <= 1000] <- 'orange'
      x[ng.mL > 1000 & ng.mL <= 5000] <- 'yellow'
      x[ng.mL > 5000 & ng.mL <= 50000] <- 'red'
      x[ng.mL > 50000] <- 'purple'
      x[IMR.Fail.=='NO'] <- 'grey95'
      x <- factor(x, levels=kellers)
    })

    ## get the right labels, colors and breaks for the plot
    labs <- c('< 10', '10 to 100', '100 to 1000', '1000 to 5000', '5000 to 50,000', '> 50,000')
    brks <- kellers

  }else if(regexpr('opi', z) > 0){

    kellers <- c('grey95','grey50','green', 'orange', 'yellow', 'red','purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 2] <- 'grey95'
      x[ng.mL > 2 & ng.mL <= 5] <- 'grey50'
      x[ng.mL > 5 & ng.mL <= 25] <- 'green'
      x[ng.mL > 25 & ng.mL <= 200] <- 'orange'
      x[ng.mL > 200 & ng.mL <= 5000] <- 'yellow'
      x[ng.mL > 5000 & ng.mL <= 25000] <- 'red'
      x[IMR.Fail.=='YES' | RT.Fail.=='YES'] <- 'grey95'
      x[ng.mL > 25000] <- 'purple'
      x <- factor(x, levels=kellers)
    })
    ## get the right labels, colors and breaks for the plot
    labs <- c('< 2','2 to 5','5 to 25','25 to 200','200 to 5000', '5000 to 25,000','> 25,000')
    brks <- kellers

  }else if(regexpr('coc', z) > 0){
    kellers <- c('grey95', 'green', 'orange', 'yellow', 'red', 'purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 20] <- 'grey95'
      x[ng.mL > 20 & ng.mL <= 100] <- 'green'
      x[ng.mL > 100 & ng.mL <= 1000] <- 'orange'
      x[ng.mL > 1000 & ng.mL <= 5000] <- 'yellow'
      x[ng.mL > 5000 & ng.mL <= 50000] <- 'red'
      x[ng.mL > 50000] <- 'purple'
      x[IMR.Fail.=='NO'] <- 'grey95'
      x <- factor(x, levels=kellers)
    })

    ## get the right lables, colors and breaks for the plot
    labs <- c('< 10', '10 to 100', '100 to 1000', '1000 to 5000', '5000 to 50,000', '> 50,000')
    brks <- kellers

  }else if(regexpr('benz', z) > 0){
    kellers <- c('grey95', 'green', 'orange', 'yellow', 'red', 'purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 20] <- 'grey95'
      x[ng.mL > 20 & ng.mL <= 40] <- 'green'
      x[ng.mL > 40 & ng.mL <= 200] <- 'orange'
      x[ng.mL > 200 & ng.mL <= 5000] <- 'yellow'
      x[ng.mL > 5000 & ng.mL <= 50000] <- 'red'
      x[ng.mL > 50000] <- 'purple'
      x[IMR.Fail.=='NO'] <- 'grey95'
      x <- factor(x, levels=kellers)
    })

    ## get the right lables, colors and breaks for the plot
    labs <- c('< 20', '20 to 40', '40 to 100', '100 to 5000', '5000 to 50,000', '> 50,000')
    brks <- kellers

  }else if(regexpr('thc', z) > 0){
    kellers <- c('grey95', 'green', 'orange', 'yellow', 'red', 'purple')
    flag <- with(dat, {
      x <- rep(NA, nrow(dat))
      x[ng.mL <= 5] <- 'grey95'
      x[ng.mL > 5 & ng.mL <= 20] <- 'green'
      x[ng.mL > 20 & ng.mL <= 100] <- 'orange'
      x[ng.mL > 100 & ng.mL <= 1000] <- 'yellow'
      x[ng.mL > 1000 & ng.mL <= 10000] <- 'red'
      x[ng.mL > 10000] <- 'purple'
      x[IMR.Fail.=='YES' | RT.Fail.=='YES'] <- 'grey95'
      x <- factor(x, levels=kellers)
    })

    ## get the right lables, colors and breaks for the plot
    labs <- c('< 20', '20 to 40', '40 to 100', '100 to 5000', '5000 to 50,000', '> 50,000')
    brks <- kellers
  }

  results <- list(flag=flag, kellers=kellers, brks=brks, labs=labs)
  return(results)
}

