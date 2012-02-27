## ----- Take a look at the README.txt for help
## ----- THIS IS THE PRODUCTION VERSION -----

## get the local directory
datadir <- 'C:/HOTSPOTS/HOTSPOT data'
outdir <- 'C:/HOTSPOTS/HOTSPOT figures'

## this is the directory on the server
dir <- getwd()

## NAs will get introduced during this script for results that have no
## ng.mL. This supresses the warnings since we know and want this to
## happen.

options(warn=-1)

## determine if needed packages are installed or install them if they're missing
packages <- c('plyr', 'colorspace', 'digest', 'ggplot2', 'MASS', 'proto', 'RColorBrewer', 'reshape', 'tcltk')

if(packages %in% library()$results[,1]){
  library(ggplot2)
##  library(tcltk)
}else{
  packs <- list.files(gettextf('%s/packages', dir))
  sapply(packs, function(x){
    ins <- install.packages(gettextf('%s/packages/%s', dir, x), repos=NULL)
  }, simplify=TRUE)
  library(ggplot2)
##  library(tcltk)
}

## input custom theme and needed functions
source(gettextf('%s/files/ggplot2theme.R', dir))
source(gettextf('%s/Production/functions.R', dir))

##-------------- data
# well ids
ids <- read.csv(gettextf('%s/files/wellid.csv', dir))

## find all relevant files
files <- list.files(path=gettextf('%s/', datadir))

## determine if there are multiple files and if they should be pasted together
## only benzos and opis are currently in more than 1 file
## sets doneVal for use later

if(length(files) > 1 & TRUE %in% grepl('opi', files) | length(files) > 1 & TRUE %in% grepl('benz', files)){

  ## count the number of characters in the name to the left of the "."
  y <- nchar(strsplit(files[1], '\\.')[[1]][1])

  ## make a copy of the files variable but trim the letter designating the file sequence (e.g., A, B, etc.)
  filesTrim <- sapply(files, function(x){
    strtrim(x, y-1)
  })

  ## check to see if there are any files that are not true matches that should be pasted together
  if(FALSE %in% grepl(filesTrim[1], filesTrim)){
    doneVal <- 2
  }else{
    doneVal <- 1
  }
}else{
  doneVal <- 2
}

## DEPRECATED CODE -----------
## if(length(files) > 1 & TRUE %in% grepl('opi', files) | length(files) > 1 & TRUE %in% grepl('benz', files)){

##   tt <- tktoplevel()
##   tktitle(tt) <- gettextf('To stitch %s together hit OK.', paste(files, sep=','))

##   done <- tclVar(0)
##   OK.but <- tkbutton(tt, text = 'Stitch them together', command = function() tclvalue(done) <- 1)
##   Cancel.but <- tkbutton(tt, text = "Leave them alone!", command = function() tclvalue(done) <- 2)

##   tkgrid(OK.but, Cancel.but)
##   tkbind(tt, "<Destroy>", function() tclvalue(done) <- 2)
##   tkfocus(tt)

##   tkwait.variable(done)

##   doneVal <- as.integer(tclvalue(done))
##   tkdestroy(tt)
## }else{
##   doneVal <- 2
## }
## DEPRECATED CODE -----------

## determine colnames from the first file
d <- read.delim(gettextf('%s/%s', datadir, files[1]), header=TRUE, sep="\t", nrows=1, skip=6, fill=TRUE, row.names=NULL)

## force it to skip the first column
mycols <- c('NULL', rep('character', length(names(d))-1))

## if files need to be stitched, do it now
if(doneVal == 1){
  fileList <-  lapply(files, function(f){
    read.delim(gettextf('%s/%s', datadir, f), header=TRUE, blank.lines.skip=TRUE, skip=6, fill=TRUE, sep="\t", colClasses=mycols)
    ## how can I get the file name to be a column?? ##
  })

  file <- do.call(rbind, fileList)

  # drop the rows that have repeats of the column headers
  n <- names(file)
  file <- file[-grep(n[1], file$ID),]

  # change the ng.mL and ID to numeric
  file$ID <- as.numeric(file$ID)

  ## remove NAs
  filesub <- subset(file, !is.na(ID) & !is.na(ng.mL))

  filesub$ng.mL <- with(filesub, {
    x <- ng.mL
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    x
  })

  write.table(filesub, file=gettextf('%s/comb_%s', datadir, paste(files, collapse='_')), sep='\t')
  files <- list.files(path=gettextf('%s/', datadir), pattern='comb')
  combo <- TRUE
}else{
  combo <- FALSE
}

# the function that merges the well ids with the data and plots
# iterively for each z in files

graphs <- sapply(files, function(z){

  if(combo){
    dat <- read.delim(gettextf('%s/%s', datadir, z), header=TRUE, row.names=NULL, colClasses=mycols)
  }else{
    dat <- read.delim(gettextf('%s/%s', datadir, z), header=TRUE, blank.lines.skip=TRUE, skip=6, fill=TRUE, sep="\t", colClasses=mycols)
    dat$File <- rep(z, nrow(dat))

    ## drop the rows that have repeats of the column headers
    n <- names(dat)

    dat <- dat[-grep(n[1], dat$ID),]
  }

  ## change the ng.mL and ID to numeric
  dat$ID <- as.numeric(dat$ID)

  ## remove NAs
  datsub <- subset(dat, !is.na(ID) & !is.na(ng.mL))

  datsub$ng.mL <- with(datsub, {
    x <- ng.mL
    x <- as.numeric(x)
    x[is.na(x)] <- 0
    x
  })

  ## merge the dataframes
  datm <- merge(datsub, ids, by.x='ID', by.y='ID', all.x=TRUE)
  datm <- subset(datm, !is.na(Well.Position))

  ## split the well column for plotting as a grid
  datm$Row <- with(datm, {
    y <- strsplit(as.character(datm$Well.Position), ', ')
    v <- unlist(lapply(y, '[', 1))
    v
  })

  ## make a new column with the column id
  datm$Column <- with(datm, {
    y <- strsplit(as.character(datm$Well.Position), ', ')
    v <- unlist(lapply(y, '[', 2))
    as.numeric(v)
  })

  ## make a new column with the row id
  datm$Row <- factor(datm$Row,
                     levels=LETTERS[8:1]
                     )

  ## Fix the names to display better when plotting
  datm$Analyte <- with(datm, {
    x <- Name
    x[Name == 'ritalin (methylphenidate, qual only)'] <- 'ritalin, methylphenidate'
    x[Name == 'ritalinic acid (qual only)'] <- 'ritalinic acid'
    x[Name == 'N-desmethylselegiline (qual only)'] <- 'N-desmethylselegiline'
    x[Name == 'selegiline (qual only)'] <- 'selegiline'
    x[Name == 'psuedoephedrine (qual only)'] <- 'pseudoephedrine'
    x[Name == 'phentermine (qual only)'] <- 'phentermine'
    x
  })

  ## TODO: work on fixing the names in a more robust way
  ## datm$Analyte <- with(datm, function(x){
  ##   y <- rep(NA, nrow(x))
  ##   y <- strsplit(


  ## subset the data into ISD and Samples
  datmS <- subset(datm, IS.Area!='')
  datmI <- subset(datm, IS.Area=='')

  ## bin the data using determined analyte to reduce display complexity - analyte specific values

  ## get the appropriate flags for the plots
  flagging <- flags(datmS, z)
  datmS$Flag <- unlist(flagging[[1]])

  datmI$X.DevNum <- as.numeric(datmI$X.Dev)
  ## switch neg to pos since < > doesn't seem to work with neg numbers
  datmI$X.DevNum <- datmI$X.DevNum * -1

  datmI$Flag <- with(datmI, {
    x <- rep(NA, nrow(datmI))
    x[X.DevNum <= 29.9] <- 1
    x[X.DevNum > 29.9 & X.Dev <= 39.9] <- 2
    x[X.DevNum > 39.9 & X.Dev <= 49.9] <- 3
    x[X.DevNum > 49.9] <- 4
    x[is.na(X.DevNum)] <- 4

    ## use original values since this is a positive limit!!
    x[as.numeric(X.Dev) >= 100] <- 5
    x <- factor(x, levels=c(1:5))
  })

  labsIS <- c('0 to -29.9','-39.9 to -29.9', '-49.9 to -39.9','-49.9 or more','> +100')
  kellersIS <- c('grey95', 'green', 'yellow', 'red', 'purple')
  brksIS <- c(1:5)

##  save(datmS, datmI, file='Beta/outfiles/example.rda')
  ## ## ## interpretations --------------------------------------------
  ##datmS$trueConc <- datmS$ng.mL * as.numeric(datmS$Dilution)
  datmS$plateloc <- paste(datmS$Row, datmS$Column, sep='')

  source(gettextf('%s/Beta/functions.R', dir))

  params <- suspects(datmS, c('purple', 'red'))

  datmS <- merge(datmS, params, by.x=c('plateloc','Analyte'), by.y=c('wells','drugs'), all.x=TRUE)

  datmS$Original <- with(datmS, {
    x <- as.character(hot)
    x[is.na(x)] <- 'grey95'
    x
  })

  datmS$InterpFlag <- with(datmS, {
    x <- rep('grey95', nrow(datmS))
    x[Original!=Flag & Original =='red' & Flag=='green'] <- 'red'
    x[Original!=Flag & Original =='red' & Flag=='grey50'] <- 'grey50'
    x[Original!=Flag & Original == 'purple' & Flag=='green'] <- 'purple'
    x[Original!=Flag & Original == 'purple' & Flag=='orange'] <- 'purple'
    x[Original!=Flag & Original == 'purple' & Flag=='grey50'] <- 'grey50'
    x <- factor(x, levels=c('grey95', 'grey50', 'red', 'purple'))
    x
  })

  labsInterp <- c('Report result', 'Re-extract S/P', 'Re-extract L', 'Re-extract H')
  kellersInterp <- c('grey95', 'grey50', 'red', 'purple')
  brksInterp <- kellersInterp

  ## ----- plots -------

  ## the Samples plot
  gg <- ggplot(datmS, aes(x=Column, y=Row, color=Flag, label=ID))

  g <- gg + geom_point(size=5)+
    geom_text(colour='black', size=2.5)+
    theme()+
      facet_wrap(~Analyte)+
        scale_colour_manual(values=unlist(flagging[['kellers']]), breaks=unlist(flagging[['brks']]), labels=unlist(flagging[['labs']]), name='Concentration')+
          scale_x_continuous(breaks=unique(datmS$Column))+
            opts(title=gettextf('Samples for file %s', z))

  ## output the file with z specific parameters
  pdf(file=gettextf('%s/%s_Samples.pdf', outdir, z), width=8.5, height=6.5)
  print(g)
  dev.off()

  ## ## the Interpretation plot
  gg <- ggplot(datmS, aes(x=Column, y=Row, color=InterpFlag, label=ID))

  g <- gg + geom_point(size=5)+
    geom_text(colour='black', size=2.5)+
    theme()+
      facet_wrap(~Analyte)+
        scale_colour_manual(values=kellersInterp, breaks=brksInterp, labels=labsInterp, name='Interpretation')+
          scale_x_continuous(breaks=unique(datmS$Column))+
            opts(title=gettextf('Sample Interpretations for file %s', z))

  ## output the file with z specific parameters
  pdf(file=gettextf('%s/%s_Sample_Interpretations.pdf', outdir, z), width=8.5, height=6.5)
  print(g)
  dev.off()

  ## the IS plot
  gg <- ggplot(datmI, aes(x=Column, y=Row, color=Flag, label=ID))

  g <- gg + geom_point(size=5)+
    geom_text(colour='black', size=2.5)+
    theme()+
        facet_wrap(~Analyte)+
          scale_colour_manual(values=kellersIS, breaks=brksIS, labels=labsIS, name='% Deviation')+
            scale_x_continuous(breaks=unique(datmI$Column))+
              opts(title=gettextf('Internal Standards for file %s', z))

  ## output the file with z specific parameters
  pdf(file=gettextf('%s/%s_Internal-Standards.pdf', outdir, z), width=8.5, height=6.5)
  print(g)
  dev.off()

})

