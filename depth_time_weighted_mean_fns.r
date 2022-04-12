################################################################################
################################################################################
#
# Useful functions for generating volume-weighted means of variables
# and time weighted means...
#
# interpolate measured profiles to 1m intervals with interprofile()
# then calculate the volume weighted means for certain layers (or depths)
#   using vert_means() or vert_means2()
# then interpolate the (weighted) means over time to daily intervals with daily_vals()
# then generate some grouping factors for the time series to aggregate the data,
#   like seasons or months, using stechlin_factors()
# finally calculate the mean values of the variables (seasonal, monthly,
#   mixing related, etc) for each year using var_means()
#
#  Tom Shatwell, 14.10.2013
#
################################################################################
################################################################################

tz <- "UTC"

################################################################################
# interprofile()
#   interpolates depth profiles linearly to fixed (eg 1m) depth intervals
#
# input:
#   data: a data frame with columns containing profile data with one profile 
#     per row. Each column corresponds to measurements from a particular depth.
#     Optionally the first column can contain an identifier (like sampling date), 
#     then set date.column = TRUE
#   raw_depths: the depths corresponding to each column of data (except the first 
#     date column if date.column = TRUE)
#   depths: the depths to interpolate to (vector)
#   min.valid.meas: minimum number of measurements per profile for the profile to be included
#     All profiles with less than min.valid.meas are excluded. If 0, then the function returns 
#     an error if there are not at least 2 non NA measurements per profile
#   date.column: logical, Does the input data have a date column (must be the first)?
#
# output: a data frame with the first column the same identifier(sampling date)
#     as "data" and one column for each depth in depths. Ie each row contains 
#     the interpolated profile
#
#  default is to interpolate each profile to 1m intervals
#
# interprofile2()
#   same as interprofile() but takes original data in list (long) format.
#   
#   input: 
#     data: data frame with (at least) 3 columns containing 
#       (1) an identifier like the time as integer or date in POSIX format
#       (2) the depths
#       (3) the values
#     date_col, depth_col and value_col give the name (as character) or column 
#       number of the columns with the id/dates, depths, and values, respectively.
#
################################################################################

interprofile <- function(data, raw_depths, depths, min.valid.meas=0, 
                         date.column=TRUE) {
  dat1 <- data
  if(date.column) dat1 <- data[,-1]
  num.NAs <- apply(apply(dat1, 2, is.na), 1, sum) # count NAs in each profile
  ind <- num.NAs <= (ncol(dat1) - min.valid.meas)
  dat <- dat1[ind,] # exclude profiles(rows) with too many NAs 
  out <- matrix(NA, nrow=nrow(dat), ncol=length(depths))
  for(ii in 1:nrow(dat)) {
    out[ii,]<- approx(x=raw_depths, y=dat[ii,], xout=depths,
      method="linear", rule=2)$y
  }
  out <- data.frame(out)
  names(out) <- paste("m", depths, sep="")
  if(date.column) {
    out <- data.frame(data[ind,1], out)
    names(out)[1] <- names(data)[1]
  }
  return(out)
}

################################################################################

interprofile2 <- function(data, date_col=1, depth_col=2, value_col=3, depths,
                          min.valid.meas=0) {
  working <- as.data.frame(data[,c(date_col, depth_col, value_col)])
  names(working) <-c("date", "depth", "value")
  working$valid <- is.na(working$value)==FALSE
  chk <- aggregate(valid~date, working, sum)
  valid.dates <- subset(chk, valid >= min.valid.meas)$date
  ndates <- length(valid.dates)
  out1 <- matrix(NA, nrow=ndates, ncol=length(depths))
  cnt <- 0
  for(ii in 1:ndates) {
    profil <- subset(working, date==valid.dates[ii])
    out1[ii,]<- approx(x=profil$depth, y=profil$value, xout=depths,
      method="linear", rule=2)$y
    cnt <<- cnt + 1
    if (cnt > 99) {
      cnt <<- 0
      cat("completed ", ii, " of ", ndates, "\n")
    }
  }
  out <- data.frame(valid.dates, out1)
  names(out) <- c(names(data[date_col]), paste("m", depths, sep=""))
  return(out)
}


################################################################################
# vert_means()
#
# input:
#    data: data frame with 1st column as sampling date (identifier) and other
#     columns represent values at a certain depth eg output from interprofile(). 
#     One row represetns a profile.
#   top: depth of top of layer for volume-weighted mean (numeric),
#     can be a vector if more than one layer is to be calculated
#   bot: depth of bottom of layer of vol-weighted mean (numeric),
#     can be a vector if more than one layer is to be calculated (same length as top)
#   depths: vector of depths corresponding to the depths of the columns of data
#     ie length(depths) = ncol(data)-1
#   hfunc: function of hypsographic curve. See ?vertmean in marelac package.
#   level: the water level (default is 70m = Stechinsee is full)
#
# output:
#   a data frame containing the first column of data (sampling dates)
#     and the depth-weighted means for the given layer. One row for each sampling date
#
# vert_means2()
#   this is the same as vert_means() except that top and bot can vary over time 
#     to account for variable layer thicknesses and locations (eg epilimnion etc).
#     top and bot have to be
#     named data frames containing the coordinates of the layers (time and depth) for
#     volume-weighted means. Both top and bot need to have the same number of
#     rows as "data". Each column in top and bot corresponds to a layer, which is
#     identified by the column name of top or bot.
#
#
#
################################################################################

vert_means <- function(data, top, bot, depths, hfunc, level=70) {
  require(marelac)
  out <- matrix(NA, nrow(data), ncol=length(top))
  for(ii in 1:nrow(data)) {
    for(jj in 1:length(top)) {
      out[ii,jj] <- vertmean(depth=depths, vari= as.vector(t(data[ii, -1])),
                        level=level, top=top[jj], bot=bot[jj], vol=hfunc)
    }
  }
  out1 <- data.frame(data[,1], out)
  names(out1) <- c(names(data)[1], paste("m", top, "_", bot, sep=""))
  return(out1)
}

################################################################################

vert_means2 <- function(data, top, bot, depths, hfunc, level=70) {
  require(marelac)
  out <- matrix(NA, nrow(data), ncol=ncol(top))
  for(ii in 1:nrow(data)) {
    for(jj in 1:ncol(top)) {
      out[ii,jj] <- vertmean(depth=depths, vari= as.vector(t(data[ii, -1])),
                      level=level, top=top[ii,jj], bot=bot[ii,jj], vol=hfunc)
    }
  }
  out1 <- data.frame(data[,1], out)
  names(out1) <- c(names(data)[1], names(top))
  return(out1)
}

################################################################################
#as above but bthA and bthD are vectors containing hypsographic info of areas
#and the corresponding depths. length.out is the number of internal depths
#between top and bottom to interpolate to

vert_means3 <- function(data, top, bot, depths, bthA, bthD, length.out=101) {
  dat <- data
  if(is.vector(data)) dat <- t(as.matrix(data))
  Z1 <- seq(top, bot, length.out=length.out)
  dz <- (bot - top) / (length.out - 1)
  Z  <- Z1[-length(Z1)] + dz/2
  A  <- approx(x=bthD, y=bthA, xout=Z, rule=2)$y
  V  <- A * dz
  X  <- matrix(NA, nrow=nrow(dat), ncol=length(Z))
  for(ii in 1:nrow(X)) X[ii,] <- approx(x=depths, y=dat[ii,], xout=Z, rule=2)$y
  out <- X %*% V / sum(V)
  if(is.vector(data)) out <- as.numeric(out)
  return(out)
}
  

################################################################################
# daily_vals()
#   generates daily values of a variable(s)
#
# input:
#   data: data frame containing the data (1st column has the date in POSIX)
#     other columns contain data
#   dateseq: a vector in POSIX format containing the dates (best daily reolution)
#     to interpolate to
#
# output:
#   a data frame with the number of rows equal to the length of dateseq
#   and one column each for each column of data
#
################################################################################

daily_vals <- function(data, 
                       dateseq=seq(ISOdate(as.POSIXlt(data[1,1])$year+1900, 1,1,0, tz="UTC"),
                                   ISOdate(as.POSIXlt(data[nrow(data),1])$year+1900,12,31,0,tz="UTC"), 
                                   "day"), ...) {
  out <- data.frame(date=as.POSIXct(dateseq))
  for(ii in 2:ncol(data)) {
    out <- cbind(out,
      approx(x=as.POSIXct(data[,1]), y=data[,ii], xout=as.POSIXct(dateseq), ...)$y)
  }
  names(out) <- c("date", names(data)[-1])
  return(out)
}

################################################################################
# stechlin_factors()
#   generates grouping factors according to time (eg month, season, whether 
#  stratified etc)
#
# input:
#   dateseq: a vector in POSIX format containing the dates for which factors 
#           should be returned
#   strat_start : a vector in POSIX format with the dates of start of stratification
#   strat_end : a vector in POSIX format with the dates of end of stratification
#   ice_data : a data.frame containing ice data, with a column called "date" 
#             containing the dates in POSIX format, and a column called "ice80",
#             containing TRUE or FALSE depending on whether the lake was more 
#             than 80% ice covered
#             If ice_data is empty, the factor defaults to the normal stratification
#
# output:
#   a data frame with the number of rows equal to the length of dateseq
#   and one column each for
#     "year": the year of each value of datseq,
#     "month": the month of each value of datseq,
#     "season": the season of each value of datseq
#       (winter:jan-mar, spring: apr-jun, summer: jul-sep, autumn: oct-dec),
#     "mix": whether the lake was mixed or stratified on each value of datseq
#
################################################################################

stechlin_factors <- function(dateseq, strat_start, strat_end, ice_data=NA) {
  stratfun <- approxfun(x=c(strat_start, strat_end),
                      y=c(rep(1, length(strat_start)), rep(0, length(strat_end))),
                      method="constant", f=0, rule=1)
  icefun <- approxfun(x=ice_data$date, y=ice_data$ice80,
                      method="constant", f=0, rule=1)
  year  <- as.POSIXlt(dateseq)$year+1900
  month <- format(dateseq, "%b")
  month <- as.POSIXlt(dateseq)$mon+1
  season <- rep("winter", length(dateseq))
  season[month%in%4:6]<-"spring"
  season[month%in%7:9]<-"summer"
  season[month%in%10:12]<-"autumn"
  mix  <- rep("mixed", length(dateseq))
  mix[stratfun(dateseq)==1] <- "stratified"
  mix_ice<-mix
  mix_ice[mix_ice=="mixed"]<- "mixed_no_ice"
  #if(ice_data!=NA) 
  mix_ice[icefun(dateseq)==1] <- "ice"
  addyear <- rep(0, length(year))
  for(zz in 1:length(strat_end)) {
    ind <- which(dateseq>=strat_end[zz] &
      dateseq < ISOdate(as.POSIXlt(strat_start[zz])$year+1901,1,1,0, tz=tz))
    addyear[ind]<-1
  }
  mixyear <- year+addyear
  return(data.frame(year, month, season, mix, mix_ice, mixyear))
}


################################################################################
# var_means()
#   calculates the mean values of variables for each year, averaged over a
#   certain period like season, etc.
#
# input:
#     dat: a data frame with the first column a date in POSIX format,
#          and additional named columns containing the data
#     factor: a character or number vector with length equal to
#          the number of rows of dat (eg from stechlin_factors())
#     year: a vector of years of length equal to nrow(data). Leave out for all
#         factors except mix, in which case set year=mixyear.
#         This corrects the year so that the mixing at the end of one year 
#         counts towards the following year. Ignore the warning message.
#     NA.rm: ignore NAs when calculating the mean? TRUE/FALSE
# output:
#     a data frame containing annual means with the first column the year
#       and then an additional column for each level of factor containing the
#       means for that variable and level
#
# requires: reshape package
#
################################################################################

var_means <- function(dat, factor, year=NA, NA.rm=FALSE, fun=mean) {
  if(is.na(year)) {
    year1 <- as.POSIXlt(dat[,1])$year+1900
  } else {
    year1 <- year
  }
  out  <- aggregate(list(dat[,-1]), list(year=year1, factor=factor), fun, na.rm=NA.rm)
  names(out)[-1:-2] <- names(dat)[-1]
  require(reshape)
  return(cast(melt(out, id=1:2), year~variable+factor))
}


## calculates volume weighted mean of "var" between depths top and bot as a 
## time series, using simulation output "out". For hfunc see vertmean in marelac
vol_mean <- function(var, top, bot, out, hfunc, dZ=0.5) {
  depths <- seq(top, bot, dZ)
  vols <- hfunc(depths - dZ/2, 70) - hfunc(depths + dZ/2, 70)
  inds <- match(depths, (seq(0.5,70,0.5) - 0.5))
  conc <- out[, var_ind(var)][,inds]
  vtot <- sum(vols)
  return((conc %*% vols)/vtot)
} 


# detrend a time series
# detrend a vector
detrend.v <- function(data, times=1:length(data)) {
  r1 <- lm(data[,2]~data[,1])
  r2 <- lm(data[,2]~1)
  return(residuals(r1)+coef(r2))
}

# this function detrends a data set
# data: a vector, matrix or data.frame containing data to be detrended
# times: an optional vector of times corresponding to the rows/elements in data.
#   If not specified, defaults to equal intervals in time between data points
# timecol: optional column number or name if the times are included in data 

detrend <- function(data, times = 1:nrow(as.data.frame(data)), timecol=NA) {
  data1 <- as.data.frame(data)
  if(!is.na(timecol)) {
    times <- data[,timecol]
    data1 <- as.data.frame(data[,-timecol])
    names(data1) <- names(data)[-timecol]
  }
  out<- NULL
  for(ii in 1:ncol(data1)) {
    r1 <- lm(data1[,ii]~times)
    r2 <- lm(data1[,ii]~1)
#     out1<- (residuals(r1)+coef(r2))
    out1 <- data1[,ii] - predict(r1, newdata = data.frame(times=times)) +coef(r2)
    out <- cbind(out, out1)
  }
  out <- as.data.frame(out)
  names(out) <- names(data1)
  rownames(out) <- rownames(data1)
  if(!is.na(timecol)) {
    out <- cbind(data[,timecol], out)
    names(out)[1] <- names(data)[timecol]
#     out<- cbind(out[,0:(timecol-1)], data[,timecol], out[, timecol:ncol(out)])
#     names(out) <- c(names(out)[0:(timecol-1)], names(data)[timecol], names(out)[timecol:ncol(out)])
#       names(data)[timecol]
  }
  return(out)
}
