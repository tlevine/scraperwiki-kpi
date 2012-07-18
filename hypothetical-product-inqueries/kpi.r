#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
#library(ggplot2)
#library(Cairo)

DATEFORMAT = '%b%y'
#KPI.OPTS <- opts(
#  title = 'ScraperWiki Coder Activity, each line is a user',
#  theme_text(family = "sans", face = "bold"),
#  panel.background = theme_rect(fill = NA, colour = NA) # Clear background
#)

kpi.raw <- read.csv(
  # This is a dump of the username, date_joined and last_login columns
  # from the auth_users Django table.
  '2012-07-18.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])
kpi.raw$script_count <- as.numeric(kpi.raw$script_count)
kpi.raw$last_login <- as.POSIXct(kpi.raw$last_login)
kpi.raw$date_joined <- as.POSIXct(kpi.raw$date_joined)
kpi.raw$active_days <- as.numeric(difftime(kpi.raw$last_login, kpi.raw$date_joined, units = 'days'))
kpi.raw$proportional_inactivity <- as.numeric(difftime(as.POSIXct('2012-04-24'), kpi.raw$last_login, units = 'days'))/kpi.raw$active_days

kpi.raw$days_since_login <- as.numeric(difftime(
  as.POSIXct('2012-07-18'),
  kpi.raw$last_login,
  units='days'))


# Set coder type
kpi.raw$coder_type <- (function(raw){
  # Is this a longtime active coder?
  one_month_ago <- as.POSIXct('2012-03-23')
  raw$active <- raw$last_login > one_month_ago
  raw$longtime <- raw$date_joined < one_month_ago
  raw$coder_type <- 'Inactive Coder'
  raw$coder_type[raw$active] <-
    'Pseudo Shorttime Active Coder' # (user who logged in at least once in the last month)'
  raw$coder_type[raw$active & raw$longtime] <-
    'Pseudo Longtime Active Coder' # (active coder who joined in an earlier month)'
  raw$coder_type
})(kpi.raw)

# This gets us
kpi.plot <- function(kpi.raw, coder_type_labels=FALSE) {
  plot(active_days ~ I(0-days_since_login), data=kpi.raw, axes=F,
    xlab='Days since login (Fewer days implies more activeness.)',
    ylab='Days from sign up to last login (More days implies more longtimeness.)',
    main='ScraperWiki coders by longtimeness and activeness',
    pch=21,col=NULL,
    bg=rgb(0,0,0,alpha=0.3)
  )
  thousand <- seq(0, 1000, 100)
  axis(1, labels=thousand, at=-thousand)
  axis(2, labels=thousand, at=thousand)
  if (coder_type_labels) {
    text(
      x=-c(100, 100, 400),
      y= c(800, 200, 200),
      labels=c(
        'Longtime active coders',
        'Shorttime coders',
        'Longtime inactive coders'
      ),
      col=2, font=2, cex=1
    )
  }
}

kpi.sampling.frame <- function(kpi.raw, intercept, slope){
  y <- kpi.raw$active_days
  x <- kpi.raw$days_since_login
  s <- kpi.raw
  s$longtime_activeness <- scale(princomp(data.frame(x,y))$scores[,1])

  s[y > (intercept + slope * x) & (y > 70) ,]
}

# Skip people with few scripts (this is about half of users)
kpi.raw <- subset(kpi.raw, script_count > 2)

# Plot everything
kpi.plot(kpi.raw, coder_type_labels = T)

# Arbitrary make a cutoff for the sampling frame.
kpi.s <- kpi.sampling.frame(kpi.raw, intercept=700, slope=-1.3)

#kpi.plot(kpi.s)
print(paste('Let\'s sample from these', nrow(kpi.s), 'users.'))
