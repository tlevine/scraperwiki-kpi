#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
library(ggplot2)
library(Cairo)

DATEFORMAT = '%b%y'
KPI.OPTS <- opts(
  title = 'ScraperWiki Coder Activity, each line is a user',
  theme_text(family = "sans", face = "bold"),
  panel.background = theme_rect(fill = NA, colour = NA) # Clear background
)

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
kpi.raw$active_time <- as.numeric(difftime(kpi.raw$last_login, kpi.raw$date_joined, units = 'days'))
kpi.raw$proportional_inactivity <- as.numeric(difftime(as.POSIXct('2012-04-24'), kpi.raw$last_login, units = 'days'))/kpi.raw$active_time

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

melt.kpi <- function(
  raw = kpi.raw,
  min.active.days = 0.5
){
  kpi <- melt(raw,
    id.vars = colnames(raw), measure.vars = c('date_joined', 'last_login'),
    variable.name = 'event', value.name = 'datetime'
  )
  colnames(kpi)[-(1:(ncol(kpi)-2))] <- c('event', 'datetime') # Dunno why it doesn't work above

  # Ignore users without low active time because they won't show up
  # and because they'll lead to division by almost zero.
  kpi <- subset(kpi, active_time > min.active.days)

  # Ignore users with a low script-creation rate
  # kpi <- subset(kpi, script_count/active_time > 0.1)

  # Ignore users with a low script count
  # kpi <- subset(kpi, script_count > 2)
  kpi
}

plot.kpi <- function(
  raw,
  sort.yvar = NULL,
  testing = FALSE,
  line.alpha = 0.5,
  ...
){
  # Given the y variable for plotting, return some plots
  if (!is.null(sort.yvar)){
    raw$username <- factor(raw$username, levels = raw$username[order(raw[,sort.yvar])])
  }

  if (testing){
    # Subset for testing
    raw <- raw[sample.int(nrow(raw), 100),]
  }

  kpi <- melt.kpi(raw, ...)

  p <- ggplot(kpi) +
    aes(x = datetime, group = username, color = coder_type) +
    scale_x_datetime('Span of activity, from user registration to most recent login',
      format = DATEFORMAT, major = "3 months", minor = "1 month"
    ) +
    KPI.OPTS +
    geom_line(alpha = line.alpha)

  p
}

# Skip people with few scripts (this is about half of users)
kpi.raw <- subset(kpi.raw, script_count > 2)
p <- plot.kpi(kpi.raw, line.alpha = 0.2)
