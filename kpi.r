#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
library(ggplot2)
library(Cairo)

DATEFORMAT = '%b%y'

kpi.raw <- read.csv(
  # This is a dump of the username, date_joined and last_login columns
  # from the auth_users Django table.
  'data/2012-04-23.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])
kpi.raw$script_count <- as.numeric(kpi.raw$script_count)
kpi.raw$last_login <- as.POSIXct(kpi.raw$last_login)
kpi.raw$date_joined <- as.POSIXct(kpi.raw$date_joined)
kpi.raw$active_time <- as.numeric(difftime(kpi.raw$last_login, kpi.raw$date_joined, units = 'days'))

plot.kpi <- function(
  raw,
  sort.yvar = NULL,
  testing = FALSE,
  min.active.days = 0.5
){
  # Given the y variable for plotting, return some plots
  if (!is.null(sort.yvar)){
    raw$username <- factor(raw$username, levels = raw$username[order(raw[,sort.yvar])])
  }

  if (testing){
    # Subset for testing
    raw <- raw[sample.int(nrow(raw), 100),]
  }

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

  p <- ggplot(kpi) +
    aes(x = datetime, group = username) +
    scale_x_datetime('Span of activity, from user registration to most recent login',
      format = DATEFORMAT, major = "3 months", minor = "1 month"
    ) +
    opts(
      title = 'ScraperWiki Coder Activity',
      theme_text(family = "sans", face = "bold"),
      panel.background = theme_rect(fill = NA, colour = NA) # Clear background
    ) +
    geom_line(color = alpha('black', 0.2))

  p
}

p <- plot.kpi(kpi.raw)

plots <- list(
  active_time = p + aes(y = active_time) + scale_y_continuous('User\'s days of activity'),
  active_time_log = p + aes(y = active_time) + scale_y_log10('User\'s days of activity, log scale'),
  script_count = p + aes(y = script_count) + scale_y_continuous('User\'s number of scripts'),
  normalized_script_count = p + aes(y = script_count/(active_time)) + scale_y_continuous('User\'s number of scripts per day'),
  normalized_script_count_with_names = p + aes(y = script_count/(active_time), color = alpha('black', 0.4)) +
    scale_y_continuous('User\'s number of scripts per day') +
    aes(label = username) + geom_text(color = alpha('black', 0.6)), #Usernames
  last_login = p + aes(y = last_login) + scale_y_datetime('User\'s date of most recent login', format = DATEFORMAT),
  date_joined = p + aes(y = date_joined) + scale_y_datetime('User\'s join date', format = DATEFORMAT)
)

# Plot where the y axis is username sorted by sort.yvar
kpi.raw.subset <- subset(kpi.raw, script_count > 2)
plots.byuser <- list(
  last_login = plot.kpi(kpi.raw, 'last_login') + aes(y = username) + scale_y_discrete('Users'),
  last_login_subset = plot.kpi(kpi.raw.subset, 'last_login') + aes(y = username) + scale_y_discrete('Users with more than two scripts'),
  date_joined = plot.kpi(kpi.raw, 'date_joined') + aes(y = username) + scale_y_discrete('Users'),
  date_joined_subset = plot.kpi(kpi.raw.subset, 'date_joined') + aes(y = username) + scale_y_discrete('Users with more than two scripts')
)

plot.kpi.save <- function(plotslist = plots){
  # Plot them
  print('Plotting...')

  print('Generating pdfs...')
  Cairo('coder_activity.pdf',
     width = 297, height = 210, units = 'mm',
     pointsize = 10, type = 'pdf'
  )
  l_ply(plotslist, print)
  dev.off()

  print('Generating svg...')
  Cairo('coder_activity.svg',
     width = 297, height = 210, units = 'mm',
     pointsize = 10, type = 'svg'
  )
  print(plots$date_joined)
  dev.off()

  print('Finished plotting')
}
