#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
library(ggplot2)
library(Cairo)

DATEFORMAT = '%b%y'

raw <- read.csv(
  # This is a dump of the username, date_joined and last_login columns
  # from the auth_users Django table.
  'data/2012-04-23.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])

raw$script_count <- as.numeric(raw$script_count)
raw$last_login <- as.POSIXct(raw$last_login)
raw$date_joined <- as.POSIXct(raw$date_joined)

# Subset for testing
# raw <- raw[sample.int(nrow(raw), 100),]

raw$active_time <- as.numeric(difftime(raw$last_login, raw$date_joined, units = 'days'))
kpi <- melt(raw,
  id.vars = colnames(raw), measure.vars = c('date_joined', 'last_login'),
  variable.name = 'event', value.name = 'datetime'
)
colnames(kpi)[-(1:(ncol(kpi)-2))] <- c('event', 'datetime') # Dunno why it doesn't work above

# Ignore users without low active time because they won't show up
# and because they'll lead to division by almost zero.
kpi <- subset(kpi, active_time > 0.5)

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
# geom_vline(position = as.POSIXct("2010-01-01"), color = 'black') +
  geom_line(color = alpha('black', 0.2))

plots <- list(
  active_time = p + aes(y = active_time) + scale_y_continuous('Users sorted by days of activity'),
  active_time_log = p + aes(y = active_time) + scale_y_log10('Users sorted by days of activity, log scale'),
  script_count = p + aes(y = script_count) + scale_y_continuous('Users sorted by number of scripts'),
  normalized_script_count = p + aes(y = script_count/(active_time)) + scale_y_continuous('Users sorted by number of scripts per day'),
  normalized_script_count_with_names = p + aes(y = script_count/(active_time), color = alpha('black', 0.4)) +
    scale_y_continuous('Users sorted by number of scripts per day') +
    aes(label = username) + geom_text(color = alpha('black', 0.6)), #Usernames
  last_login = p + aes(y = last_login) + scale_y_datetime('Users sorted by date of most recent login', format = DATEFORMAT),
  date_joined = p + aes(y = date_joined) + scale_y_datetime('Users sorted by date joined', format = DATEFORMAT)
)

plot.kpi <- function(){
  # Plot them
  print('Plotting...')

  print('Generating pdfs...')
  Cairo('coder_activity.pdf',
     width = 297, height = 210, units = 'mm',
     pointsize = 10, type = 'pdf'
  )
  l_ply(plots, print)
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
