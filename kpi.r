#!/usr/bin/env Rscript

library(reshape2)
library(plyr)
library(ggplot2)
library(Cairo)

raw <- read.csv(
  # This is a dump of the username, date_joined and last_login columns
  # from the auth_users Django table.
  'data/2012-04-23.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])

raw$last_login <- as.POSIXct(raw$last_login)
raw$date_joined <- as.POSIXct(raw$date_joined)

# Subset for testing
raw <- raw[sample.int(nrow(raw), 100),]

raw$active_time <- as.numeric(difftime(raw$last_login, raw$date_joined, units = 'days'))
kpi <- melt(raw,
  id.vars = colnames(raw), measure.vars = c('date_joined', 'last_login'),
  variable.name = 'event', value.name = 'datetime'
)
colnames(kpi)[-(1:(ncol(kpi)-2))] <- c('event', 'datetime') # Dunno why it doesn't work above

p <- ggplot(kpi) +
  aes(x = datetime, group = username) +
  scale_x_datetime('Span of activity, from user registration to most recent login') +
  opts(
    title = 'ScraperWiki Coder Activity',
    theme_text(family = "sans", face = "bold")
  ) +
# aes(label = username) + geom_text() + #Usernames
  geom_line(color = alpha('black', 0.1))

plots <- list(
  duration = p + aes(y = active_time) + scale_y_continuous('Users sorted by length of activity'),
  last_login = p + aes(y = last_login) + scale_y_datetime('Users sorted by date of most recent login'),
  date_joined = p + aes(y = date_joined) + scale_y_datetime('Users sorted by date joined login')
)

# Plot them
print('Plotting...')

#pdf('coder_activity.pdf', paper = 'a4r', fonts = 'Helvetica')
# Cairo kerns fonts properly
Cairo('coder_activity.pdf',
   width = 297, height = 210, units = 'mm',
   pointsize = 10, type = 'pdf'
)
l_ply(plots, print)
dev.off()

print('Finished plotting')
