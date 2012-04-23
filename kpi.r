#!/usr/bin/env Rscript
raw <- read.csv(
  # This is a dump of the username, date_joined and last_login columns
  # from the auth_users Django table.
  'data/2012-04-23.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])

raw$last_login <- as.POSIXlt(raw$last_login)
raw$date_joined <- as.POSIXlt(raw$date_joined)

kpi <- melt(raw, 'username', variable.name = 'event', value.name = 'datetime')
colnames(kpi)[2:3] <- c('event', 'datetime') # Dunno why it doesn't work above

#rownames(raw) <- raw$username
#raw$username <- NULL
