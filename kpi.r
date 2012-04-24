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
  'data/2012-04-23.csv',
  colClasses = c('character')
  #colClasses = c('character', 'Date', 'Date')
)
#print(raw[sample(nrow(raw), 3),])
kpi.raw$script_count <- as.numeric(kpi.raw$script_count)
kpi.raw$last_login <- as.POSIXct(kpi.raw$last_login)
kpi.raw$date_joined <- as.POSIXct(kpi.raw$date_joined)
kpi.raw$active_time <- as.numeric(difftime(kpi.raw$last_login, kpi.raw$date_joined, units = 'days'))

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

plots.bytime <- list(
  active_time = p + aes(y = active_time) + scale_y_log10('Days from join to last login'),
  script_count = p + aes(y = script_count) + scale_y_continuous('Number of scripts'),
  normalized_script_count = p + aes(y = script_count/(active_time)) + scale_y_log10('Number of scripts per day'),
  normalized_script_count_with_names = p + aes(y = script_count/(active_time), alpha = 0.4) +
    scale_y_continuous('Number of scripts per day') +
    aes(label = username) + geom_text(alpha = 0.6), #Usernames
  last_login = p + aes(y = last_login) + scale_y_datetime('Date of most recent login', format = DATEFORMAT),
  date_joined = p + aes(y = date_joined) + scale_y_datetime('Join date', format = DATEFORMAT)
)

# Plot where the y axis is username sorted by sort.yvar
kpi.raw.subset <- subset(kpi.raw, script_count > 2)
plots.byuser <- list(
  last_login = plot.kpi(kpi.raw, 'last_login') + aes(y = username) + scale_y_discrete('Users'),
  last_login_subset = plot.kpi(kpi.raw.subset, 'last_login') + aes(y = username) + scale_y_discrete('Users with more than two scripts'),
  date_joined = plot.kpi(kpi.raw, 'date_joined') + aes(y = username) + scale_y_discrete('Users'),
  date_joined_subset = plot.kpi(kpi.raw.subset, 'date_joined') + aes(y = username) + scale_y_discrete('Users with more than two scripts')
)


plot.longtime <- function(raw, title) {
  ggplot(raw) +
    aes(
      x = last_login, y = active_time, color = coder_type
    ) + KPI.OPTS +
    scale_y_continuous('Days from join to last login') +
    scale_x_datetime('Date of last login',
      format = DATEFORMAT, major = "3 months", minor = "1 month"
    ) +
    geom_point()
}

plots.other <- list(
  coder_type = ggplot(melt.kpi()) + aes(x = coder_type, y = active_time) + geom_jitter() + KPI.OPTS,
  longtime2 = plot.longtime(subset(kpi.raw, script_count >= 3),
    'ScraperWiki Coder Activity, each point is a user with three or more scripts') +
    aes(size = script_count),
  longtime10 = plot.longtime(subset(kpi.raw, script_count >= 10),
    'ScraperWiki Coder Activity, each point is a user with ten or more scripts') +
    aes(size = script_count),
)

plot.kpi.save <- function(){
  # Plot them
  print('Plotting...')

  print('Generating pdfs...')
  Cairo('coder_activity.pdf',
     width = 297, height = 210, units = 'mm',
     pointsize = 10, type = 'pdf'
  )
  l_ply(plots.bytime, print)
  l_ply(plots.byuser, print)
  l_ply(plots.other, print)
  dev.off()

  print('Generating svg...')
  Cairo('coder_activity.svg',
     width = 297, height = 210, units = 'mm',
     pointsize = 10, type = 'svg'
  )
  print(plots.bytime$date_joined)
  dev.off()

  print('Finished plotting')
}
