rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)

dt_orig = fread('jira_clean.tsv', sep = '\t')
setnames(dt_orig, c('time_string', 'unix_time', 'instance', 'product', 'username', 'event', 'attributes'))


# Initial exploration
# dt_orig[, .(instance_count = uniqueN(instance)), username][order(-instance_count)]
# dt_orig[, .N, event][order(-N)] %>% View
# dt_orig[, .N, username]
# dt_orig[username=='b4d3faf5d77a48fedebe57e32ee749a9',] %>% View

# First compute the time difference between two events
dt = dt_orig[, .(time_string, unix_time, instance, product, username, event)]
setorderv(dt, c('instance', 'product', 'username', 'unix_time'))
dt = dt[, prev_time := shift(unix_time, 1L, type = 'lag', fill = NA_integer_), by = .(instance, username, product)]
dt = dt[, prev_time_str := shift(time_string, 1L, type = 'lag', fill = NA_integer_), by = .(instance, username, product)]
dt = dt[, prev_event := shift(event, 1L, type = 'lag', fill = NA_integer_), by = .(instance, username, product)]
dt[prev_time == 0, prev_time := NA_integer_]
dt[, time_diff := unix_time - prev_time, by = .(instance, username, product)]

# View(dt[, .(instance, username, product, unix_time, prev_time, time_string, event, prev_event, time_diff)])

# Generate similar times
# unix_time_diff = 600000 # 10 minutes
# unix_time_diff = 300000 # 5 minutes
unix_time_diff = 60000 # 1 minute
# unix_time_diff = 30000 # 30 seconds
# unix_time_diff = 10000 # 10 seconds


dt_bi_events = dt[time_diff < unix_time_diff & !is.na(prev_event), .(instance, username, product, prev_time_str, time_string, prev_event, event, time_diff)]
View(dt_bi_events)

# Save data
file_name = paste0('dt_bi_events_',unix_time_diff,'.csv')
fwrite(dt_bi_events, file_name)





