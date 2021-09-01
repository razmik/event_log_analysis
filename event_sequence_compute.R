rm(list = ls())

library(data.table)
library(dplyr)
library(ggplot2)

dt_orig = fread('jira_clean.tsv', sep = '\t')
setnames(dt_orig, c('time_string', 'unix_time', 'instance', 'product', 'username', 'event', 'attributes'))


# Initial exploration
dt_orig[, .(instance_count = uniqueN(instance)), username][order(-instance_count)]
event_user_details = dt_orig[, .(event_occurrence = .N, unique_users = uniqueN(username)), event][order(-event_occurrence )]

# Exploration
# event_user_details[event_occurrence<=5, .N]
# event_user_details[unique_users>1 & event_occurrence > 5, .N]
# 
# plot(density(log1p(event_user_details$event_occurrence)))
# plot(density(log1p(event_user_details$unique_users)))
# 
# event_user_details = melt(event_user_details)
# event_user_details[, log_value := log1p(value)]
# ggplot(event_user_details, aes(value, fill = variable)) +
#   geom_density(alpha = 0.2) + xlab('Value of occurences/user count') +
#   ggtitle('Occurence of event sequence vs unique users')



# First compute the time difference between two events
dt = dt_orig[, .(time_string, unix_time, instance, product, username, event)]
setorderv(dt, c('instance', 'product', 'username', 'unix_time'))
dt = dt[, prev_time := shift(unix_time, 1L, type = 'lag', fill = NA_integer_), by = .(instance, username, product)]
dt = dt[, prev_event := shift(event, 1L, type = 'lag', fill = NA_integer_), by = .(instance, username, product)]
dt[prev_time == 0, prev_time := NA_integer_]
dt[, time_diff := unix_time - prev_time, by = .(instance, username, product)]

# View(dt[, .(instance, username, product, unix_time, prev_time, time_string, event, prev_event, time_diff)])

# Generate similar times
# unix_time_diff = 600000 # 10 minutes
# unix_time_diff = 300000 # 5 minutes
# unix_time_diff = 60000 # 1 minute
# unix_time_diff = 30000 # 30 seconds
# unix_time_diff = 20000 # 30 seconds
# unix_time_diff = 15000 # 15 seconds
# unix_time_diff = 10000 # 10 seconds
# unix_time_diff = 5000 # 1 seconds
# unix_time_diff = 1000 # 1 seconds
unix_time_diff = 100 # 0.5 seconds

dt[, counter := 0]
dt[time_diff >= unix_time_diff, counter := 1]
dt[, event_group_id := cumsum(counter), by = .(instance, username, product)]

# View(dt[, .(instance, username, product, time_string, event, time_diff, counter, event_group_id)])

# Combine event sequences

dt_event = dt[, lapply(.SD, paste0, collapse="#-#"), .SDcols = c('event'), by = .(instance, username, product, event_group_id)]
setnames(dt_event, 'event', 'event_seq')
dt_event[, seq_length := length(strsplit(event_seq, '#-#')[[1]]), by = .(instance, username, product, event_group_id)]

get_tot_time = function(x){
  return(sum(x$time_diff[2:NROW(x)]))
}

dt_event_time = dt[, .(total_time = get_tot_time(.SD)), .SDcols = c('time_diff'), by = .(instance, username, product, event_group_id)]
dt_event[dt_event_time, on=.(instance, username, product, event_group_id), total_seq_time := i.total_time]


# Filter importance events
thresh_occurence = 5
thresh_uni_user = 1

dt_event_seq = dt_event[seq_length > 1, .(occurrence_count = .N, unique_users = uniqueN(username), mean_seq_time = mean(total_seq_time)), event_seq]
dt_event_seq[, seq_length := length(strsplit(event_seq, '#-#')[[1]]), by=event_seq]
dt_event_seq = dt_event_seq[occurrence_count > thresh_occurence & unique_users > thresh_uni_user, ]

dt_event_seq[order(-mean_seq_time)][1, mean_seq_time] / (1000)
dt_event_seq[order(-seq_length)][1, seq_length]


# Visualization

View(dt_event_seq)

dt_event_seq_melt = setDT(melt(dt_event_seq[, .(event_seq, occurrence_count, unique_users)]))
dt_event_seq_melt[, log_value := log1p(value)]

ggplot(dt_event_seq_melt, aes(log_value, fill = variable)) +
  geom_density(alpha = 0.2) + xlab('Value of occurences/user count') +
  ggtitle('Occurence of event sequence vs unique users')

ggplot(dt_event_seq_melt, aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  ylab('Event Count') + xlab('Username') + 
  ggtitle('Box-Plot for Daily Event Count of Users A and B')

# Save data
file_name = paste0('dt_event_seq_',unix_time_diff,'_',thresh_occurence,'_',thresh_uni_user,'.csv')
fwrite(dt_event_seq, file_name)





