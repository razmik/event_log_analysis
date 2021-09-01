library(data.table)
library(dplyr)
library(ggplot2)
library(car)

dt_orig = fread('jira_clean.tsv', sep = '\t')
setnames(dt_orig, c('time_string', 'unix_time', 'instance', 'product', 'username', 'event', 'attributes'))

# Initial exploration
dt_orig[, .N, event][order(-N)] %>% View
dt_orig[, .N, username]
dt_orig[username=='b4d3faf5d77a48fedebe57e32ee749a9',] %>% View

# Data processing
dt_orig[, c("date", "time") := tstrsplit(time_string, " ", fixed=TRUE)]
dt_orig[, date := as.Date(date, format = "%Y-%m-%d")]
dt_orig[, time := NULL]

# Exploration
dt_orig[, range(date)]

# Filter data for t-test
selected_users = c('778251ffec35de6bbbb089198b9ac516', '7e7a085a9f6f55253b2aa15dbcb702a8')
selected_instance = 'c93da7e17688ef90fc43963b5a6c6259'
dt = dt_orig[username %in% selected_users & instance == selected_instance, .(username, date, event)]
dt[username == '778251ffec35de6bbbb089198b9ac516', username := 'A']
dt[username == '7e7a085a9f6f55253b2aa15dbcb702a8', username := 'B']

dt_avg_per_day = dt[, .(.N), by=.(username, date)]
setnames(dt_avg_per_day, 'N', 'event_count')

# Add new row for Dec 20
dt_avg_per_day_0_imputed = rbind(dt_avg_per_day, list('A', as.Date('2014-12-20', format = "%Y-%m-%d"), 0), fill=TRUE)

ggplot(dt_avg_per_day_0_imputed,aes(x = date, y = event_count)) + 
  geom_bar(aes(fill = username),stat = "identity", width=0.8, position = position_dodge(width=0.5)) +
  xlab("Date") + ylab("Event Count") +
  ggtitle('Daily Event Count of Users A and B')

ggplot(dt_avg_per_day_0_imputed,aes(x = date, y = event_count)) + 
  geom_bar(aes(fill = username),stat = "identity", width=0.8, position = position_dodge(width=0.5)) +
  xlab("Date") + ylab("LOG Event Count") +
  ggtitle('LOG Daily Event Count of Users A and B') + 
  scale_y_log10()

# Cast data for statistical testing

dt_avg_per_day_cast = dcast(
  dt_avg_per_day,
  date ~ username,
  value.var = c('event_count')
)

# Fill NA days with 0 events
dt_avg_per_day_cast[is.na(A), A := 0]
dt_avg_per_day_cast[is.na(B), B := 0]

# Compute further statics
dt_avg_per_day_cast[, day := weekdays(date)]
dt_avg_per_day_cast[, diff := A-B]



# Mean values
mean(dt_avg_per_day_cast$A)
mean(dt_avg_per_day_cast$B)

# Check normality of data

shapiro.test(dt_avg_per_day_cast$A) 
# p-value = 0.02763 > 0.05 implying that the distribution of the data are 
# not significantly different from normal distribution. 
# In other words, we can assume the normality.

qqPlot(dt_avg_per_day_cast$A, ylab = 'User A', main = 'Q-Q plot of User A Event Count')
# As all the points fall approximately along this reference line, we can assume normality.

shapiro.test(dt_avg_per_day_cast$B)
# p-value = 0.000244 < 0.05 implying that the distribution of the data are 
# significantly different from normal distribution. 
# In other words, we cannot assume the normality.

qqPlot(dt_avg_per_day_cast$B, ylab = 'User B', main = 'Q-Q plot of User B Event Count')
# As all the points do not fall approximately along this reference line, we cannot assume normality.

## T-test cannot be performed because the daily counts are not normally distributed
## Since we have <30 data points, these are not larege enough to sustain the normality assumption.
# # Find t-test value
# t_test = t.test(dt_avg_per_day_cast$A, dt_avg_per_day_cast$B, 
#                 paired = TRUE, mu = 0, conf.level = 0.95,
#                 alternative = 'two.sided')
# t_test
# 
# # Find t-critical value
# qt(p = 0.05/2, df = 22, lower.tail = FALSE)

# Wilcoxon signed rank test
# https://www.statisticshowto.com/probability-and-statistics/non-normal-distributions/

wilcox.test(dt_avg_per_day_cast$A, dt_avg_per_day_cast$B,
            paired = TRUE, mu = 0, conf.level = 0.95,
            alternative = 'two.sided')


# Critical rank
qsignrank(p = 0.05/2, n = NROW(dt_avg_per_day_cast), lower.tail=FALSE)

# W_stat = 214
# W_critical = 252
# W_stat < W_critical
# Test stat is less than the critical value. Thus, we reject null hypothesis.
# There is sufficient evidence to suggest that there is a difference between the two users average daily events.

# Plot differences

ggplot(dt_avg_per_day, aes(event_count, fill = username)) +
  geom_density(alpha = 0.2) + xlab('DAILY EVENT COUNT') +
  ggtitle('Density of daily event counts')



ggplot(dt_avg_per_day_cast, aes(x = as.numeric(`A`), y = as.numeric(`B`), color = day, shape = day)) + 
  geom_point(shape = 18) + 
  geom_abline(intercept = 0, slope = 1, size = 0.5) + 
  xlab('User A') + ylab('User B') +
  ggtitle('Daily Event Count of Users A and B')


ggplot(dt_avg_per_day, aes(x=username, y=event_count, fill=username)) +
  geom_boxplot() +
  ylab('Event Count') + xlab('Username')
  ggtitle('Box-Plot for Daily Event Count of Users A and B')







