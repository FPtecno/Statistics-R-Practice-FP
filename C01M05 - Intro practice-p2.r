data(nycflights)
dim(nycflights)
names(nycflights)
str(nycflights)

ggplot(data=nycflights, aes(x=dep_delay)) + geom_histogram(binwidth = 15)

rdu_flights <- nycflights %>%
  filter(dest == "RDU")
ggplot(data=rdu_flights, aes(x=dep_delay)) + geom_histogram(binwidth = 30)

rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)

sfo_feb_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

ggplot(data=sfo_feb_flights, aes(x=arr_delay)) + geom_histogram(binwidth = 15)

rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median_dd = median(arr_delay), IQR_dd = IQR(arr_delay), n=n())

nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay), median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay <5, "on time", "delayed"))

nycflights %>%
  group_by(origin) %>%
  summarise(ot_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(ot_dep_rate))

ggplot(data=nycflights, aes(x = origin, fill = dep_type)) + geom_bar()

nycflights <- nycflights %>%
  mutate(avg_speed = distance / (air_time/60))

nycflights %>%
  select(avg_speed, tailnum) %>%
  arrange(desc(avg_speed)) %>%
  head(5)

ggplot(data=nycflights, aes(x=distance, y=avg_speed)) + geom_line()

nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay<=0, "on time", "delayed"))

nycflights %>%
  select(origin, dest, dep_type, arr_type) %>%
  filter(dep_type == "delayed") %>%
  summarise(ot_arr_rate = sum(arr_type == "on time") / n())
