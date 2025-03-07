data(arbuthnot)
arbuthnot
dim(arbuthnot)
names(arbuthnot)
arbuthnot$boys
arbuthnot$year
min(arbuthnot$year)
max(arbuthnot$year)
arbuthnot$girls

ggplot(data = arbuthnot, aes(x = year, y = girls)) + geom_point()
ggplot(data = arbuthnot, aes(x = year, y = boys)) + geom_point()

?ggplot

5218 + 4683
arbuthnot$boys + arbuthnot$girls

arbuthnot <- arbuthnot %>% mutate(total = boys + girls)

ggplot(data = arbuthnot, aes(x = year, y = total)) + geom_line() + geom_point()

ggplot(data = arbuthnot, aes(x = year, y = boys)) + geom_line() + geom_point()

arbuthnot <- arbuthnot %>% mutate(more_boys = boys > girls)

arbuthnot

ggplot(data = arbuthnot, aes(x = year, fill = more_boys))
+ geom_bar(position = "dodge")

data(present)
dim(present)
names(present)

range(present$year)

present <- present %>% mutate(total = boys + girls)
present <- present %>% mutate(prop_boys = boys / total)

ggplot(data = present, aes(x = year, y = prop_boys)) +
  geom_line() + geom_point()

present <- present %>% mutate(more_boys = boys > girls)

ggplot(data = present, aes(x = year, fill = more_boys)) +
  geom_bar(position = "dodge")

present <- present %>% mutate(prop_boy_girl = boys / girls)

ggplot(data = present, aes(x = year, y = prop_boy_girl)) + geom_line()

max(present$total)

desc(present$total)
