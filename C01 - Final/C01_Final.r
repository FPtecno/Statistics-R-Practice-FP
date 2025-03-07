library(ggplot2)
library(dplyr)

br <- load("brfss2013.Rdata")

dim(brfss2013)
names(brfss2013)

br <- brfss2013

ggplot(data=br, aes(x=educa, fill=income2)) + geom_bar(position = "dodge")

ggplot(data=br, aes(x=exerany2, y=fruit1)) + geom_boxplot()
