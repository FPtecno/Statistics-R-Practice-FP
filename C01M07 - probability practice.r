data(kobe_basket)
names(kobe_basket)

kobe_streak <- calc_streak(kobe_basket$shot)
ggplot (data=kobe_streak, aes(x=length)) + geom_histogram(binwidth = 1)
ggplot (data=kobe_streak, aes(x=length)) + geom_boxplot()

# simulation of flipping a fair coin
coin_outcomes <- c("heads", "tails")
sim_fair_coin <- sample(coin_outcomes, size=100, replace=TRUE)
table(sim_fair_coin)

sim_unfair_coin <- sample(coin_outcomes, size=100, replace=TRUE, prob=c(0.2, 0.8))
table(sim_unfair_coin)

shot_outcomes <- c("H", "M")
sim_basket <- sample(shot_outcomes, size = 133, replace=TRUE, prob=c(0.45,0.55))
table(sim_basket)

sim_streak <- calc_streak(sim_basket)

ggplot (data=sim_streak, aes(x=length)) + geom_histogram(binwidth = 1)
