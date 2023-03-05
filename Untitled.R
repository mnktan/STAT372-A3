### STAT372 A3 Q3 ###

bridges <- read.table('Bridges.txt')
cost <- bridges[2:51, 2]
cost <- as.numeric(as.character(cost))
time <- bridges[2:51, 1]
time <- as.numeric(as.character(time))
urgency <- bridges[2:51, 3]
urgency <- as.numeric(as.character(urgency))

# a) 90% CI for total cost of bridge repairs, notes: n = 50, N = 645
a.n = length(cost)

# estimates
a.mu.hat = mean(cost)
a.std.hat = sqrt(var(cost))

a.ci.lower = a.mu.hat - (qnorm(0.95))*(sqrt(1 - a.n/645) * (a.std.hat / sqrt(a.n)))
a.ci.upper = a.mu.hat + (qnorm(0.95))*(sqrt(1 - a.n/645) * (a.std.hat / sqrt(a.n)))


# b) cost data vs time, see whether ration and regression estimates are appropriate
# to estimate total cost
plot(x = time, y = cost, main = "Time vs Cost", pch = 16, 
     col = adjustcolor("firebrick", 0.6), cex.main = 0.8)

# There looks to be a linear relationship between time and cost of repair, hence
# we can use ratio and regression estimates to produce a better estimate for the total
# cost compared to using the sample average


# c) ratio estimation
# ratio estimate of avg cost
mu.hat.x = mean(time)
mu.hat.y = mean(cost)
theta.hat = mu.hat.y / mu.hat.x
mu.x.true = 9.1
n = 50
N = 645

mu.hat.y.ratio <- theta.hat*mu.x.true
var.r <- var(cost - theta.hat*time)
var.mu.ratio <- (1/n) * (1 - n/N) * var.r
se.mu.ratio <- sqrt(var.mu.ratio)

# 90% CI for avg cost
mu.y.ratio.lower <- mu.hat.y.ratio - (qnorm(0.95))*se.mu.ratio 
mu.y.ratio.upper <- mu.hat.y.ratio + (qnorm(0.95))*se.mu.ratio

# 90% CI for total cost
mu.y.ratio.lower * 645
mu.y.ratio.upper * 645



# d) regression estimation 
mod <- lm(cost ~ time) 
beta.hat <- summary(mod)$coef[2,1]
mu.hat.y.reg <- mu.hat.y + beta.hat*(mu.x.true - mu.hat.x)
y = cost
x = time

var.mu.reg <- (1/n) * (1 - n/N) * sum((y - mu.hat.y - beta.hat*(x - mu.hat.x))^2)/(n-1)
se.mu.reg <- sqrt(var.mu.reg)
mu.y.reg.lower <- mu.hat.y.reg - (qnorm(0.95))*se.mu.reg 
mu.y.reg.upper <- mu.hat.y.reg + (qnorm(0.95))*se.mu.reg


# e) set sample n so we are 95% confident that pi 
# (proportion of bridges needing urgent repair) is estimated within 0.025
# of its true value

# l = 0.025?
# c = 1.96

urgent = urgency[urgency == 1]
n.urgents = length(urgent)



# f) Stratas
# Strat 1: at most 5 years since last repair (160 bridges in group)
# Strat 2: more than 5 years between at most 10 years since last repair (230 bridges in group)
# Strat 3: more 10 years since last repair (255 bridges in group)

# stratum weights
f.W1 = 160 / 645
f.W2 = 230 / 645
f.W3 = 255 / 645

# stratas
bridges.df = data.frame(time, cost, urgency)
f.strat1 = bridges.df[time <= 5, ] # n1 = 10
f.strat2 = bridges.df[time > 5 & time <= 10, ] # n2 = 14
f.strat3 = bridges.df[time > 10, ] # n3 = 26

# fpcs
f.f1 = 10 / 160
f.f2 = 14 / 230
f.f3 = 26 / 255

# mean
f.mu.hat = (f.W1 * mean(f.strat1$cost)) + (f.W2 * mean(f.strat2$cost)) + (f.W3 * mean(f.strat3$cost))

# std dev
f.var.mu = (f.W1)^2 * (1-f.f1) * (var(f.strat1$cost)/10) +
           (f.W2)^2 * (1-f.f2) * (var(f.strat2$cost)/14) + 
           (f.W3)^2 * (1-f.f3) * (var(f.strat3$cost)/26)
f.sd.mu = sqrt(f.var.mu)

# 90 CI for total cost of bridge repair 
f.mu.y.reg.lower <- 645 * (f.mu.hat - (qnorm(0.95))*f.sd.mu)
f.mu.y.reg.upper <- 645 * (f.mu.hat + (qnorm(0.95))*f.sd.mu)
cat("90% CI for total cost: (", f.mu.y.reg.lower, ",", f.mu.y.reg.upper, ")")


# g) Stratas (this time ratio for strat 2, regression for strat 3)
# Strat 1: at most 5 years since last repair (160 bridges in group)
# Strat 2: more than 5 years between at most 10 years since last repair (230 bridges in group)
# Strat 3: more 10 years since last repair (255 bridges in group)

# stratum weights
f.W1 = 160 / 645
f.W2 = 230 / 645
f.W3 = 255 / 645

# stratas
bridges.df = data.frame(time, cost, urgency)
f.strat1 = bridges.df[time <= 5, ] # n1 = 10
f.strat2 = bridges.df[time > 5 & time <= 10, ] # n2 = 14
f.strat3 = bridges.df[time > 10, ] # n3 = 26

# fpcs
f.f1 = 10 / 160
f.f2 = 14 / 230
f.f3 = 26 / 255

# mean (ratio for stratum 2, regression for stratum 3)
# ratio and regression estimates
g.x2 = f.strat2$time
g.y2 = f.strat2$cost
g.mu.hat.x2 = mean(g.x2)
g.mu.hat.y2 = mean(g.y2)
g.theta.hat = g.mu.hat.y2 / g.mu.hat.x2

g.x3 = f.strat3$time
g.y3 = f.strat3$cost
g.mu.hat.x3 = mean(g.x3)
g.mu.hat.y3 = mean(g.y3)


g.mod <- lm(g.y3 ~ g.x3)
g.beta.hat <- summary(g.mod)$coef[2,1]

# population avg for # of years since last major repair 
# strat 1
g.mu.x.true.s1 = 3.5
g.mu.x.true.s2 = 7

# we know u(x) = 9.1 from part (c) and 
# stratum weights
# N1 = 160 
# N2 = 230
# N3 = 255 
g.mu.x.true = 9.1
g.mu.x.true.s3 = ((9.1*645) - (g.mu.x.true.s1 *160) - (g.mu.x.true.s2 * 230)) / 255
  

# 1) point estimates
g.mu.hat.y.ratio <- g.theta.hat*g.mu.x.true.s2
g.mu.hat.y.reg <- g.mu.hat.y3 + g.beta.hat*(g.mu.x.true.s3 - g.mu.hat.x3)

# 2) variances
g.var.r <- var(g.y2 - g.theta.hat*g.x2)
g.var.mu.ratio <- (1/14) * (1 - 14/230) * g.var.r
g.se.mu.ratio <- sqrt(g.var.mu.ratio)

g.var.mu.reg <- (1/26) * (1 - 26/255) * sum((g.y3 - g.mu.hat.y3 - g.beta.hat*(g.x3 -g.mu.hat.x3))^2)/(26-1)
g.se.mu.reg <- sqrt(g.var.mu.reg)

# 3) CIs
# mean
g.mu.hat = (f.W1 * mean(f.strat1$cost)) + (f.W2 * g.mu.hat.y.ratio) + (f.W3 * g.mu.hat.y.reg)

# std dev
g.var.mu = (f.W1)^2 * (1-f.f1) * (var(f.strat1$cost)/10) +
  (f.W2)^2 * (g.var.mu.ratio/14) + 
  (f.W3)^2 * (g.var.mu.reg/26)
g.sd.mu = sqrt(g.var.mu)

# 90 CI for total cost of bridge repair 
g.mu.y.lower <- 645 * (g.mu.hat - (qnorm(0.95))*g.sd.mu)
g.mu.y.upper <- 645 * (g.mu.hat + (qnorm(0.95))*g.sd.mu)
cat("90% CI for total cost: (", g.mu.y.lower, ",", g.mu.y.upper, ")")





### QUESTION 4 ###

# a)
# we now have 50+300 = 350 bridges to sample
# based on results from pilot study
# W1 = f.W1, W2 = f.W2, W3 = f.W3
# sd.1 = sd(f.strat1$cost)
# sd.2 = sd(f.strat2$cost)
# sd.3 = sd(f.strat3$cost)

sd.s1 = sd(f.strat1$cost)
sd.s2 = sd(f.strat2$cost)
sd.s3 = sd(f.strat3$cost)
op.denom = (f.W1 * sd.s1) + (f.W2 * sd.s2) + (f.W3 * sd.s3)

op.n1 = ((f.W1 * sd.s1) / op.denom) * 350
op.n2 = ((f.W2 * sd.s2)  / op.denom) * 350
op.n3 = ((f.W3 * sd.s3) / op.denom) * 350

# new allocations
# n1 = 66.42754 = 66
# n2 = 65.63542 = 66
# n3 = 217.937 = 218

# original
# n1 = 10
# n2 = 14
# n3 = 26

# we need to send
# n1 = 66 - 10 = 56
# n2 = 66 - 14 = 52
# n3 = 218 - 26 = 192


n.c = ((1.644854) * (sqrt( (160/645)*(10.70222)^2 +
      (230/645)*(7.356244)^2 + (255/645)*(22.03112)^2))/1000)^2
