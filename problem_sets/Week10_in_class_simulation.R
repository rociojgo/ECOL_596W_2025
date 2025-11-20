# Make up glm data

library(dplyr)

# Simulate candy where the average person eats
# 4 pounds of candy, (SD = 1)
data <- data.frame(candy = rnorm(100, mean = 4, sd = 1))
head(data)
#
# Simulate an effect on sleep, typical amount of sleep is 8 h,
# each pound of candy decreases sleep by 1h.
# simulate a linear relationship i.e. sleep = 8 - 1 * candy
data <- data %>% mutate (sleep = 8 - 1*candy)
head(data)
plot(sleep ~ candy, data = data )
# add tiny bit of error from a normal
# distribution (sd = 0.1)

data <- data %>% mutate(sleep = sleep +
                          rnorm(100, mean = 0, sd = 0.2))
plot(sleep ~ candy, data = data )
# round hours of sleep to whole numbers
data <- data %>% mutate(sleep = round(sleep))

# compare lm glm
lmmod <- lm(sleep ~ candy, data = data)
glmmod <- glm(sleep ~ candy, data = data, family= "poisson")
AIC(lmmod)
AIC(glmmod)
#

#
fish <- read.delim("datasets/maine_fish.txt", sep = '\t')
head(fish)

fish$log_depth_max <- log(fish$depth_max)
fish %>% select(depth_max, log_depth_max) %>% head

lm(fish ~ depth_max, data = fish) %>% summary
lm(fish ~ log_depth_max, data = fish) %>% summary


ps <- NULL
for( i in 1:1000) {
a <- rnorm(20, mean = 12, sd = 3)
b <- rnorm(20, mean = 10, sd = 3)
test <- t.test(a,b)
ps[i] <- test$p.value
}

hist(ps)


