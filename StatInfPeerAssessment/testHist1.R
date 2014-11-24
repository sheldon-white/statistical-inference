library(ggplot2)

runCount = 1000
sampleSize = 40
lambda = 0.2
mns = c()
expectedMean = standardErr = 1 / lambda
populationVariance = 1 / lambda^2

cfunc <- function(runMean, n) sqrt(n) * (runMean - expectedMean) / standardErr

for (i in 1 : runCount) mns = c(mns, mean(rexp(sampleSize, lambda)))

calculatedMean = mean(mns, na.rm = T)

mnsFrame = data.frame(m = mns)
mnsFrame$adjusted = cfunc(mnsFrame$m, sampleSize)

p = ggplot(mnsFrame, aes(x = m)) +
    geom_histogram(fill="lightblue2", colour="blue") +
    geom_vline(aes(xintercept=calculatedMean), color="red", linetype="dashed", size=1) +
    geom_vline(aes(xintercept=expectedMean), color="green", linetype="dashed", size=1)
print(p)

q = ggplot(mnsFrame, aes(x = adjusted)) +
    geom_histogram(fill="grey", colour = "green", aes(y = ..density..)) +
    stat_function(fun = dnorm, size = 2)
print(q)

#Sample mean should equal the expected mean
print(calculatedMean)
print(expectedMean)

# Variance of sample mean should equal (population variance) / (sample size)
message("sample mean variance = ", var(mnsFrame$m))
message("population variance = ", populationVariance / sampleSize)


