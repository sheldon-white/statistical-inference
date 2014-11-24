library(ggplot2)

runCount = 1000
sampleSize = 40
lambda = 0.2
exponentials = c()

for (i in 1 : runCount) exponentials = c(exponentials, rexp(sampleSize, lambda))
frame = data.frame(m = exponentials)

p = ggplot(frame, aes(x = m)) +
    geom_histogram(fill="lightblue2", colour="blue", binwidth = 0.5)
print(p)
