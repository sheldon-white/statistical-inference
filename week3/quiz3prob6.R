#To further test the hospital triage system, administrators selected 200 nights
#and randomly assigned a new triage system to be used on 100 nights and a standard
#system on the remaining 100 nights. They calculated the nightly median waiting time (MWT)
#to see a physician. The average MWT for the new system was 4 hours with a standard
#deviation of 0.5 hours while the average MWT for the old system was 6 hours with a
#standard deviation of 2 hours. Consider the hypothesis of a decrease in the mean MWT
#associated with the new treatment. What does the 95% independent group confidence
#interval with unequal variances suggest vis a vis this hypothesis?
#(Because there's so many observations per group, just use the Z quantile instead of the T.)

nNew = nX = 100
meanWaitingTimeNew = meanX = 4
sdMWTNew = sdX = 0.5

nOld = nY = 100
meanWaitingTimeOld = meanY = 6
sdMWTOld = sdY = 2

# pooled variance estimate: ((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2)
varPooled = ((nX-1) * sdX^2 + (nY-1) * sdY^2) / (nX + nY -2)
sdPooled = sqrt(varPooled)


#xf1 = sdX^2/nX
#yf1 = sdY^2/nY

#df = ((xf1 + yf1)^2) / ((xf1^2/(nX - 1)) + (yf1^2/(nY - 1)))
#tdf = qt(0.95, df=df)

range = meanY - meanX + c(-1, 1) * qt(0.975, nX + nY - 2) * sdPooled * sqrt(1/ 100 + 1/100)

