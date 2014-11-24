#In a study of emergency room waiting times, investigators consider a new
#and the standard triage systems. To test the systems, administrators selected
#20 nights and randomly assigned the new triage system to be used on 10 nights
#and the standard system on the remaining 10 nights.
#They calculated the nightly median waiting time (MWT) to see a physician.
#The average MWT for the new system was 3 hours with a variance of 0.60
#while the average MWT for the old system was 5 hours with a variance of 0.68.
#Consider the 95% confidence interval estimate for the differences of the mean
#MWT associated with the new system. Assume a constant variance.
#What is the interval? Subtract in this order (New System - Old System).


nOld = nX = 10
medWaitTimeOld = meanX = 5
varOld = vX = 0.68
sdX = sqrt(vX)

nNew = nY = 10
medWaitTimeNew = meanY = 3
varNew = vY = 0.60
sdY = sqrt(vY)

xf1 = sdX^2/nX
yf1 = sdY^2/nY

df = ((xf1 + yf1)^2) / ((xf1^2/(nX - 1)) + (yf1^2/(nY - 1)))
tdf = qt(0.975, df=df)

range = meanY - meanX + c(-1, 1) * tdf * sqrt(xf1 + yf1)

