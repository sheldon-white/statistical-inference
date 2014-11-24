#Suppose that 18 obese subjects were randomized, 9 each, to a new diet pill and a placebo.
#Subjects’ body mass indices (BMIs) were measured at a baseline and again after having
#received the treatment or placebo for four weeks. The average difference from follow-up
#to the baseline (followup - baseline) was −3 kg/m2 for the treated group and 1 kg/m2 for
#the placebo group. The corresponding standard deviations of the differences was 1.5 kg/m2
#for the treatment group and 1.8 kg/m2 for the placebo group.
#Does the change in BMI over the four week period appear to differ between the treated
#and placebo groups? Assuming normality of the underlying data and a common population
#variance, calculate the relevant *90%* t confidence interval.
#Subtract in the order of (Treated - Placebo) with the smaller (more negative) number first.


nPlacebo = nX = 9
placeboDiff = meanX = 1
sdPlacebo = sdX = 1.8

nTreated = nY = 9
treatedDiff = meanY = -3
sdTreated = sdY = 1.5

xf1 = sdX^2/nX
yf1 = sdY^2/nY

df = ((xf1 + yf1)^2) / ((xf1^2/(nX - 1)) + (yf1^2/(nY - 1)))
tdf = qt(0.95, df=df)

range = meanY - meanX + c(-1, 1) * tdf * sqrt(xf1 + yf1)

