data(ToothGrowth)
ToothGrowth$dose = as.factor(ToothGrowth$dose)

resultsSupp = data.frame(label = character(0), low = numeric(0), high = integer(0))
generateInterval = function(label, supp1, dose1, supp2, dose2) {
    p = with(ToothGrowth, t.test(len[supp == supp1 & dose == dose1],
                                 len[supp == supp2 & dose == dose2]))
    row = data.frame(label, p$conf.int[1], p$conf.int[2])
    colnames(row) = c("label", "low", "high")
    row
}
resultsSupp = rbind(resultsSupp, generateInterval("OJ/0.5 - VC/0.5", 'OJ', 0.5, 'VC', 0.5))
resultsSupp = rbind(resultsSupp, generateInterval("OJ/1.0 - VC/1.0", 'OJ', 1, 'VC', 1))
resultsSupp = rbind(resultsSupp, generateInterval("OJ/2.0 - VC/2.0", 'OJ', 2, 'VC', 2))

resultsDose = data.frame(label = character(0), low = numeric(0), high = integer(0))
resultsDose = rbind(resultsDose, generateInterval("OJ/1.0 - OJ/0.5", 'OJ', 1, 'OJ', 0.5))
resultsDose = rbind(resultsDose, generateInterval("OJ/2.0 - OJ/1.0", 'OJ', 2, 'OJ', 1))
resultsDose = rbind(resultsDose, generateInterval("VC/1.0 - VC/0.5", 'VC', 1, 'VC', 0.5))
resultsDose = rbind(resultsDose, generateInterval("VC/2.0 - VC/1.0", 'VC', 2, 'VC', 1))

variances = data.frame(supplement = character(0), dosage = character(0), variance = numeric(0))
for (s in c('OJ', 'VC')) {
    for (d in c(0.5, 1, 2)) {
        row = data.frame(s, d, var(subset(ToothGrowth, supp == s & dose == d)$len))
        colnames(row) = c("suppliment", "dosage", "variance")
        variances = rbind(variances, row)
    }
}