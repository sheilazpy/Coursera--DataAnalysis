Final Submission
================
```{r}
# Get data:
setwd('~/Desktop/coursera-data-analysis/assignments/1')
load('data/raw/loansData.rda')

# Clean it:
loansData$Interest.Rate <- sapply(sub("%", "", loansData$Interest.Rate,), as.numeric)
loansData$Debt.To.Income.Ratio <- sapply(sub("%", "", loansData$Debt.To.Income.Ratio,), as.numeric)
loansData$Loan.Length <- sapply(sub(" months", "", loansData$Loan.Length,), as.numeric)
loansData$Employment.Length <- factor(loansData$Employment.Length,
                                      levels(loansData$Employment.Length)[c(2:3,5:12,4,13,1)])
loansData$Employment.Length.numeric <- sub(" years?", "", loansData$Employment.Length)
loansData$Employment.Length.numeric <- sub("\\+", "", loansData$Employment.Length.numeric)
loansData$Employment.Length.numeric <- sub("< 1", "0", loansData$Employment.Length.numeric)
loansData$Employment.Length.numeric <- sub("n/a", "-1", loansData$Employment.Length.numeric)
loansData$Employment.Length.numeric <- sapply(loansData$Employment.Length.numeric, as.numeric)
loansData$FICO.Range.floor <- sapply(sub("-\\d{3}", "", loansData$FICO.Range), as.numeric)
```

Figures
-------
```{r fig.width=8.5, fig.height=11}
# open PDF
pdf(file="loansData-analysis-4up.pdf", height=11, width=8.5)

par(mfrow=c(2,2))

lmNoAdjust <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested +
                   loansData$Amount.Funded.By.Investors + loansData$Loan.Length)
xFICO = jitter(loansData$FICO.Range.floor)

#par(mfrow=c(1,1))
smoothScatter(loansData$FICO.Range.floor, loansData$Interest.Rate,
              xlab="Applicant FICO Score",
              ylab="Loan Interest Rate (%)",
              main="Figure 1. Interest Rate by FICO Score")

#par(mfrow=c(1,1))
plot(xFICO, loansData$Interest.Rate,
     col=as.factor(cut(loansData$Amount.Requested,
                       breaks=c(0, 5000, 10000, 20000, 35000))),
     pch=19, cex=0.5,
     xlab="Applicant FICO Score",
     ylab="Loan Interest Rate (%)",
     main="Figure 2. Amount Requested")
# must be an easier way to align the factor to the legend...
legend(780, 25,
       legend=c("$0-5k", "$5-10k", "$10-20k", "$20-35k"),
       col=c("black", "red", "green", "blue"),
       pch=19, cex=0.5)

#par(mfrow=c(1,1))
plot(xFICO, loansData$Interest.Rate,
     col=as.factor(cut(loansData$Amount.Funded.By.Investors,
                       breaks=c(0, 5000, 10000, 20000, 35000))),
     pch=19, cex=0.5,
     xlab="Applicant FICO Score",
     ylab="Loan Interest Rate (%)",
     main="Figure 3. Amount Funded")
legend(780, 25,
       legend=c("$0-5k", "$5-10k", "$10-20k", "$20-35k"),
       col=c("black", "red", "green", "blue"),
       pch=19, cex=0.5)

#par(mfrow=c(1,1))
lengthFactor <- 
plot(xFICO, loansData$Interest.Rate,
     # coerce months to years (so they aren't the same color)
     col=as.factor(loansData$Loan.Length),
     pch=19, cex=0.5,
     xlab="Applicant FICO Score",
     ylab="Loan Interest Rate (%)",
     main="Figure 4. Loan Length")
legend(780, 25,
       legend=c("36 months", "60 months"),
       col=c("black", "red"),
       pch=19, cex=0.5)

# close PDF
dev.off()
```
