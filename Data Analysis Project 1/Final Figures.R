#Final Submission
#================
# Cleaning dataset:
  #Replacing % from data & importing as numeric values:
load("loansData.rda")
loansData$Interest.Rate <- sapply(sub(pattern="%",replacement="",x=loansData$Interest.Rate,),FUN=as.numeric)
loansData$Debt.To.Income.Ratio <- sapply(sub(pattern="%",replacement= "",x=loansData$Debt.To.Income.Ratio,),FUN=as.numeric)

  #Other Changes:
loansData$Loan.Length <- sapply(sub(" months", "", loansData$Loan.Length,),FUN=as.numeric)
loansData$Employment.Length <- factor(loansData$Employment.Length,levels(loansData$Employment.Length)[c(2:3,5:12,4,13,1)])

loansData$Employment.Length <- sub(pattern=" years?",replacement="", loansData$Employment.Length)
loansData$Employment.Length<- sub(pattern="\\+",replacement="",loansData$Employment.Length)
loansData$Employment.Length<- sub(pattern="< 1",replacement="0",loansData$Employment.Length)
loansData$Employment.Length<- sub(pattern="n/a",replacement="-1",loansData$Employment.Length)
loansData$Employment.Length<- as.numeric(loansData$Employment.Length)

loansData$FICO.Range<- sapply(sub("-\\d{3}", "", loansData$FICO.Range),FUN=as.numeric)

#Figures

lm_mine <- lm(loansData$Interest.Rate ~ loansData$Amount.Requested +
                   loansData$Debt.To.Income.Ratio + loansData$Loan.Length)
xFICO = jitter(as.numeric(loansData$FICO.Range))

par(mfrow=c(2,2))
#Plot 1
smoothScatter(loansData$FICO.Range, loansData$Interest.Rate,
              xlab="Applicant FICO Score",
              ylab="Loan Interest Rate (%)",
              main="Figure 1. Interest Rate by FICO Score")

#Plot 2
plot(xFICO, loansData$Interest.Rate,
     col=as.factor(cut(loansData$Amount.Requested,breaks=c(0,7500,15000, 22000,34000))),
     pch=19, cex=0.7,
     xlab="Applicant FICO Score",
     ylab="Loan Interest Rate (%)",
     main="Figure 2. Amount Requested")
legend(807,25.5,
       legend=c("$0-5k", "$5-10k", "$10-20k", "$20-35k"),
       col=c("black", "red", "green", "blue"),
       pch=19, cex=0.60)

#Plot 3
lengthFactor <- plot(xFICO, loansData$Interest.Rate,
                     col=as.factor(cut(loansData$Debt.To.Income.Ratio,breaks=c(0,10,20,30,40))),
                     pch=19, cex=0.7,
                     xlab="Applicant FICO Score",
                     ylab="Loan Interest Rate (%)",
                     main="Figure 3. Debt To Income Ratio")
legend(813,25.5,
       legend=c("0-10", "10-20", "20-30","30-40"),
       col=c("black", "red", "green", "blue"),
       pch=19, cex=0.60)

#Plot 4
lengthFactor <- plot(xFICO, loansData$Interest.Rate,
     col=as.factor(loansData$Loan.Length),
     pch=19, cex=0.7,
     xlab="Applicant FICO Score",
     ylab="Loan Interest Rate (%)",
     main="Figure 4. Loan Length")
legend(803, 25.5,
       legend=c("36 months", "60 months"),
       col=c("black", "red"),
       pch=19, cex=0.60)