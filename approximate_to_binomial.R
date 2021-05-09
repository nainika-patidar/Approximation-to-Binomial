n=300
lambda=6.9
p = lambda/n
#generating the random numbers
set.seed(1)
sample1 <- rbinom(100,300,p)
set.seed(2)
sample2 <- rbinom(200,300,p)
set.seed(3)
sample3 <- rbinom(300,300,p)

set.seed(4)
Psample <- rpois(300,lambda)

#FrequencyTables
Bfreqtable1 <- transform(table(sample1))
colnames(Bfreqtable1) <- c("Binomial RV Sample 1","Frequency")
Bfreqtable2 <- transform(table(sample2))
colnames(Bfreqtable2) <- c("Binomial RV Sample 2","Frequency")
Bfreqtable3 <- transform(table(sample3))
colnames(Bfreqtable3) <- c("Binomial RV Sample 3","Frequency")
Pfreqtable <- transform(table(Psample))
colnames(Pfreqtable) <- c("Poisson RV Sample 1","Frequency")
Bfreqtable1;Bfreqtable2;Bfreqtable3;Pfreqtable

# Bar Diagrams
par(mfrow=c(2,2),mar=c(4,3,1,1))
barplot(Bfreqtable1$Frequency,names.arg = as.character(Bfreqtable1$`Binomial RV Sample 1`),col="#0099FF",xlim=c(0,17),ylim = c(0,50),cex.names = 0.8,xlab = "binom1(X=100,np=6.9)")
barplot(Bfreqtable2$Frequency,names.arg = as.character(Bfreqtable2$`Binomial RV Sample 2`),col="#009999",xlim=c(0,17),ylim = c(0,50),cex.names = 0.8,xlab = "binom2(X=200,np=6.9)")
barplot(Bfreqtable3$Frequency,names.arg = as.character(Bfreqtable3$`Binomial RV Sample 3`),col="#33CCFF",xlim=c(0,17),ylim = c(0,50),cex.names = 0.8,xlab = "binom3(X=300,np=6.9)")
barplot(Pfreqtable$Frequency,names.arg = as.character(Pfreqtable$`Poisson RV Sample 1`),col="#006699",xlim=c(0,17),ylim = c(0,50),cex.names = 0.8,xlab = "poisson(X=300,lambda=6.9)")



#Normal's Approximation to binomial

n1=300; n2=600; n3=1000
p1=6.9/n1; p2=6.9/n2; p3=6.9/n3
X1=100; X2=200; X3=300

#Generating random numbers 

set.seed(1)
S1 <- rbinom(X1,n1,p1)
set.seed(2)
S2 <- rbinom(X1,n2,p2)
set.seed(3)
S3 <- rbinom(X1,n3,p3)

set.seed(1)
S4 <- rbinom(X2,n1,p1)
set.seed(2)
S5 <- rbinom(X2,n2,p2)
set.seed(3)
S6 <- rbinom(X2,n3,p3)

set.seed(1)
S7 <- rbinom(X3,n1,p1)
set.seed(2)
S8 <- rbinom(X3,n2,p2)
set.seed(3)
S9 <- rbinom(X3,n3,p3)


#Frequency Tables
freqtabS1 <- transform(table(S1)); freqtabS2 <- transform(table(S2)); freqtabS3 <- transform(table(S3))
freqtabS4 <- transform(table(S4)); freqtabS5 <- transform(table(S5)); freqtabS6 <- transform(table(S6))
freqtabS7 <- transform(table(S7)); freqtabS8 <- transform(table(S8)); freqtabS9 <- transform(table(S9))

#Combined Bar plot
par(mfrow=c(3,3))
barplot(freqtabS1$Freq,names.arg = as.character(freqtabS1$S1),xlim = c(0,17),ylim = c(0,20),xlab="binom(X=100,n=300,p=0.023)",cex.names = 0.8,col="#FAD7A0")
barplot(freqtabS2$Freq,names.arg = as.character(freqtabS2$S2),xlim = c(0,17),ylim = c(0,20),xlab="binom(X=100,n=600,p=0.0115)",cex.names = 0.8,col="#E59866")
barplot(freqtabS3$Freq,names.arg = as.character(freqtabS3$S3),xlim = c(0,17),ylim = c(0,20),xlab="binom(X=100,n=1000,p=0.0069)",cex.names = 0.8,col="#D35400")

barplot(freqtabS4$Freq,names.arg = as.character(freqtabS4$S4),xlim = c(0,17),ylim = c(0,40),cex.names = 0.8,col="#17A589",xlab="binom(X=200,n=300,p=0.023)")
barplot(freqtabS5$Freq,names.arg = as.character(freqtabS5$S5),xlim = c(0,17),ylim = c(0,40),cex.names = 0.8,col="#1E8449",xlab="binom(X=200,n=600,p=0.0115)")
barplot(freqtabS6$Freq,names.arg = as.character(freqtabS6$S6),xlim = c(0,17),ylim = c(0,40),cex.names = 0.8,col="#186A3B",xlab="binom(X=200,n=1000,p=0.0069)")

barplot(freqtabS7$Freq,names.arg = as.character(freqtabS7$S7),xlim = c(0,17),ylim = c(0,60),cex.names = 0.8,col="#7FB3D5",xlab="binom(X=300,n=300,p=0.023)")
barplot(freqtabS8$Freq,names.arg = as.character(freqtabS8$S8),xlim = c(0,17),ylim = c(0,60),cex.names = 0.8,col="#76D7C4",xlab="binom(X=300,n=600,p=0.0115)")
barplot(freqtabS9$Freq,names.arg = as.character(freqtabS9$S9),xlim = c(0,17),ylim = c(0,60),cex.names = 0.8,col="#52BE80",xlab="binom(X=300,n=1000,p=0.0069)")








