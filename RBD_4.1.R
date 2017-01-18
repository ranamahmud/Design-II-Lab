setwd("/media/rana/Contest Programming/Stat/Design II/Design II Lab")
data <- read.csv("data.csv")
names(data)
attach(data)

single.factor <- aov(measure~as.factor(psi))
summary(single.factor)

double.factor <- aov(measure~as.factor(block)+as.factor(psi))
summary(double.factor)

SSTotal <- sum(measure^2)- ((sum(measure))^2)/length(measure)

res <- aggregate(measure, by = list(psi), FUN = sum)
SSTreatment <- (1/6)*sum(res$x^2) - ((sum(measure))^2)/length(measure)
SSTreatment

res2 <- aggregate(measure, by = list(block), FUN = sum)
SSBlock <- (1/4)*sum(res2$x^2) - ((sum(measure))^2)/length(measure)
SSBlock

SSError <- SSTotal - SSTreatment - SSBlock
SSError

MSTreatment <- SSTreatment / 3
MSTreatment
MSError <- SSError / 15
MSError

FCal <- MSTreatment / MSError
FCal

pValue <- 1 - pf(FCal,3,15)
pValue
