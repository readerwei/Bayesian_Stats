library("boot")
data("urine")
?urine
head(urine)
#install.packages('corrplot')
library("corrplot")
dat = na.omit(urine)
Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black", 
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")
library(rjags)
?densplot()
