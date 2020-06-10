install.packages("devtools") ## if devtools package not installed
devtools::install_github("kolesarm/GMMSensitivity")

#Larsen and Sergent 2001, 2008 Robust Decision making
#Huber and Ronchetti, 2009 textbook

library(mnormt)
library(MASS)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
str(dat)
#  Fitting symmetric t by profile likelihood
df = seq(5.25,6.75,.01)
df
n = length(df)
loglik = rep(0,n)
?cov.trob
for(i in 1:n){
  fit = cov.trob(dat,nu=df[i])
  loglik[i] = sum(log(dmt(dat,mean=fit$center,S=fit$cov,df=df[i])))
}
loglik
options(digits=7)
aic_t = -max(2*loglik)+ 2*(4 + 10 + 1) + 64000
df[loglik == max(loglik)]
z1 = (2*loglik > 2*max(loglik) - qchisq(.95,1))
z1
2*max(loglik)-64000
par(mfrow = c(1,1))
plot(df,2*loglik-64000,type="l",cex.axis=1.5,cex.lab=1.5,
     ylab="2*loglikelihood - 64,000",lwd=2)
abline(h = 2*max(loglik) - qchisq(.95,1)-64000)
abline(h = 2*max(loglik)-64000 )
abline(v=(df[16]+df[17])/2)
abline(v=(df[130]+df[131])/2)
graphics.off()

options(digits=4)
cov.trob(dat,nu=6,cor=TRUE)

colMeans(dat)
cor(dat)

#install.packages('R2WinBUGS')
#install.packages('MCMCpack')
library(R2WinBUGS)
library(MASS)  # need to mvrnorm
library(MCMCpack) # need for rwish
library(mnormt)
data(CRSPday,package="Ecdat")
y = CRSPday[,4:7]
N = dim(y)[1]
m = dim(y)[2]
mu0 = rep(0,m)
Prec_mu = diag(rep(1,m))/10000
Prec_tau =  diag(rep(1,m))/10000
df_wishart = 6
df_likelihood = 6
df_prior = 6
data = list(y = y, N = N, Prec_mu = Prec_mu, 
            Prec_tau = Prec_tau,
            mu0 = mu0, m = m, df_likelihood = df_likelihood, 
            df_prior = df_prior, df_wishart = df_wishart)
inits_t_CRSP = function(){list(mu = mvrnorm(1, mu0, diag(rep(1,m)/100)),
                               tau = rwish(6,diag(rep(1,m))/100))}
#library(R2WinBUGS)
multi_t.sim = bugs(data,inits_t_CRSP,model.file="mult_t_CRSP.bug",
                   parameters=c("mu","tau"),n.chains = 3,
                   n.iter=2200,n.burnin=200,n.thin=2,
                   program="WinBUGS", bugs.directory = 'E:/WinBUGS14', bugs.seed=13,codaPkg=FALSE)
print(multi_t.sim,digits=2)
print(multi_t.sim,digits=7)

tauhat = multi_t.sim$mean$tau
lambdahat = solve(tauhat)
sdinv = diag(1/sqrt(diag(lambdahat)))
cor = sdinv %*% lambdahat %*% sdinv
print(cor,digits=4)



