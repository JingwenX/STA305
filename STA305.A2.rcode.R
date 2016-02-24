#1 c)
```{r}
z7 <- qnorm(0.7, mean = 0, sd = 1)
z8 <- qnorm(0.8, mean = 0, sd = 1)
z9 <- qnorm(0.9, mean = 0, sd = 1)

n7 <- (((1.645 + z7)*sqrt(20))/2.1)^2
n8 <- (((1.645 + z8)*sqrt(20))/2.1)^2
n9 <- (((1.645 + z9)*sqrt(20))/2.1)^2

n7 #output
n8 #output
n9 #output
```

#2a)
twosamplettestpow <- function(alpha,n1,n2, mu1, mu2,sigma){
    delta <- mu1-mu2
    t.crit <-qt(1-alpha,n1+n2-2)
    t.gamma <- delta/(sigma*sqrt(1/n1+1/n2))
    t.power <- 1-pt(t.crit,n1+n2-2,ncp=t.gamma)
    return(t.power)}

twosamplettestpow(0.05, 150, 300, 27, 24, 10)

#2b)
set.seed(2016)
pvals <- replicate(250000, t.test(24 + 10 * rt(n=300,df=5),
                                  27 + 10 * rt(n=150,df=5),
                                  alternative = "less",
                                  var.equal = T)$p.value) # studies stimulation

pow_t5 <- sum(pvals<=0.05)/250000 # calculating the power
pow_t5 # output

#2c)
set.seed(2016)
pvals <- replicate(250000, t.test(24 + 10 * rt(n=300,df=20),
                                  27 + 10 * rt(n=150,df=20),
                                  alternative = "less",
                                  var.equal = T)$p.value) # studies stimulation
pow_t20 <- sum(pvals<=0.05)/250000 # calculating the power

pow_t20 # output

#3a)
# How to display a matrix in rmarkdown
obs <- matrix(c(1,0,95,NA,2,1,NA,77,3,0,61,NA,4,0,53,NA,5,0,71,
                NA,6,1,NA,82,7,1,NA,70), ncol = 4, byrow = TRUE)
colnames(obs) <- c("Student", "T", "Yi(0)", "Yi(1)")
library(knitr)
kable(obs, digits = getOption("digits"))

#3d)
y0 <- c(76, 53, 65)
y1 <- c(79, 54, 69, 82)
both <- c(y0,y1) #pool data
N <- choose(7, 3)
res <- numeric(N) # to store the results
index <-combn(1:7, 3)
for (i in 1:N)
{
    res[i] <- mean(both[index[,i]])-mean(both[-index[,i]])
}
hist(res,xlab="ybar0-ybar1", main="Randomization Distribution of difference in means")
observed <- mean(y0)-mean(y1) #store observed mean difference
abline(v=observed,col="blue") #add line at observed mean diff
# of times values from the mean randomization distribution less than observed value
sum(res<observed) # Number of assignment resulting more extreme result
N # Number of randomizations
tbar <- mean(res)
#pval <- sum(abs(res-tbar)>=abs(observed-tbar))/N # Randomization p value
pval <- sum(res <= observed)/N
round(pval,2)