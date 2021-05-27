

# this script attempts to test the properties of the distribution of residuals, which should be a uniform distribution between 0 and 1.

library(stats)
library(spgs)


uniform_test <- function(db_with_quotas) {
  
  db_with_quotas <- db_with_quotas %>% 
  mutate(residual = quota - floor(quota)) 

  ggplot(data = db_with_quotas, aes(x = residual)) + geom_histogram(bins = 50)

  ks_summary <- ks.test(db_with_quotas$residual, "punif", 0, 1, alternative = "two.sided")
  ks_summary_runif <- ks.test(runif(446), "punif", 0, 1, alternative = "two.sided")
  

  ed <- ecdf(db_with_quotas$residual)
  maxdiffidx <- which.max(abs(ed(db_with_quotas$residual)-punif(db_with_quotas$residual,0,1)))
  maxdiffat <- db_with_quotas$residual[maxdiffidx]

  p <- ggplot(aes(residual),data=db_with_quotas)+stat_ecdf()+theme_bw()+stat_function(fun=punif,args=list(0,1))
  p <- p+labs(title="ECDF and theoretical CDF")+geom_vline(xintercept=maxdiffat, lty=2)

  u <- runif(446)
  chisq.unif.test(u)
  v <- db_with_quotas$residual
  chisq.unif.test(v)
}




