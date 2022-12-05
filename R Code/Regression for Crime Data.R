library('tidyverse')
library('stargazer')
library('plm')
library('kableExtra')
library('rstatix')
library('psych')

#Load in data
df <- read.csv("C:/Users/Owner/Documents/ECON 485/Project on Crime/toomuchwork.csv")

#Edit df 
fun <- df %>%
  select(msa, year, pop, pcinc, pcrime, nfl, playoff) #reorder variables

fun = pdata.frame(fun, index = c("msa", "year")) #order df by msa

#Descriptive statistics
fun %>%
  get_summary_stats(
    pcrime, pop, pcinc, nfl, playoff,
    type = "common", show = c("n", "min", "max", "median", "mean")) %>% #grab sum stats
  kbl(caption = "Summary Statistics") %>%
  kable_styling(bootstrap_options = c("condensed"))

#Create log variables
fun <- fun %>%
  add_column(lncrime = log(fun$pcrime)) %>% #log of property crime just incase
  add_column(lnY = log(fun$pcinc)) %>% #log of per capita income
  

#Regressions and their output
pooled <- lm(pcrime ~ pop + lnY + nfl + playoff, data = fun) #pooled regression
regfd <- plm(pcrime ~ playoff, data = fun, model = "fd") #first difference with intercept
regfd2 <- plm(pcrime ~ playoff + 0, data = fun, model = "fd") #first difference with w/o intercept
regfe <- lm(pcrime ~ pop + lnY + nfl + playoff + msa, data = fun ) #fixed effects with msa
regfeboth <- lm(pcrime ~ pop + lnY + nfl + playoff + msa + year, data = fun) #fixed effects with msa and time 

stargazer(pooled, regfd, regfe, regfeboth, regfd2,type = "html", title = "Regression Results", 
          dep.var.labels = "Property Crime (Per 100,000 People)", covariate.labels = c("Pop", "ln(Y)", " Has NFL Team", " Made Playoffs"), out = "Crime Regression Output2.html") 

 


  
  
  
  
  

