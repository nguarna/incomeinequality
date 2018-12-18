m.none1 <- glm(govern ~ income10 + age_num + race + demrepub, data = GSS, family = "binomial")
newmodel<- stepAIC(m.none1, scope=list(upper= ~income10*age_num*race*demrepub, direction = "both", lower= ~1))
summary(newmodel)


#each additional $10,000 0f income is associated with 5.3% decline in the odds of agreeing 
#this result is technically expected for a black 49 year old, but because the effect of race is insignificant in this interaction, there is no
#reason to expect a different result for black versus white or other race people. 
#-0.19237 + 0.002812*49
#[1] -0.054582
#> exp( -0.054582)
#[1] 0.9468809
#> (0.9468809 - 1) * 100
#[1] -5.31191

#
#> exp(-2.086267)
#[1] 0.1241497
#> (0.1241497-1)*100
#[1] -87.58503


#for last hypothesis
library(lmtest)
m.first <- glm(govern ~ race+ age_num + income10, data = GSS, family = "binomial")
#adding in political party to model
m.second <- glm(govern ~ race+ demrepub +age_num + income10, data = GSS, family = "binomial")
lrtest(m.first, m.second)
summary(m.first)
summary(m.second)


#plots I removed
```{r include = FALSE}
plot1 <- qplot(x = income, y = factor(govern), color = demrepub, data = GSS, geom = "point")
plot2 <- qplot(x = income, y = factor(govern), color = race, data = GSS, geom = "point")
plot3 <- qplot(x = age_num, y = factor(govern), color = demrepub, data = GSS, geom = "point")
plot1
plot2
plot3
```

age_group <- cut(age_num, breaks = seq(18,90,by=10), right = TRUE)
m.a <- glm(govern ~ race+ age_group + income10, data = GSS, family = "binomial")
#adding in political party to model
m.b <- glm(govern ~ race+ demrepub + age_group + income10, data = GSS, family = "binomial")
summary(m.a)
summary(m.second)
