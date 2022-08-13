library(tidyverse)
install.packages("ggrepel")
library(ggrepel)

p=0.5
n=10
B=1000
test_data <- replicate(B,{
  res=sample(c(1,0),prob=c(p,1-p),size=n,replace=T)
  r=sum(res)
  w=n-sum(res)
  c(r,w)
})
test_data <-data.frame(test_data)
test_data <- t(data.frame(test_data))              
test_data <-data.frame(test_data)
cor(test_data$X2, test_data$X1)

ggplot(data=test_data) + geom_point(aes(x=X1, y=X2))

midterm <-read.csv("midterms_house.csv")
names(midterm$)
summary(midterm)

r_midterm <- midterm %>% filter(unemp <15 & year <2013)


ggplot(data=r_midterm,aes(x=unemp, y=house_change,label=potus)) +
  geom_point(col="red")+
  geom_text_repel(size=3)+
  stat_smooth(method = "lm", se =T)

reg_model <-lm(house_change~unemp, data=r_midterm)
summary(reg_model)

#residual plot
ggplot(data= reg_model, aes(x=.fitted, y=.resid))+
  geom_point()+
  geom_hline(yintercept = 0, linetype="dashed")

#normal probabilty plot
ggplot(data= reg_model, aes(sample=.resid))+
  stat_qq()
  
43.5755 + 69 * c(0.2863 -qt(0.975, df=168) *0.0686,0.2863+qt(0.975, df=168) *0.0686)
c(0.877 -qt(0.975, df=250)*0.067, 0.877 +qt(0.975, df=250)*0.067)