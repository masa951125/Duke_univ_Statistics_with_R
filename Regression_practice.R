library(tidyverse)
install.packages("ggrepel")
library(ggrepel)
install.packages("qqplotr")
library(qqplotr)

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

#multiple regression
states$ <- read_csv("states.csv")
ggplot(data=states$,aes(x=female_house, y=poverty)) +
  geom_point() +
  stat_smooth(method="lm", se=T)

pov_slr <-lm(poverty ~ female_house, data= states)
summary(pov_slr)
anova(pov_slr)

anova(lm(poverty~female_house,data=states))

pov_mlr <- lm(poverty ~female_house +white, data= states)  
summary(pov_mlr)

anova(pov_mlr)

pov_all<- lm(poverty ~metro_res+ white + hs_grad + female_house, data=states)
summary(pov_all)$adj.r.squared
#0.6104086

pov_all_1_f <-lm(poverty ~metro_res+ white + hs_grad, data=states)
summary(pov_all_1_f)$adj.r.squared
#0.6183401

pov_all_1_m <-lm(poverty ~white + hs_grad + female_house, data=states)
summary(pov_all_1_m)$adj.r.squared
#0.5498985

pov_all_1_w <-lm(poverty ~metro_res+ hs_grad + female_house, data=states)
summary(pov_all_1_w)$adj.r.squared
#0.6011228

pov_all_h<- lm(poverty ~metro_res+ white + female_house, data=states)
summary(pov_all_h)$adj.r.squared
# 0.3869346


pov_all_2 <-lm(poverty ~metro_res + hs_grad, data=states)
summary(pov_all_2)


###
load("mariokart.rda")
summary(mariokart)
str(mariokart)

#price cond_new stock photo duration wheels
mario_kart <- mariokart%>% select(total_pr, cond, stock_photo,duration, wheels) 

mario_kart <- mario_kart %>% rename("price" =total_pr)
mario_kart <- mario_kart %>% rename("cond_new" =cond)
mario_kart$cond_new <- ifelse(mario_kart$cond_new=="new",1,0)
mario_kart$stock_photo <- factor(mario_kart$stock_photo, levels=c(1,0))
mario_kart$cond_new <- factor(mario_kart$cond_new)

#eliminating 2 outliners
mario_kart <- mario_kart %>% filter(mario_kart$price<100)  


#cond_new
summary(lm(price~cond_new, data= mario_kart))

ggplot(data=mario_kart, aes(x=cond_new, y=price))+ 
  geom_point() +
  geom_abline(slope=10.900, intercept = 42.871,col="red")

#all
summary(lm(price~cond_new + stock_photo +duration+ wheels,data=mario_kart))

summary(lm(price~1, data=mario_kart))$adj.r.squared
mean(mario_kart$price)

reg <- lm(price~cond_new + stock_photo+ wheels,data=mario_kart)

#Normal probability plot
ggplot(data=reg,mapping= aes(sample=.resid)) +
  stat_qq_point(size = 3)

#residuals plot
ggplot(data=reg, aes(x=.fitted,y=.resid)) +
  geom_point(size=3)

#absolute values of residuals
ggplot(data=reg, aes(x=.fitted,y=abs(.resid)))+
  geom_point(size=3)

ggplot(data=reg, aes(x=index,y=.resid)) seq(.resid)+
  geom_point(size=3)

#order
plot(reg$residuals)

#boxplot
ggplot(data=reg, aes(x=cond_new,y=.resid)) +
  geom_boxplot()

ggplot(data=reg, aes(x=stock_photo,y=.resid)) +
  geom_boxplot()

ggplot(data=reg,aes(x=factor(wheels),y=.resid)) +
  geom_boxplot()

ggplot(data=reg,aes(x=factor(wheels),y=.resid)) +
  geom_point(size=3)

c(-0.08-qt(0.975,df=53)*0.12,-0.08+qt(0.975,df=53)*0.12)

pred <-c(names(evals[2:12]),names(evals[19:21]))
pred[1]

lm(score~names(evals[2]),data=evals)

lm(score~"rank",data=evals)
?formula

pred[-2]

r_list= c()

r_max =0
for(i in 1:length(pred)){
  fmla_i <- as.formula(paste("score ~ ", 
                             paste(pred[-i], collapse= "+")))
  r_i <-summary(lm(fmla_i,data=evals))$adj.r.squared
  if(r_i > r_max){
    r_max = r_i
    num=i
  }
}
print(c(r_max,pred[num]))

      



 
pred[1]

fmla <- as.formula(paste("score ~ ", paste(pred, collapse= "+")))
fmla
