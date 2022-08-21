install.packages("patchwork")
library(tidyverse)
library(patchwork)

#Confidence in financial institutes annual changes
levels <- levels(gss$confinan)
levels[length(levels) + 1] <- "No Answer"
gss$confinan <- factor(gss$confinan, levels = levels)
gss$confinan[is.na(gss$confinan)] <- "No Answer"

changes <- data.frame(addmargins(table(gss$year, gss$confinan)))
df_changes <- spread(data= changes,key =Var2 , value=Freq)
df_changes <-rename(df_changes, Year=Var1)

df_changes <- df_changes %>% mutate(pro_great=(`A Great Deal` /Sum), 
                                    pro_hardly=(`Hardly Any`/Sum))

df_changes <-df_changes[c(-1,-2,-3,-12,-30),] 

g <-ggplot(data=df_changes) +
  geom_line(aes(Year,pro_great,group=1,col="A great deal"))
g+ geom_line(aes(Year,pro_hardly,group=1, col="Hardly any"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Year")+ylab("Confidence")+
  ggtitle("Changes in confidence on financial institutes")+
  geom_vline(xintercept = c("2008","2012"),linetype=4)

#filter the data
gss_2012 <- gss %>% filter(year==2012)
gss_2008 <- gss %>% filter(year==2008) 

#comparison
g_2012 <- ggplot(data=gss_2012,aes(x=confinan,fill=race))+
  geom_bar()+
  ggtitle("2012 Confidence in financial institution")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 900))

g_2008 <- ggplot(data=gss_2008,aes(x=confinan,fill=race))+
  geom_bar()+
  ggtitle("2008 Confidence in financial institution")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 900))

g_2012 + g_2008

addmargins(table(gss_2012$race, gss_2012$confinan))
addmargins(table(gss_2008$race, gss_2008$confinan))

addmargins(prop.table(table(gss_2012$race, gss_2012$confinan)))
addmargins(prop.table(table(gss_2008$race, gss_2008$confinan)))

#Are there any differences in confidence among race groups(white, black, others)?

g_2012_fill <- ggplot(data=gss_2012,aes(x=confinan,fill=race))+
  geom_bar(position="fill")+
  ggtitle("2012 Confidence in financial institution")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_2008_fill <- ggplot(data=gss_2008,aes(x=confinan,fill=race))+
  geom_bar(position="fill")+
  ggtitle("2008 Confidence in financial institution")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_2012_fill + g_2008_fill

#hypothesis testing 
#for the proportion of people who have confidence in financial institution a great deal
#Ho: p_2008_great = p_2012_great, HA:p_2008_great > p_2012_great

n_2008 <-nrow(gss_2008)
n_2012 <-nrow(gss_2012)
alpha =0.05

p_great_2008 = mean(gss_2008$confinan=="A Great Deal")
p_great_2012 = mean(gss_2012$confinan=="A Great Deal")

p_diff = p_great_2012-p_great_2008
p_pool = (p_great_2008*n_2008 + p_great_2012*n_2012)/(n_2008+n_2012)

se_ci <- sqrt(p_great_2008*(1-p_great_2008)/n_2008 +
                p_great_2012*(1-p_great_2012)/n_2012)

ci_95 <-c(p_diff-qnorm(0.975)*se_ci, p_diff+qnorm(0.975)*se_ci)
ci_95
#[1] -0.07324728 -0.03580009

se <- sqrt(p_pool*(1-p_pool)/n_2008 + 
             p_pool*(1-p_pool)/n_2012 )

z_sta <- p_diff/ se
p_value <- pnorm(z_sta)
p_value < alpha
#we reject H_o and both upper and lower boundary ci is below 0

#hypothesis testing 
#for the proportion of people who hardly have any confidence in financial institution
#Ho: p_2002_hardly = p_2012_hardly, HA:p_2008_hardly < p_2008_hardly

p_hardly_2008 = mean(gss_2008$confinan=="Hardly Any")
p_hardly_2012 = mean(gss_2012$confinan=="Hardly Any")

p_diff_hardly = p_hardly_2012-p_hardly_2008
p_pool_hardly = (p_hardly_2008*n_2008 + p_hardly_2012*n_2012)/(n_2008+n_2012)

se_hardly <- sqrt(p_pool_hardly*(1-p_pool_hardly)/n_2008 + 
             p_pool_hardly*(1-p_pool_hardly)/n_2012 )

z_sta_hardly <- p_diff_hardly/ se_hardly
p_value_hardly <- pnorm(z_sta_hardly,lower.tail = F)
p_value_hardly < alpha
#we reject H_o

#The data provide convincing evidence that people who have a great deal of 
#confidence in financial institutions plummeted significantly from 2008 to 2012
#Furthermore, they provide convincing evidence that people who hardly have any 
#confidence in financial institutions increased significantly from 2008 to 2012


table_2012 <-addmargins(table(gss_2012$confinan, gss_2012$race))
chisq.test(table_2012)

table_2008 <-addmargins(table(gss_2008$confinan, gss_2008$race))
chisq.test(table_2008)

#"Hardly any confidence" among whites
#hypothesis testing 
#for the proportion of whites who hardly have any confidence in financial institution
#Ho: p_2002_hardly_whites = p_2012_hardly_whites
#HA: p_2008_hardly_whites < p_2012_hardly_whites

hardly_2008 <-gss_2008 %>% filter(confinan=="Hardly Any")
hardly_2012 <-gss_2012 %>% filter(confinan=="Hardly Any")

n_hardly_2008 <- nrow(hardly_2008)
n_hardly_2012 <- nrow(hardly_2012)

p_hardly_white_2008 = mean(hardly_2008$race=="White")
p_hardly_white_2012 = mean(hardly_2012$race=="White")

p_diff_hardly_white = p_hardly_white_2012 - p_hardly_white_2008

#ci
se_hardly_white_ci <- sqrt((p_hardly_white_2008*(1-p_hardly_white_2008)/n_hardly_2008+
                              (p_hardly_white_2012*(1-p_hardly_white_2012)/n_hardly_2012)))
ci_95_w <- c(p_diff_hardly_white- qnorm(0.975)*se_hardly_white_ci,
             p_diff_hardly_white+ qnorm(0.975)*se_hardly_white_ci)
ci_95_w 

p_pool_hardly_white = 
  (p_hardly_white_2008*n_hardly_2008 +
     p_hardly_white_2012*n_hardly_2012)/
  (n_hardly_2008+n_hardly_2012)

se_hardly_white <- sqrt(p_pool_hardly_white*(1-p_pool_hardly_white)/n_hardly_2008 + 
                    p_pool_hardly_white*(1-p_pool_hardly_white)/n_hardly_2012 )



z_sta_hardly_white <- p_diff_hardly_white/ se_hardly_white
p_value_hardly_white <- pnorm(z_sta_hardly_white,lower.tail = F)
p_value_hardly_white
#we reject H_o

#"Hardly any confidence" among blacks
#hypothesis testing 
#for the proportion of blacks who hardly have any confidence in financial institution
#Ho: p_2002_hardly_blacks = p_2012_hardly_blacks
#HA: p_2008_hardly_blacks > p_2012_hardly_blacks

p_hardly_black_2008 = mean(hardly_2008$race=="Black")
p_hardly_black_2012 = mean(hardly_2012$race=="Black")

p_diff_hardly_black = -(p_hardly_black_2012 - p_hardly_black_2008)
p_pool_hardly_black = (p_hardly_black_2008* n_hardly_2008 + 
                         p_hardly_black_2012* n_hardly_2012)/(n_hardly_2008+n_hardly_2012)

se_hardly_black <- sqrt(p_pool_hardly_black*(1-p_pool_hardly_black)/n_hardly_2008 + 
                          p_pool_hardly_black*(1-p_pool_hardly_black)/n_hardly_2012 )

z_sta_hardly_black <- p_diff_hardly_black/ se_hardly_black
p_value_hardly_black <- pnorm(z_sta_hardly_black,lower.tail = F)
p_value_hardly_black
#we reject H_o

#"Hardly any confidence" among others
#hypothesis testing 
#for the proportion of others who hardly have any confidence in financial institution
#Ho: p_2002_hardly_others = p_2012_hardly_others
#HA: p_2008_hardly_others > p_2012_hardly_others

p_hardly_other_2008 = mean(hardly_2008$race=="Other")
p_hardly_other_2012 = mean(hardly_2012$race=="Other")

p_diff_hardly_other = -(p_hardly_other_2012 - p_hardly_other_2008)
p_pool_hardly_other = (p_hardly_other_2008* n_hardly_2008 + 
                         p_hardly_other_2012* n_hardly_2012)/(n_hardly_2008+n_hardly_2012)

se_hardly_other <- sqrt(p_pool_hardly_other*(1-p_pool_hardly_other)/n_hardly_2008 + 
                          p_pool_hardly_other*(1-p_pool_hardly_other)/n_hardly_2012 )

z_sta_hardly_other <- p_diff_hardly_other/ se_hardly_other
p_value_hardly_other <- pnorm(z_sta_hardly_other,lower.tail = F)
p_value_hardly_other
#we reject H_o

#"A Great Deal" of confidence" among whites
#hypothesis testing 
#for the proportion of whites who have a great deal of confidence in financial institution
#Ho: p_2002_great_whites = p_2012_great_whites
#HA: p_2008_great_whites < p_2012_great_whites

great_2008 <-gss_2008 %>% filter(confinan=="A Great Deal")
great_2012 <-gss_2012 %>% filter(confinan=="A Great Deal")

n_great_2008 <- nrow(great_2008)
n_great_2012 <- nrow(great_2012)

p_great_white_2008 = mean(great_2008$race=="White")
p_great_white_2012 = mean(great_2012$race=="White")

p_diff_great_white = p_great_white_2012 - p_great_white_2008

#ci
se_great_white_ci <- sqrt((p_great_white_2008*(1-p_great_white_2008)/n_great_2008+
                              (p_great_white_2012*(1-p_great_white_2012)/n_great_2012)))
ci_95_w <- c(p_diff_great_white- qnorm(0.975)*se_great_white_ci,
             p_diff_great_white+ qnorm(0.975)*se_great_white_ci)
ci_95_w 

p_pool_great_white = 
  (p_great_white_2008*n_great_2008 +
     p_great_white_2012*n_great_2012)/
  (n_great_2008+n_great_2012)

se_great_white <- sqrt(p_pool_great_white*(1-p_pool_great_white)/n_great_2008 + 
                          p_pool_great_white*(1-p_pool_great_white)/n_great_2012 )

z_sta_great_white <- p_diff_great_white/ se_great_white
p_value_great_white <- pnorm(z_sta_great_white,lower.tail = F)
p_value_great_white
#we fail to reject H_o

#"Great confidence" among blacks
#hypothesis testing 
#for the proportion of blacks who have a great deal of confidence in financial institution
#Ho: p_2002_great_blacks = p_2012_great_blacks
#HA: p_2008_great_blacks > p_2012_great_blacks

p_great_black_2008 = mean(great_2008$race=="Black")
p_great_black_2012 = mean(great_2012$race=="Black")

p_diff_great_black = p_great_black_2012 - p_great_black_2008
p_pool_great_black = (p_great_black_2008* n_great_2008 + 
                         p_great_black_2012* n_great_2012)/(n_great_2008+n_great_2012)

se_great_black <- sqrt(p_pool_great_black*(1-p_pool_great_black)/n_great_2008 + 
                          p_pool_great_black*(1-p_pool_great_black)/n_great_2012 )

z_sta_great_black <- p_diff_great_black/ se_great_black
p_value_great_black <- pnorm(z_sta_great_black,lower.tail = F)
p_value_great_black
#we reject H_o

#"Great confidence" among others
#hypothesis testing 
#for the proportion of others who have great deal of confidence in financial institution
#Ho: p_2002_great_others = p_2012_great_others
#HA: p_2008_great_others > p_2012_great_others

p_great_other_2008 = mean(great_2008$race=="Other")
p_great_other_2012 = mean(great_2012$race=="Other")

p_diff_great_other = p_great_other_2012 - p_great_other_2008
p_pool_great_other = (p_great_other_2008* n_great_2008 + 
                         p_great_other_2012* n_great_2012)/(n_great_2008+n_great_2012)

se_great_other <- sqrt(p_pool_great_other*(1-p_pool_great_other)/n_great_2008 + 
                          p_pool_great_other*(1-p_pool_great_other)/n_great_2012 )

z_sta_great_other <- p_diff_great_other/ se_great_other
p_value_great_other <- pnorm(z_sta_great_other,lower.tail = T)
p_value_great_other
#we fail to reject H_o

#From these three hypothesis tests, the data provide convincing evidence that these hypothesizes are true
#HA: p_2008_hardly_whites < p_2012_hardly_whites
#HA: p_2008_hardly_blacks > p_2012_hardly_blacks
#HA: p_2008_hardly_others > p_2012_hardly_others
#In other words, the proportion of whites who have hardly any confidence in financial institutions
#has increased from 2008 to 2012, in contrast, regarding blacks and others, the proportions are 
#decreased.

####
#convert NA to "No Answer"
levels <- levels(gss_2012$confinan)
levels[length(levels) + 1] <- "No Answer"
gss_2012$confinan <- factor(gss_2012$confinan, levels = levels)
gss_2012$confinan[is.na(gss_2012$confinan)] <- "No Answer"

levels <- levels(gss_2008$confinan)
levels[length(levels) + 1] <- "No Answer"
gss_2008$confinan <- factor(gss_2008$confinan, levels = levels)
gss_2008$confinan[is.na(gss_2008$confinan)] <- "No Answer"

