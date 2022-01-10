library(tidyverse)

url ="https://openintro.org/data/csv/loan50.csv"
df = read.csv(url)
str(df)
summary(df)

ggplot(data=df, aes(y=loan_amount,x=total_income)) +geom_point()
df$interest_rate %>% quantile(0.25) 

ggplot(data=df, aes(interest_rate)) +geom_histogram(col="black", bins=9)
ggplot(data=df, aes(interest_rate)) +geom_boxplot(col="black")+ coord_flip()

#statistic
df$interest_rate %>% mean() 
df$interest_rate %>% sd()
df$interest_rate %>% stats::IQR() 
df$interest_rate %>% quantile(0.25)
df$interest_rate %>% quantile(0.75) 

url2 ="https://openintro.org/data/csv/loans_full_schema.csv"
df_full <- read.csv(url2)
df_full %>% names()

t_own <-table(df_full$homeownership) %>% addmargins()
prop.table(table(df_full$homeownership)) %>% addmargins()

t <-table(df_full$application_type, df_full$homeownership)
t_app_own <-addmargins(t)

url3 <-"https://openintro.org/data/csv/email.csv"
email <- read.csv(url3)
email %>% str()

addmargins(table(email$spam, email$format))


url4 ="https://openintro.org/data/csv/avandia.csv"
d_avandia <- read.csv(url4)

d_avandia %>% str()

t <-table(d_avandia$treatment, d_avandia$cardiovascular_problems) 
t
chisq.test(t)
