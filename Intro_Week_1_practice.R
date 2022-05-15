library(tidyverse)

url="https://openintro.org/data/csv/loan50.csv"
da = read_csv(url)
head(da)
names(da)
ggplot(data=da, aes(x=))

url2= "https://openintro.org/data/csv/usairports.csv"
db= read_csv(url2)