library(dplyr)
library(ggplot2)

datapath <- "./data/Tskin"
date <- dir(datapath)
df <- data.frame()

for(i in 1:length(date)){
  ls <- list.files(paste(datapath, date[i], sep = "/"))
  df1 <- paste(datapath, date[i], ls[1], sep = "/") %>% 
    read.csv() %>% 
    select(Time = 1, Temp = 2) %>% 
    mutate(Time = as.POSIXct(Time),
           No = date[i],
           Bodypart = unlist(strsplit(ls[j], "\\."))[1])
  for(j in 1:length(ls)){
    df1 <- paste(datapath, date[i], ls[j], sep = "/") %>% 
      read.csv() %>% 
      select(Time = 1, Temp = 2) %>% 
      mutate(Time = as.POSIXct(Time),
             No = date[i],
             Bodypart = unlist(strsplit(ls[j], "\\."))[1])
    df <- rbind(df, df1)
  }
}

ggplot(data = df, aes(x = Time, y = Temp, col = Bodypart, grp = Bodypart)) +
  geom_line()+
  facet_grid(.~No)
