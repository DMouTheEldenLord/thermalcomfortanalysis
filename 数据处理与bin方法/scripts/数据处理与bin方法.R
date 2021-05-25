library(ggplot2)
library(dplyr)


df <- read.csv("./data/TSV-Ta数据.csv")
df.bin <- df %>% 
  mutate(Tabin = cut_width(Ta, width = 1, center = 25)) %>% 
  group_by(Tabin) %>% 
  summarise(TSVmean = mean(TSV),
            observations = length(TSV),
            Tamean = mean(Ta))

ls.raw <- summary(lm(data = df,formula = TSV~Ta))
eqn.raw <- as.character(as.expression(
  substitute(italic(TSV) == k * italic(T[a]) + b* "," ~~ italic(r)^2 ~ "=" ~ r2 ~~ italic(p) ~ "< 0.0001",
             list(
               k = format(ls.raw$coefficients[2,1], digits = 3),
               b= format(ls.raw$coefficients[1,1], digits = 3),
               r2 = format(ls.raw$r.squared, digits = 3)
             )
  )))

ls.bin <- summary(lm(data = df.bin,formula = TSVmean~Tamean))
eqn.bin <- as.character(as.expression(
  substitute(italic(TSV) == k * italic(T[a]) + b* "," ~~ italic(r)^2 ~ "=" ~ r2 ~~ italic(p) ~ "< 0.0001",
             list(
               k = format(ls.bin$coefficients[2,1], digits = 3),
               b= format(ls.bin$coefficients[1,1], digits = 3),
               r2 = format(ls.bin$r.squared, digits = 3)
             )
  )))



ggplot() +
  geom_point(data = df, aes(x = Ta, y = TSV, col = "raw"),alpha = 0.2) +
  geom_smooth(data = df, aes(x = Ta, y = TSV, col = "raw"), method = "lm") +
  geom_point(data = df.bin, aes(x = Tamean, y = TSVmean, size = observations, col = "binned"), shape = 1) +
  geom_smooth(data = df.bin, aes(x = Tamean, y = TSVmean, col = "binned"), 
              method = "lm") +
  theme_bw() +
  annotate(geom = "text", x = 13.5, y = 1.5,
           label= eqn.raw,
           size = 2, 
           parse=TRUE) +
  annotate(geom = "text", x = 13.5, y = 3,
           label= eqn.bin,
           size = 2, 
           parse=TRUE) 

  

