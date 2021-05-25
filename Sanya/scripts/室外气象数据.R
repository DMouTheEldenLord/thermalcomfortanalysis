library(ggplot2)
library(dplyr)
library(patchwork)


df <- "./data/Climate.csv" %>% 
  read.csv() %>% 
  mutate(Time = as.POSIXct(Time))

p1 <- ggplot(data = df, aes(x = Time, y = Tair)) +
  geom_line() +
  theme_bw() +
  theme(
    text = element_text(size = 10)
  ) +
  scale_x_datetime(breaks = "12 hours", date_labels = "%d日 %H时") +
  scale_y_continuous(breaks = 28:37) +
  labs(x = "时间",
       y = expression(paste("室外空气温度 /", degree, "C")))
p1

ggsave("温度.png",plot = p1, path = "./plot", dpi = 1200, height = 6, width = 11, units = "cm")


p2 <- ggplot(data = df, aes(x = Time, y = RH)) +
  geom_line() +
  theme_bw() +
  theme(
    text = element_text(size = 10)
  ) +
  scale_x_datetime(breaks = "12 hours", date_labels = "%d日 %H时") +
  scale_y_continuous(breaks = seq(0,100,5)) +
  labs(x = "时间",
       y = "相对湿度 /%")
p2
ggsave("湿度.png",plot = p2, path = "./plot", dpi = 1200, height = 6, width = 11, units = "cm")
