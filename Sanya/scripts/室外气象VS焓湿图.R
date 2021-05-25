library(ggplot2)
library(reticulate)
library(dplyr)


rm(list = ls())
psypath <<- "./modules/psy" # psy文件夹的位置
scale.factor <<- 2.45 #用来调整等焓线的角度，英文推荐2.53，中文推荐2.45
source(paste(psypath, "chart.R", sep = "/"), encoding = "UTF-8")


df <- "./data/Climate.csv" %>% 
  read.csv() %>% 
  mutate(B = 101325,
         d = cal.d_Ta.RH(Tair, RH, B),
         h = cal.h_Ta.d(Tair, d),
         y = cal.y(h, d))

ta <- c(25.1, 23.6, 26.8, 28.3) 
d <- c(0,12,12,0)
h <- cal.h_Ta.d(ta,d)
y <- cal.y(h,d)
df.comfortzone <- data.frame(y,d)

p <- draw_psy(linesize = 0.1) +
  geom_point(data = df, aes(x = d, y =y)) +
  geom_polygon(data = df.comfortzone, aes(x = d, y = y, fill = "ASHRAE 0.5 clo 舒适区"), alpha = 0.3, col = "black", size = 0.1) +
  theme(
    text = element_text(size = 14),
    legend.title = element_blank(),
    legend.position = c(0.7,0.2)
  ) +
  labs(x = expression(paste("含湿量/", " g·k",g[干空气]^-1)),
       y = expression(paste("空气温度/", degree, "C"))) 
p
ggsave("焓湿图.png",plot = p, path = "./plot", dpi = 1200, height = 12, width = 11, units = "cm")