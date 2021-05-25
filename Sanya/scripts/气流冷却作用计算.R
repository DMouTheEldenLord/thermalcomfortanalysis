library(ggplot2)
library(reticulate)
library(dplyr)


rm(list = ls())
df <- data.frame(v = seq(0.2,3.0,0.1))

source_python("./scripts/calpmv.py") 

df <- py$df

ggplot(data = df, aes(x = v, y = PMV)) +
  geom_line() +
  theme_bw() +
  theme(
    text = element_text(size = 14)
  ) +
  scale_y_continuous(breaks = seq(0,4,0.1)) +
  scale_x_continuous(breaks = seq(0,4,0.5), minor_breaks = seq(0,4,0.1)) +
  labs(x = expression(paste("空气流速 /", "m·",s^-1)),
       y = expression(paste("PM",V[italic(adjusted)]))) +
  geom_hline(yintercept = c(1,0.5))

ggsave("PMV与空气流速.png", width = 11, height = 12, dpi = 1200, unit = "cm", path = "./plot")
