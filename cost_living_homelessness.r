# Cost of living compared with homelessness

library(ggplot2)
library(plotly)
read_csv("C:\\Users\\Owner\\Documents\\R and Data Science\\data_tests\\cost_homeless.csv") -> h

ggplot(h, mapping = aes(x = costIndex, y = homeless_rate_per_10K))+geom_point()+geom_smooth(method = lm)

h1 <- ggplot(h, mapping = aes(x = costIndex, y = homeless_rate_per_10K, color =states))+geom_point()

ggplotly(h1, tooltip = "all") %>%layout(dragmode = "pan")

summary(lm(homeless_rate_per_10K ~ costIndex, h))

lm(homeless_rate_per_10K ~ costIndex, h) -> qq

qqplot(h$costIndex, h$homeless_rate_per_10K, plot.it = T)

