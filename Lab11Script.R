library(pwr)
library(tidyverse)
library(effectsize)

pwr.t.test(d = 0.65, # large effect
           power = 0.80,
           sig.level = 0.05,
           alternative = "two.sided",
           type = "one.sample")

n = 21

closer = read_csv("closer.csv") |>
  select(1)
farther = read_csv("farther.csv") |>
  select(1)

closer.v = closer$Values
farther.v = farther$Values

differences = c()

for(i in 1:length(closer.v)){
  differences[i] = closer.v[i] -farther.v[i]
}

data = tibble(closer = closer.v, farther = farther.v, difference = differences)

ggplot(data = data) +
  geom_boxplot(aes(x = closer, color = "Closer")) +
  geom_boxplot(aes(x = farther, color = "Farther")) +
  scale_color_manual(values = c("Closer" = "blue", "Farther" = "red"), 
                     name = "Guide")+
  theme_minimal()+
  xlab("Change in Dopamine")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggplot(data)+
  geom_boxplot(aes(x=difference))+
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())+
  xlab("Difference in Dopamine")


t.test(data$closer, alternative = "greater")
hedges_g(x = data$closer, alternative = "greater")
t.test(data$closer)

#(t = 8.3024, p < 0.0001; g = 1.34; 95% CI: 0.1173875, 0.1950586)

t.test(data$farther, alternative = "less")
hedges_g(x = data$farther, alternative = "less")
t.test(data$farther)

#(t = -7.778, p < 0.0001; g = -1.51; 95% CI: -0.2565176, -0.1489313)

t.test(data$difference)
hedges_g(x = data$difference)

#(t = 8.5109, p < 0.0001; g = 1.65; 95% CI: 0.2719028, 0.4459921)

graph = tibble(x=seq(-15,15, length.out = 1000), 
               y = dt(x,24), 
               y1=dt(x, 24, 8.3024),
               y2=dt(x,24,-7.778),
               y3=dt(x,24,8.5109))

ggplot(graph)+
  geom_line(aes(x=x,y=y))+
  geom_point(aes(x=8.3024, y = pt(8.3024,24,lower.tail = F)),color = "blue")+
  theme_minimal()+
  ylab("Density")+
  geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x > qt(0.95,24), y, NA)),
              fill = "orange", alpha = 0.3)+
  geom_line(aes(x=x,y=y1))+
  geom_hline(yintercept = 0)
  

ggplot(graph)+
  geom_line(aes(x=x,y=y))+
  geom_point(aes(x=-7.778, y = pt(-7.778,24,lower.tail = T)),color = "red")+
  theme_minimal()+
  ylab("Density")+
  geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x < qt(0.05,24), y, NA)),
              fill = "orange", alpha = 0.3)+
  geom_line(aes(x=x,y=y2))+
  geom_hline(yintercept = 0)
  

ggplot(graph)+
  geom_line(aes(x=x,y=y))+
  geom_point(aes(x=8.5109, y = pt(8.5109,24,lower.tail = F)),color = "purple")+
  theme_minimal()+
  ylab("Density")+
  geom_ribbon(aes(x = x, ymin = 0, ymax = ifelse(x < qt(0.025,24) | x > qt(0.975,24), y, NA)),
              fill = "orange", alpha = 0.3)+
  geom_line(aes(x=x,y=y3))+
  geom_hline(yintercept = 0)


