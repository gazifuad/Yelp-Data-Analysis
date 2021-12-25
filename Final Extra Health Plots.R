library(tidyverse)
library(plm)
library(viridis)

load("~/Desktop/STAT405 Home/Presentation/All American.rda")
load("~/Desktop/STAT405 Home/Presentation/All Fast Food.rda")


#Since already doing bars for killer plot decided to mix it up and do fast food

#Explored slighlty different in killer plot, but still diff enough

ggplot(all_ff) +
  aes(x = `Review Count`, y = Obesity, shape = `Average Stars`, color = `Mental Health Problems`) +
  facet_wrap(~state, scales = "free") + #in place of this line could subset a state instead
  geom_jitter() +
  scale_color_viridis(option = "D")+
  labs(x = "Number of Fast Food Reviews",
       y = "Crude Prevalence of Obesity (%)",
       color = "Mental Health Issues (%)")


ggplot(all_american) +
  aes(x = `Binge Drinking`, y = heart, shape = `Average Stars`,
      color = `High Blood Pressure`, size = Stroke) +
  facet_wrap(~state, scales = "free") + #in place of this line could subset a state instead
  geom_jitter() +
  scale_color_viridis(option = "D") +
  xlab("Smoking") +
  ylab("Heart Condition Prevalence (%)")+
  labs(color = "High BP (%)")


