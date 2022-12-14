library(afex)
library(performance)
library(tidyverse)
library(emmeans)

#<READING DATA>
challenge_data2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")
head(challenge_data2)
#between participants
#DV is RT
#4 levels of the condition

#<TIDY>
challenge_data2_tidied <- challenge_data2 %>%
  mutate(Condition = factor(Condition))
head(challenge_data2_tidied)
#we want to know if there is a difference between conditions
#and where

#<SUMMARY>
challenge_data2_tidied %>%
  group_by(Condition) %>%
  summarise(mean = mean(RT), sd = sd(RT))

#<VISUALISE>
plot5 <- challenge_data2_tidied %>%
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "RT (s)")

#<BUILD ANOVA MODEL>
challenge_model2 <- aov_4 (RT ~ Condition + (1 | Subject), data = challenge_data2_tidied)

#output
anova(challenge_model2)
#there is a difference
#where is it

#<INTERPRETING>
emmeans(challenge_model2, pairwise ~ Condition)
#difference between all of them, as expected from the visualisation
