#Script to produce Tables 1 and 2 and Figures 1 and 2

#--------------------------------------------------------------------------
# Load required packages --------------------------------------------------
#--------------------------------------------------------------------------

# Random effect model -----------------------------------------------------
library(lmerTest)
# library(mgcv) #non-linear
library(ggplot2)

#--------------------------------------------------------------------------
# Multiple regression analysis --------------------------------------------
#--------------------------------------------------------------------------

#Reorder capture levels so that cluster is reference level
qwb_deng$capture <- factor(qwb_deng$capture, levels = c("cluster","clinic", "community"))

#Re-format labels for age groups
qwb_deng$age_group
qwb_deng <- qwb_deng %>% 
  mutate(age_group = factor(case_when(age_group == "[5,15]" ~ "5-15yrs",
                               age_group == "(15,27]" ~ "15-27yrs",
                               age_group == "(27,80]" ~ "27+yrs")))


#Create score1
#0.005 added to ensure any scores of zero would not lead - Inf values when applying qlogis function
#Multiplied by 0.99 to ensure any score of 1 would not result in + Inf values when applying qlogis function
qwb_deng$score1 <- qlogis(0.005 + 0.99*qwb_deng$score) 


#Mixed effects model 
    #Random effect: participant
    #Fixed effects: day of illness, recruitment method (capture), age, sex
mod_mixed <- lmer(score1 ~ day_illness+capture+age+sex+ (1|case_name), data = qwb_deng)
summary(mod_mixed)

#Fixed effects only model
    #Fixed effects: day of illness, recruitment method (capture), age, sex
mod_fixed <- lm(score1 ~ day_illness+capture+age+sex, data = qwb_deng)
summary(mod_fixed)
#Comparison of models
anova(mod_mixed, mod_fixed) #P=0.138 therefore no significant rnd effect by participant
#mod_fixed therfore final selected model
#From final model
    #day illness: qlogis(HRQoL Score) increases by 0.07 with every day of illness. p < 0.001 (when holding all other explan vars constant)
    #age: qlogis(HRQoL Score) decreases by 0.04 with every day of illness. p < 0.001 (when holding all other explan vars constant)
    #sex: qlogis(HRQoL Score) when male decreases by 0.09 but non significant p = 0.737 (when holding all other explan vars constant)


#Model used for figures (The model used in Fig 3. uses the age groups rather than continuous age values used in mod_final)
mod_final_plot <- lm(score1 ~ day_illness+capture+age_group+sex, data = qwb_deng)
summary(mod_final_plot)
#Create dummy data set of predicted scores 
 
predicted_scores <- expand.grid(day_illness = 0:60, 
                    capture = c("cluster", "clinic", "community"),
                    age_group = c("5-15yrs", "15-27yrs", "27+yrs"),
                    sex = c("male", "female"))


#--------------------------------------------------------------------------
# Fig 3. HRQoL by day of illness, sex, age group and recruitment m --------
#--------------------------------------------------------------------------


qwb_deng %>% 
  mutate(day_illness = ifelse(day_illness > 70, 50, day_illness)) %>% 
  ggplot(aes(x = day_illness, y = score, color = capture)) +
  geom_point(alpha = 0.5,  size = 2.5) +
  geom_point(data = qwb_deng %>% 
               mutate(day_illness = ifelse(day_illness > 70, 50, day_illness)) %>% filter(day_illness == 50), 
             aes(x = day_illness, y = score), col = "black", shape = "*", size = 6) +
  geom_line(data = predicted_scores, aes(x = day_illness,
                             y = plogis(predict(mod_final_plot, newdata = predicted_scores)),
                             colour = capture), size = 1) +
  facet_grid(sex ~ age_group) +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.05)) +
  theme(axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 28, vjust = 4),
        axis.title.x = element_text(size = 26, vjust = -2.5),
        strip.text = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 22),
        legend.title = element_blank(), 
        legend.position="top",
        strip.background = element_rect(fill = "#f0f0f0")) 

