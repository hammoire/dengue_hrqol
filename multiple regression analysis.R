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

#Modify data frame for analysis

qwb_regression <- qwb_deng %>% 
  mutate(capture = factor(capture, levels = c("cluster", "community","clinic")), #Reorder capture levels so that cluster is reference level
         age_group = factor(case_when(age_group == "[5,15]" ~ "5-15yrs",
                                      age_group == "(15,27]" ~ "15-27yrs",
                                      age_group == "(27,80]" ~ "27+yrs"), levels = c("5-15yrs", "15-27yrs", "27+yrs")),
         #Create score1
         #0.005 added to ensure any scores of zero would not lead - Inf values when applying qlogis function
         #Multiplied by 0.99 to ensure any score of 1 would not result in + Inf values when applying qlogis function
        score1 = qlogis(0.005 + 0.99*score))


#Mixed effects model 
    #Random effect: participant
    #Fixed effects: day of illness, recruitment method (capture), age, sex
mod_mixed <- lmer(score1 ~ day_illness+capture+age+sex+ (1|case_name), data = qwb_regression, REML = FALSE)
summary(mod_mixed)
AIC(mod_mixed)

#Fixed effects only model
    #Fixed effects: day of illness, recruitment method (capture), age, sex
mod_fixed <- lm(score1 ~ day_illness+capture+age+sex, data = qwb_regression)
summary(mod_fixed)
AIC(mod_fixed)
#Comparison of models
anova(mod_mixed, mod_fixed) #P=0.138 therefore no significant rnd effect by participant
#mod_fixed therfore final selected model
#From final model
    #day illness: qlogis(HRQoL Score) increases by 0.07 with every day of illness. p < 0.001 (when holding all other explan vars constant)
    #age: qlogis(HRQoL Score) decreases by 0.04 with every day of illness. p < 0.001 (when holding all other explan vars constant)
    #sex: qlogis(HRQoL Score) when male decreases by 0.09 but non significant p = 0.737 (when holding all other explan vars constant)

tmp1_df <- round(cbind(exp(coef(mod_fixed)), exp(confint(mod_fixed))), 2) %>% as.data.frame() %>% rownames_to_column()
tmp2_df <- round(cbind(exp(confint(mod_mixed)), c(NA, NA, exp(lme4::fixef(mod_mixed)))), 2) %>% as.data.frame() %>% rownames_to_column() %>% filter(!rowname %in% c(".sig01", ".sigma"))

tmp1_df %>% 
  bind_cols(tmp2_df) %>% 
  select(inde_var = rowname,
         OR_fixed = V1,
         ci0.25 = `2.5 %`,
         ci0.975 = `97.5 %`,
         OR_mixed = V3,
         ci0.25m = `2.5 %1`,
         ci0.975m = `97.5 %1`) %>% 
  mutate(CIf = str_c("OR:", ci0.25, "-", ci0.975),
         CIm = str_c("OR:", ci0.25m, "-", ci0.975m)) %>% 
  select(inde_var, OR_fixed, CIf, OR_mixed, CIm, everything()) 


#Model used for figures (The model used in Fig 3. uses the age groups rather than continuous age values used in mod_final)
mod_final_plot <- lm(score1 ~ day_illness+capture+age+sex, data = qwb_regression)
summary(mod_final_plot)
#Create dummy data set of predicted scores 
 
median_age_male <- median(qwb_regression$age[qwb_regression$sex == "male"])
quant_25 <- quantile(qwb_regression$age[qwb_regression$sex == "male"], probs = 0.25)
quant_75 <- quantile(qwb_regression$age[qwb_regression$sex == "male"], probs = 0.75)
median_age_female <- median(qwb_regression$age[qwb_regression$sex == "female"])
min_age_male <- min(qwb_regression$age[qwb_regression$sex == "male"])
max_age_male <- max(qwb_regression$age[qwb_regression$sex == "male"])
#Adjust this df to enable more complex plots
predicted_scores <- expand.grid(day_illness = 0:60, 
                    capture = c("cluster", "community", "clinic"),
                    age = c(median_age_male),
                    sex = c("male", "female")) %>% 
  mutate(age_facet = case_when(age == quant_25 ~ str_c("Age (25th pc):", quant_25), 
                               age == median_age_male ~ str_c("Age (median):", median_age_male), 
                               age == quant_75 ~ str_c("Age (75th pc):", quant_75)),
         age_facet = factor(age_facet, levels = c(str_c("Age (25th pc):", quant_25), 
                                                  str_c("Age (median):", median_age_male), 
                                                  str_c("Age (75th pc):", quant_75)))) %>% 
  filter(sex == "male") 

pred1 <- broom::augment(mod_fixed, newdata = predicted_scores) %>% 
  mutate(upper = .fitted - (2*.se.fit), lower = .fitted + (2*.se.fit))


#--------------------------------------------------------------------------
# Fig 3. HRQoL by day of illness, sex, age group and recruitment m --------
#--------------------------------------------------------------------------


fig_3 <- qwb_regression %>% 
  mutate(day_illness = ifelse(day_illness > 70, 50, day_illness)) %>% 
  ggplot(aes(x = as.numeric(day_illness), y = as.numeric(score), color = factor(capture))) +

  # scale_x_continuous() +
  # scale_y_continuous() +
  # geom_polygon(data = predicted_scores, aes(x = as.numeric(day_illness),
  #                                           y = plogis(predict(mod_fixed, newdata = predicted_scores))))
  geom_ribbon(data = predicted_scores %>% filter(day_illness <= 19), aes(x = as.numeric(day_illness),
                                           ymin = plogis(predict(mod_fixed, newdata = predicted_scores%>% filter(day_illness <= 19))),
                                           ymax = as.numeric(1)), alpha = 0, color = "black", linetype = "dotted") +
  geom_ribbon(data = predicted_scores, aes(x = as.numeric(day_illness),
                                                                         ymin = plogis(predict(mod_fixed, newdata = predicted_scores)),
                                                                         ymax = as.numeric(1)), fill = "grey", alpha = 0.5, linetype = "blank") +
  geom_line(data = predicted_scores, aes(x = as.numeric(day_illness),
                                         y = plogis(predict(mod_fixed, newdata = predicted_scores)),
                                         color = factor(capture)), size = 0.35) +
  geom_line(data = pred1, aes(x = as.numeric(day_illness),
                                         y = plogis(lower)), size = 0.2,  color = "grey") +
  geom_line(data = pred1, aes(x = as.numeric(day_illness),
                              y = plogis(upper)), size = 0.2,  color = "grey") +
  geom_point(alpha = 0.6,  size = 0.5, position = "jitter") +
  
  
    
    facet_wrap(~capture) +
  labs(x = "day of illness", y = "score") +
  theme_bw() +
  # scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.05)) 
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, vjust = -2.5),
        strip.text = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), 
        legend.position="top",
        strip.background = element_rect(fill = "#f0f0f0")) 

tiff("~/Desktop/Fig3.tiff", units="in", width=7, height=3.8, res=300)
fig_3
dev.off()

#--------------------------------------------------------------------------
# Fig 5. Disease burden calculation ---------------------------------------
#--------------------------------------------------------------------------

#Use the model with "average" settings
#Calculate the for the first 20 days (so can be compared easily to Zeng et al)
#Use median age 16
broom::augment(mod_fixed, newdata = predicted_scores) %>% 
  mutate(upper = .fitted - (2*.se.fit), lower = .fitted + (2*.se.fit)) %>% 
  filter(day_illness <= 19, age == 16) %>% 
  mutate(score = 1 - plogis(.fitted),
         score_lower = 1 -plogis(lower),
         score_upper = 1 -plogis(upper)) %>% 
  group_by(capture, age, sex) %>% 
  summarise(n(), 
            daly = sum(score)/365,
            daly_lower = sum(score_lower)/365,
            score_upper = sum(score_upper)/365) 


