#Script to produce Tables 1 and 2 and Figures 1 and 2
#See lines 

#--------------------------------------------------------------------------
# Load required packages --------------------------------------------------
#--------------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(kableExtra)
library(lemon)

#--------------------------------------------------------------------------
# Read in data ------------------------------------------------------------
#--------------------------------------------------------------------------

qwb_deng <- read_csv("input/qwb_deng.csv")

#--------------------------------------------------------------------------
# Table 1. Participant characteristics ------------------------------------
#--------------------------------------------------------------------------

note <- "* Capture method referes to the mode of surveillance leading to participant recruitment: \n  Active: captured in cohort, Passive: captured in clinic/hospital."
qwb_deng %>% 
  mutate(day_of_capture = first_blood-first_symptom,
         capture_mod = ifelse(index_contact == "contact", index_contact, capture)) %>% 
  group_by(case_name, age, sex, index_contact, capture_mod, hospitalized, diagnosis, day_of_capture) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(`Number of cases` = n(),
            `Number of forms` = sum(n),
            `Sex (%)` = "",
            Male = str_c(sum(sex == "male"), " (", 100*round(sum(sex == "male")/n(), digits = 2), "%)"),
            Female = str_c(sum(sex == "female"), " (", 100*round(sum(sex == "female")/n(), digits = 2), "%)"),
            `Median age (IQR)` = str_c(round(median(age), digits = 2), " (", quantile(age, 0.25), "-", quantile(age, 0.75),")"),
            `Day at diagnosis (IQR)` = str_c(round(median(day_of_capture, na.rm = TRUE), digits = 2), " (", quantile(day_of_capture, 0.25, na.rm = TRUE), "-", quantile(day_of_capture, 0.75, na.rm = TRUE),")"),
            `Capture method (%)` = "",
            Clinic = str_c(sum(capture_mod == "clinic"), " (", 100*round(sum(capture_mod == "clinic")/n(), digits = 2), "%)"),
            Community = str_c(sum(capture_mod == "community"), " (", 100*round(sum(capture_mod == "community")/n(), digits = 2), "%)"),
            Contact = str_c(sum(capture_mod == "contact"), " (", 100*round(sum(capture_mod == "contact")/n(), digits = 2), "%)"),
            `Hospitalized (%)` = str_c(sum(hospitalized), " (", 100*round(sum(hospitalized)/n(), digits = 2), "%)"),
            `Serotype (%)` = "",
            DEN2 = str_c(sum(diagnosis == "deng2"), " (", 100*round(sum(diagnosis == "deng2")/n(), digits = 2), "%)"),
            DEN3 = str_c(sum(diagnosis == "deng3"), " (", 100*round(sum(diagnosis == "deng3")/n(), digits = 2), "%)")) %>% 
  t() %>% 
  as.data.frame() %>% 
  rename(Value = V1) %>% 
  mutate(Characteristic = row.names(.))  %>% 
  select(Characteristic, everything()) %>% 
  kable("html",  align = 'c', escape = F) %>%
  kable_styling(bootstrap_options = "bordered", full_width = F, position = "center")

#--------------------------------------------------------------------------
# Fig 1. Heatmap: Proportion of participants reporting symptoms -----------
#--------------------------------------------------------------------------

#Select questions for concise version of heatmap
sig_quest <- {c("Fever or chills", "Headache", "Fatigue", "Anorexia", "Abdominal pain", 
               "Eye pain", "Weight change", "Dizziness", "Change bowels", "Itching", 
               "Balance problem", "Sore throat", "Insomnia", "Feeling upset", 
               "Nervousness", "Affect school/work", "Affect personal life", 
               "Bedbound", "Change plans", "Avoid walking", "Difficulty bending", 
               "Difficulty carrying", "Hospitalized")}

# Data frame for heatmap 
qwb_heatmap <- qwb_fisher_full %>% 
  filter(question %in% sig_quest) %>% 
  ungroup() %>% 
  select(question, `early_acute (n = 69)`, `late_acute (n = 67)` , 
         `convalescent (n = 59)`) %>% 
  gather(phase, lab, -question) %>% 
  separate(col = lab, into = c("num", "lab"), sep = "__") %>%
  mutate(prop = as.numeric(str_remove_all(lab, "\\*")),
         group = case_when(question %in% physical_qs ~ "physical",
                           question %in% pyschological_qs ~ "psychological",
                           question %in% scoial_qs ~ "social"),
         phase = factor(phase, levels = c("early_acute (n = 69)", "late_acute (n = 67)", "convalescent (n = 59)"))) %>% 
  group_by(question) %>% 
  arrange(group, question) %>% 
  mutate(order_var = first(prop)) %>% #enables nice ordering of heatmap rows
  ungroup() 

p <- ggplot(qwb_heatmap, aes(x = factor(phase), y = fct_reorder(question, order_var), fill = prop)) +
  geom_tile() +
  geom_text(aes(label = lab), hjust=0, nudge_x = -0.1, size = 6, data = filter(qwb_heatmap %>% filter(group == "psychological"))) +
  geom_text(aes(label = lab),hjust=0, nudge_x = -0.1, size = 6, data = filter(qwb_heatmap %>% filter(group == "physical"))) +
  geom_text(aes(label = lab), hjust=0, nudge_x = -0.1, size  = 6, data = filter(qwb_heatmap %>% filter(group == "social"))) +
  # facet_grid(1 ~ group , scales = "free", space = "free")
  facet_rep_wrap(~group, ncol = 1, scales='free_y', repeat.tick.labels = 'left') +
  scale_x_discrete(expand = c(0, 0)) +    
  scale_y_discrete(expand = c(0, 0)) + 
  theme_bw() +
  # theme(axis.text.x = element_blank())
  scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Proportion") +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(vjust = -5, size = 18, ),
        legend.title = element_text(size = 16),
        axis.ticks = element_blank(),     
        strip.text = element_text(colour="black", size = 20, face=2),
        strip.background = element_rect(fill= "white"),
        strip.text.x = element_text(margin = margin(0.3,0,0.3,0, "cm")),
        legend.key.size = unit(35, units = "pt"),
        legend.text = element_text(size = 14),
        plot.margin = unit(c(1,1,1,1), "cm"))


# Access gtab to alter row heights to match
g <- ggplot_build(p) 
## find out how many y-breaks are in each panel
## to infer the number of tiles
vtiles <- map_dbl(map(g$layout$panel_params, "y.major"), length)
gt <- ggplot_gtable(g)
## find out which items in the layout correspond to the panels
## we refer to the "t" (top) index of the layout
panels <- gt$layout$t[grepl("panel", gt$layout$name)]
## replace the default panel heights (1null) with relative sizes 
## null units scale relative to each other, so we scale with the number of tiles
gt$heights[panels[1]] <-unit(vtiles[1], "null")
gt$heights[panels[2]] <-unit(vtiles[2], "null")
gt$heights[panels[3]] <-unit(vtiles[3], "null")
## draw on a clean slate
library(grid)
grid.newpage()
grid.draw(gt)

#--------------------------------------------------------------------------
# Table 2. HRQoL Scores by illness phase ----------------------------------
#--------------------------------------------------------------------------

#Define participants who can be included in the paired analysis (a form in each illness phase)
#Determine which participants completed a form in each illness phase
all <- qwb_deng %>% 
  group_by(case_name, phase) %>% 
  count() %>% 
  filter(n == 1) %>% 
  group_by(case_name) %>% 
  summarise(all = sum(n, na.rm = TRUE)) %>% 
  filter(all == 3) %>% pull(case_name)


qwb_deng %>%
  mutate(phase = factor(phase, levels = c("early_acute",  "late_acute", "convalescent"))) %>% 
  # filter(case_name %in% all) %>% #uncomment for paired analysis
  group_by(phase = as.character(phase)) %>% 
  summarise(forms = n(),
            participants = length(unique(case_name)),
            `median score (IQR)` = str_c(round(median(score, na.rm = TRUE), digits = 2), 
                                         " (", 
                                         round(unname(quantile(score))[2], digits = 2), 
                                         "-", 
                                         round(unname(quantile(score))[4], digits = 2), ")"),
            range = str_c(round(min(score, na.rm = TRUE), digits = 2), 
                          "-", 
                          round(max(score, na.rm = TRUE), digits = 2))) %>% 
  mutate(phase = factor(phase, levels = c("early_acute", "late_acute", "convalescent"), ordered = TRUE)) %>% 
  arrange(phase) %>% 
  mutate(phase = as.character(phase)) %>% 
  kable("html",  align = 'c', escape = F) %>%
  kable_styling(bootstrap_options = "bordered", full_width = F, position = "center")

#--------------------------------------------------------------------------
# Fig 2. HRQoL score by illness phase -------------------------------------
#--------------------------------------------------------------------------

#supplementary df for labels
qwb_score_phase <- qwb_deng %>% 
  mutate(phase = factor(phase, levels = c("early_acute",  "late_acute", "convalescent"))) %>% 
  # filter(case_name %in% all) %>% #uncomment for paired analysis
  group_by(phase) %>% 
  count()

qwb_deng %>%
  mutate(phase = factor(phase, levels = c("early_acute",  "late_acute", "convalescent"))) %>% 
  # filter(case_name %in% all) %>% #uncomment for paired analysis
  filter(!is.na(score)) %>% 
  ggplot(aes(x = phase, y = score)) +
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "red", size = 3) +
  geom_label(data = qwb_score_phase, aes(x = phase, y = 0.1, label = str_c("n=",n)), size = 8) +
  labs(
    # title = "FIGURE 1. HRQoL score by illness phase",
    y = "HRQoL score",
    x = "") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.35)) +
  theme(plot.title = element_text(hjust = 0.5, size = 27, vjust = 5), 
        axis.text.y = element_text(size = 30),
        axis.text.x = element_text(size = 30),
        plot.margin = unit(c(0.1,1,0.1,1), "cm"),
        axis.title.y = element_text(vjust = 4, size = 35),
        axis.title.x = element_text(size = 35, vjust = -5),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line()) +
  coord_capped_cart(left='both') +
  stat_compare_means(comparisons = list(c("early_acute", "late_acute"), 
                                        c("late_acute", "convalescent")),
                     label.y = c(1.2, 1.3),
                     size = 12, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns"))) 


#--------------------------------------------------------------------------
# Frequency (%) of all physical and psychological and social symptoms -----
#--------------------------------------------------------------------------

# Response frequencies & Fisher’s exact
#Define physical, psycological and social variables
physical_qs <- {c("Blindness (bilateral)", "Blindness (unilateral)", "Speech problem", 
                  "Paralysis of limbs", "Paralysis of digits", "Deformity", "Fatigue", 
                  "Weight change", "Over or under weight", "Chewing problem", 
                  "Deafness", "Skin condition", "Itching", "Visual problem", "Eye pain", "Headache", "Dizziness", "Ear symptoms", 
                  "Nose symptoms", "Sore throat", "Toothache", "Oral bleeding", 
                  "Cough", "Shortness of breath", "Chest pain", "Abdominal pain", 
                  "Change bowels", "Dysuria", "Reduced bladder control", 
                  "Genital symptoms", "Broken bones", "Back or neck pain", "Hip pain", 
                  "Joint pain", "Edema", "Fever or chills", "Loss of conciousness", 
                  "Balance problem", "Took medication", "Anorexia")}
pyschological_qs <- {c("Insomnia", "Nervousness", "Feeling upset", 
                       "Anxiety", "No control of life", "Loneliness", "Frustration", 
                       "Hungover^", "Reduced libido", "Confusion", "Invasive thoughts")}
scoial_qs <-   {c("Use of health appliances", "On medical diet", "Hospitalized", 
                  "Help with personal care", "Drove vehicle", "Used public transport", 
                  "Transport not used", "Difficulty with stairs", "Avoid walking", 
                  "Limp or walking aids", "Difficulty bending", "Difficulty carrying", 
                  "Other physical limitation", "Bedbound", "Used wheelchair", "Wheelchair controlled by other", 
                  "Affect school/work", "Affect personal life", 
                  "Change plans")}
case <- {c("case_id", "case_name", "age", "age_group", "sex", "diagnosis","capture", "hospitalized", "cluster_code", 
           "index_contact", "first_blood", "first_symptom", "first_fever")}

#Create data frame for fisher's exact
qwb_fisher_pre <- qwb_deng %>% 
  select(-score) %>% 
  gather(question, present, -case,  -phase, -day_illness, -health_over_scale) %>% 
  mutate(present = as.logical(present),
         group = case_when(question %in% physical_qs ~ "physical",
                           question %in% pyschological_qs ~ "psychological",
                           question %in% scoial_qs ~ "social")) %>% 
  group_by(question, group, phase) %>% 
  summarise(n = n(), 
            num = sum(present,  na.rm = TRUE),
            prop =round((sum(present,  na.rm = TRUE)/n()), digits = 2)) %>% 
  mutate(phase = str_c(phase, " (n = ", n, ")")) %>% 
  group_by(question, group) %>%
  select(-n) %>% 
  mutate(prop =   ifelse(nchar(as.character(prop)) == 4, as.character(prop),
                         ifelse(nchar(as.character(prop)) == 3, str_c(as.character(prop), "0"), "0.00"))) %>%
  unite(col = temp, num, prop, sep = "__") %>% 
  spread(phase, temp) 

# Vector of questions
question_vars <- qwb_fisher_pre$question

# Perform fisher's exact test
qwb_fisher <- map_df(question_vars, function(x){
  #acute v conv
  qwb_early_acute <-  qwb_deng[qwb_deng$phase != "late_acute",]
  qwb_early_acute <- droplevels(qwb_early_acute)
  question_early_acute <- qwb_early_acute[[x]]
  if(nlevels(factor(question_early_acute)) < 2){
    p_early_acute <- NA
    sig_95_early_acute <- NA
    sig_99_early_acute <- NA
  }else{
    con_tab_early_acute <- table(question_early_acute, qwb_early_acute$phase)
    fishy_early_acute <- fisher.test(con_tab_early_acute, simulate.p.value = TRUE)
    p_early_acute <- fishy_early_acute$p.value
    sig_95_early_acute <- fishy_early_acute$p.value < 0.05/70
    sig_99_early_acute <- fishy_early_acute$p.value < 0.01/70
  }
  
  
  #late_acute v conv
  qwb_late_acute <-  qwb_deng[qwb_deng$phase != "early_acute",]
  qwb_late_acute <- droplevels(qwb_late_acute)
  question_late_acute <- qwb_late_acute[[x]]
  
  
  if(nlevels(factor(question_late_acute)) < 2){
    p_late_acute <- NA
    sig_95_late_acute <- NA
    sig_99_late_acute <- NA
  }else{
    con_tab_late_acute <- table(question_late_acute, qwb_late_acute$phase)
    fishy_late_acute <- fisher.test(con_tab_late_acute, simulate.p.value = TRUE)
    p_late_acute <- fishy_late_acute$p.value
    sig_95_late_acute <- fishy_late_acute$p.value < 0.05/70
    sig_99_late_acute <- fishy_late_acute$p.value < 0.01/70
  }
  
  tibble(question = x,  p_early_acute = p_early_acute, sig_95_early_acute = sig_95_early_acute, sig_99_early_acute = sig_99_early_acute,
         p_late_acute = p_late_acute, sig_95_late_acute = sig_95_late_acute, sig_99_late_acute = sig_99_late_acute)
}) 

# Re-join to qwb_fisher_pre 
qwb_fisher_full <- qwb_fisher_pre %>% 
  left_join(qwb_fisher, by = "question") %>%
  arrange(group, p_early_acute) %>% 
  mutate(`early_acute (n = 69)` = str_c(`early_acute (n = 69)`, ifelse(sig_95_early_acute, ifelse(sig_99_early_acute, "**", "*"), "")),
         `late_acute (n = 67)` = str_c(`late_acute (n = 67)`, ifelse(sig_95_late_acute, ifelse(sig_99_late_acute, "**", "*"), "")),
         p_early_acute = round(p_early_acute, 6),
         p_late_acute = round(p_late_acute, 6)) 

# Full comparison table 
qwb_responses_full_table <- qwb_deng %>% 
  gather(question, present, -score, -case,  -phase, -day_illness, -health_over_scale) %>% 
  mutate(present = as.logical(present),
         group = case_when(question %in% physical_qs ~ "physical",
                           question %in% pyschological_qs ~ "psychological",
                           question %in% scoial_qs ~ "social")) %>% 
  group_by(question, group, phase) %>% 
  summarise(n = n(), 
            num = sum(present,  na.rm = TRUE),
            pc =100*round((sum(present,  na.rm = TRUE)/n()), digits = 2)) %>%
  mutate(phase = str_c(phase, " (n = ", n, ")")) %>% 
  group_by(question, group) %>%
  select(-n) %>% 
  unite(col = temp, num, pc, sep = "__") %>% 
  spread(phase, temp) %>% 
  left_join(qwb_fisher, by = "question") %>%
  mutate(early_acute = str_c(str_replace(`early_acute (n = 69)`, "__", " ("), ")", ifelse(is.na(p_early_acute), "", ifelse(sig_95_early_acute, ifelse(sig_99_early_acute, "**", "*"), ""))),
         late_acute = str_c(str_replace(`late_acute (n = 67)`, "__", " ("), ")", ifelse(is.na(p_late_acute), "", ifelse(sig_95_late_acute, ifelse(sig_99_late_acute, "**", "*"), ""))),
         convalescent = str_c(str_replace(`convalescent (n = 59)`, "__", " ("), ")"),
         p_early_acute = round(p_early_acute, digits = 15),
         p_late_acute = round(p_late_acute, digits = 15)) %>% 
  select(question, group,   early_acute, late_acute, convalescent,  p_early_acute_v_conv = p_early_acute,  p_late_acute_v_conv = p_late_acute) %>% 
  arrange(group, as.numeric(str_replace_all(p_early_acute_v_conv, "\\*", ""))) 

