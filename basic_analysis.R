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

#Ensure rds file path set correctly
qwb_deng <- read_rds("qwb_deng.rds")

#Determine which participants completed a form in each illness phase
all <- qwb_deng %>% 
  group_by(case_name, phase) %>% 
  count() %>% 
  filter(n == 1) %>% 
  group_by(case_name) %>% 
  summarise(all = sum(n, na.rm = TRUE)) %>% 
  filter(all == 3) %>% pull(case_name)

#Filter original data frame to include only those that have form in each illness phase
qwb_pair <- qwb_deng %>% 
  filter(case_name %in% all)

#--------------------------------------------------------------------------
# Table 1. Participant characteristics ------------------------------------
#--------------------------------------------------------------------------

tab1_fun <- function(capture_mod_type) {
  qwb_deng %>% #comment for paired analysis
  # qwb_pair %>% #uncomment for paired analysis
    mutate(day_of_capture = first_blood-first_symptom,
           capture_mod = ifelse(index_contact == "contact", index_contact, capture)) %>% 
    group_by(case_name, age, sex, index_contact, capture_mod, hospitalized, diagnosis, day_of_capture) %>% 
    count() %>% 
    ungroup() %>% 
    filter(str_detect(capture_mod, capture_mod_type)) %>% 
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
    select(Characteristic, everything())
}
tab1 <- map(c(".","clinic", "community", "contact"), tab1_fun)
names(tab1) <- c("all","clinic", "community", "contact")
do.call(bind_cols, tab1) %>% 
  select(Characteristic = Characteristic...1, Total = Value...2,  Clinic = Value...4, 
         Community = Value...6, Cluster  = Value...8) 
  


#Age comparisons
qwb_age_sex <- qwb_deng %>%
  mutate(day_of_capture = as.numeric(first_blood-first_symptom)) %>% 
  select(case_name, age, sex, capture, day_of_capture) %>% 
  distinct()

fisher.test(qwb_age_sex$sex[qwb_age_sex$capture!="cluster"],  qwb_age_sex$capture[qwb_age_sex$capture!="cluster"])
fisher.test(qwb_age_sex$sex[qwb_age_sex$capture!="community"],  qwb_age_sex$capture[qwb_age_sex$capture!="community"])
fisher.test(qwb_age_sex$sex[qwb_age_sex$capture!="clinic"],  qwb_age_sex$capture[qwb_age_sex$capture!="clinic"])


#Sex comparisons
wilcox.test(age ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="cluster",])
wilcox.test(age ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="community",])
wilcox.test(age ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="clinic",])

#Day capture comparisons
wilcox.test(day_of_capture ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="cluster",])
wilcox.test(day_of_capture ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="community",])
wilcox.test(day_of_capture ~ capture, data = qwb_age_sex[qwb_age_sex$capture!="clinic",])



#--------------------------------------------------------------------------
# Figure 1 Comparison of proportions & heatmap-----------------------------
#--------------------------------------------------------------------------
#Define physical, psycological and social variables

chronic_physical <- {c("Blindness (bilateral)", "Blindness (unilateral)", "Speech problem", 
                             "Paralysis of limbs", "Paralysis of digits", "Deformity", "Fatigue", 
                             "Weight change", "Over or under weight","Chewing problem", 
                             "Deafness", "Skin condition", "Itching","Use of health appliances")}
acute_physical <- {c("Visual problem", "Eye pain", "Headache", "Dizziness", "Ear symptoms", 
                     "Nose symptoms", "Sore throat", "Toothache", "Oral bleeding", 
                     "Cough", "Shortness of breath", "Chest pain", "Abdominal pain", 
                     "Change bowels", "Dysuria", "Reduced bladder control", 
                     "Genital symptoms", "Broken bones", "Back or neck pain", "Hip pain", 
                     "Joint pain", "Edema", "Fever or chills", "Loss of conciousness", 
                     "Balance problem")}
psychological <- {c("Insomnia", "Nervousness", "Feeling upset", 
                                 "Anxiety", "No control of life", "Loneliness", "Frustration", 
                                 "Hungover^", "Reduced libido", "Confusion", "Invasive thoughts",
                                  "Took medication", "Anorexia","On medical diet")}
self_care <- {c("Hospitalized", "Help with personal care")}
mobility_physical_function <- {c("Drove vehicle", "Used public transport", 
                                 "Transport not used", "Difficulty with stairs", "Avoid walking", 
               "Limp or walking aids", "Difficulty bending", "Difficulty carrying", 
               "Other physical limitation", "Bedbound", "Used wheelchair", "Wheelchair controlled by other")}
usual_social_activity <- {c("Affect school/work", "Affect personal life", 
                            "Change plans")}



# physical_qs <- {c("Blindness (bilateral)", "Blindness (unilateral)", "Speech problem", 
#                   "Paralysis of limbs", "Paralysis of digits", "Deformity", "Fatigue", 
#                   "Weight change", "Over or under weight", "Chewing problem", 
#                   "Deafness", "Skin condition", "Itching", "Visual problem", "Eye pain", "Headache", "Dizziness", "Ear symptoms", 
#                   "Nose symptoms", "Sore throat", "Toothache", "Oral bleeding", 
#                   "Cough", "Shortness of breath", "Chest pain", "Abdominal pain", 
#                   "Change bowels", "Dysuria", "Reduced bladder control", 
#                   "Genital symptoms", "Broken bones", "Back or neck pain", "Hip pain", 
#                   "Joint pain", "Edema", "Fever or chills", "Loss of conciousness", 
#                   "Balance problem", "Took medication", "Anorexia")}
# pyschological_qs <- {c("Insomnia", "Nervousness", "Feeling upset", 
#                        "Anxiety", "No control of life", "Loneliness", "Frustration", 
#                        "Hungover^", "Reduced libido", "Confusion", "Invasive thoughts")}
# scoial_qs <-   {c("Use of health appliances", "On medical diet", "Hospitalized", 
#                   "Help with personal care", "Drove vehicle", "Used public transport", 
#                   "Transport not used", "Difficulty with stairs", "Avoid walking", 
#                   "Limp or walking aids", "Difficulty bending", "Difficulty carrying", 
#                   "Other physical limitation", "Bedbound", "Used wheelchair", "Wheelchair controlled by other", 
#                   "Affect school/work", "Affect personal life", 
#                   "Change plans")}
case <- {c("case_id", "case_name", "age", "age_group", "sex", "diagnosis","capture","capture_mod", "hospitalized", "cluster_code", 
           "index_contact", "first_blood", "first_symptom", "first_fever")}

# Prepare into list of separte data frames by capture ---------------------
pre_fisher_df <- qwb_deng %>%
  mutate(capture_mod = ifelse(index_contact == "contact", index_contact, capture)) %>% 
  select(-score) %>% 
  gather(question, present, -case,  -phase, -day_illness, -health_over_scale) %>% 
  # mutate(present = as.factor(as.logical(present)),
  #        group = case_when(question %in% physical_qs ~ "physical",
  #                          question %in% pyschological_qs ~ "psychological",
  #                          question %in% scoial_qs ~ "social"))
mutate(present = as.factor(as.logical(present)),
       group = case_when(question %in% chronic_physical|question %in% acute_physical ~ "Physical",
                         question %in% psychological ~ "Psychological",
                         question %in% self_care ~ "Self-care",
                         question %in% mobility_physical_function ~ "Mobility",
                         question %in% usual_social_activity ~ "Social-activity"),
       group = factor(group, levels = c("Physical",  "Psychological",
                                        "Self-care", "Mobility", "Social-activity"))) %>% 
  filter(question != "Hungover^")


all_df <- split(pre_fisher_df, pre_fisher_df$capture_mod)
all_df_names_temp <- names(all_df)
all_df[[4]] <- pre_fisher_df
names(all_df) <- c(all_df_names_temp,"all")
names(all_df)


# Functions to be used  ---------------------------------------------------
prop_fun <- function(x, phase_num) {mean(as.logical(x$present[x$phase==phase_num]))}
sum_fun <- function(x, phase_num) {sum(as.logical(x$present[x$phase==phase_num]))}
fisher_fun <- function(x, phase_drop){
  sub <- x[x$phase != phase_drop,]
  tab <- table(sub$present, sub$phase)
  # print(tab)
  fish <- fisher.test(tab, simulate.p.value = TRUE)
  fish
}

#Round to 
round_to <- 2

# Create summary df -------------------------------------------------------
all_df_fisher <- map2(all_df, names(all_df), function(x, y){
  x %>% 
    group_by(question, group) %>% 
    nest() %>% 
    mutate(capture  = y,
           prop_ea = round(map_dbl(data, prop_fun, phase_num = "early_acute"), digits = round_to),
           prop_la = round(map_dbl(data, prop_fun, phase_num = "late_acute"), digits = round_to),
           prop_c = round(map_dbl(data, prop_fun, phase_num = "convalescent"), digits = round_to),
           sum_ea = round(map_dbl(data, sum_fun, phase_num = "early_acute"), digits = round_to),
           sum_la = round(map_dbl(data, sum_fun, phase_num = "late_acute"), digits = round_to),
           sum_c = round(map_dbl(data, sum_fun, phase_num = "convalescent"), digits = round_to),
           fish_ea_c =  map(data, fisher_fun, phase_drop  = "late_acute"),
           fish_la_c =  map(data, fisher_fun, phase_drop  = "early_acute"),
           fish_ea_la =  map(data, fisher_fun, phase_drop  = "convalescent"),
           ea_c_p = map_dbl(fish_ea_c, "p.value"),
           la_c_p = map_dbl(fish_la_c, "p.value"),
           ea_la_p = map_dbl(fish_ea_la, "p.value"),
           ea_c_sig = case_when(ea_c_p<(0.01/70) ~ "**",
                                ea_c_p<(0.05/70) ~ "*",
                                TRUE ~ ""),
           la_c_sig = case_when(la_c_p<(0.01/70) ~ "**",
                                la_c_p<(0.05/70) ~ "*",
                                TRUE ~ ""),
           ea_la_sig = case_when(ea_la_p<(0.01/70) ~ "**",
                                 ea_la_p<(0.05/70) ~ "*",
                                 TRUE ~ ""),
           early_acute  = str_c(prop_ea, "__", ea_c_sig),
           late_acute  = str_c(prop_la, "__", la_c_sig),
           convalescent  = str_c(prop_c, "__")) 
})

#Select questions to plot
sig_qs <- all_df_fisher$all$question[str_detect(all_df_fisher$all$ea_c_sig, "\\*")]

#X-Axis labels
x_axis_lab_n <- all_df %>% 
  map(~{.x %>% select(phase, case_name) %>% 
      distinct() %>% 
      mutate(phase = factor(phase, levels = c("early_acute", "late_acute", "convalescent"))) %>% 
      group_by(phase) %>% 
      count() %>% 
      pull(n)})

#Arguments to iterate over in pmap 
args_list <- list(all_df_fisher, names(all_df_fisher), x_axis_lab_n, c(2:4, 1))

heatmap_plots <- pmap(args_list, ~{
  ea_lab <- str_c("early_acute", " (n=", ..3[1], ")")
  la_lab <- str_c("late_acute", " (n=", ..3[2], ")")
  c_lab <- str_c("convalescent", " (n=", ..3[3], ")")
  ..1 %>% 
    # filter(question %in% sig_qs | str_detect(early_acute, "\\*")) %>%
    pivot_longer(cols = c("early_acute", "late_acute", "convalescent"), names_to = "phase", values_to = "value") %>% 
    separate(col = value, into = c("prop", "sig"), sep =  "__", remove = FALSE) %>% 
    mutate(prop = as.numeric(prop),
           phase = factor(phase, levels = c("early_acute", "late_acute", "convalescent"), labels  = c("early_acute", "late_acute", "convalescent")),
           prop_lab  =  case_when(prop==0 ~ "0.00",
                                  prop==1 ~ "1.00",
                                  nchar(as.character(prop))==3  ~ str_c(prop, 0),
                                  TRUE ~ as.character(prop)),
           labels  =  str_c(prop_lab, sig)) %>% 
    group_by(question) %>% 
    arrange(group, question) %>% 
    mutate(order_var = first(prop),
           phase = case_when(phase == "early_acute" ~ ea_lab,
                             phase == "late_acute" ~ la_lab,
                             phase == "convalescent" ~ c_lab)) %>% #enables nice ordering of heatmap rows
    mutate(phase = factor(phase, levels = c(ea_lab, la_lab, c_lab))) %>% 
    ungroup() %>%  
    ggplot(aes(x = phase, y = fct_reorder(question, order_var), fill = prop)) +
    geom_tile() +
    geom_text(aes(label = labels), hjust=0, nudge_x = -0.1, size = 2.2) +
    geom_text(aes(label = labels),hjust=0, nudge_x = -0.1, size = 2.2) +
    geom_text(aes(label = labels), hjust=0, nudge_x = -0.1, size  = 2.2) +
    facet_rep_wrap(~group, ncol = 1, scales='free_y', repeat.tick.labels = 'left') +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_bw() +
    labs(title = str_c("Figure S", ..4, ": ", ..2)) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Proportion") +
    theme(
      # plot.title = element_blank(),
      plot.title =  element_text(hjust = 0.5, size = 16),
          axis.title = element_blank(),
          axis.text.y = element_text(size = 6),
          axis.text.x = element_text(vjust = -5, size = 6),
          legend.title = element_text(size = 8),
          axis.ticks = element_blank(),
          strip.text = element_text(colour="black", size = 7, face=2),
          strip.background = element_rect(fill= "white"),
          strip.text.x = element_text(margin = margin(0.2,0,0.2,0, "cm")),
          legend.key.size = unit(15, units = "pt"),
          legend.text = element_text(size = 6),
          plot.margin = unit(c(1,1,1,1), "cm"))
})


plot_heatmap_fun <- function(plot, name) {
  # Access gtab to alter row heights to match
  g <- ggplot_build(plot) 
  ## find out how many y-breaks are in each panel
  ## to infer the number of tiles
  vtiles <- map_dbl(g$layout$panel_params, ~ length(.x$y$breaks))
  gt <- ggplot_gtable(g)
  ## find out which items in the layout correspond to the panels
  ## we refer to the "t" (top) index of the layout
  panels <- gt$layout$t[grepl("panel", gt$layout$name)]
  ## replace the default panel heights (1null) with relative sizes 
  ## null units scale relative to each other, so we scale with the number of tiles
  gt$heights[panels[1]] <-unit(vtiles[1], "null")
  gt$heights[panels[2]] <-unit(vtiles[2], "null")
  gt$heights[panels[3]] <-unit(vtiles[3], "null")
  gt$heights[panels[4]] <-unit(vtiles[4], "null")
  gt$heights[panels[5]] <-unit(vtiles[5], "null")
  # gt$heights[22] <- unit(0.1, "cm")
  ## draw on a clean slate
  library(grid)
  grid.newpage()
  grid.draw(gt) 
  
  # tiff(str_c("~/Dropbox/R_projects/professional_r/p01_r/project2_r/qwb/r/revision2/Figures/Fig1_", name, ".tiff"),units="in", width=6, height=4.3, res=300)
  pdf(str_c("~/Dropbox/R_projects/professional_r/p01_r/project2_r/qwb/r/revision2/Figures/Fig1_", name, ".pdf"), width=8, height=12)
  grid.draw(gt)
  dev.off()
} 

walk2(heatmap_plots, names(all_df_fisher), plot_heatmap_fun)


#--------------------------------------------------------------------------
# Table 2. HRQoL Scores by illness phase ----------------------------------
#--------------------------------------------------------------------------

#Define participants who can be included in the paired analysis (a form in each illness phase)
#Determine which participants completed a form in each illness phase

tab2_fun <- function(capture_mod_type) {
  # qwb_deng %>% #comment for paired analysis
  qwb_pair %>% #uncomment for paired analysis
    mutate(phase = factor(phase, levels = c("early_acute",  "late_acute", "convalescent")),
           capture_mod = ifelse(index_contact == "contact", index_contact, capture)) %>% 
    filter(str_detect(capture_mod, capture_mod_type)) %>% 
    # filter(case_name %in% qw_pair) %>% #uncomment for paired analysis
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
    ungroup() %>% 
    mutate(phase = factor(phase, levels = c("early_acute", "late_acute", "convalescent"), ordered = TRUE)) %>% 
    arrange(phase) %>% 
    mutate(phase = as.character(phase), recruitment  = ifelse(capture_mod_type==".", "All", capture_mod_type)) %>% 
    select(recruitment, phase, forms, participants, `median score (IQR)`, range)
}

all_summary_tab2 <- map(c(".","clinic", "community", "contact"), tab2_fun)
names(all_summary_tab2) <- c("all","clinic", "community", "contact")
do.call(bind_rows, all_summary_tab2) 
  




#--------------------------------------------------------------------------
# Fig 2. HRQoL score by illness phase -------------------------------------
#--------------------------------------------------------------------------

fig_2A <- qwb_deng %>%
  mutate(phase = str_replace(phase, "_", "-"),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalescent")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = phase, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  labs(
    # title = "Figure 1 all",
    y = "HRQoL score",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), 
        legend.position="top",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("early-acute", "late-acute"), 
                                        c("late-acute", "convalescent"),
                                        c("early-acute", "convalescent")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns"))) 


fig_2B <- qwb_pair %>%
  mutate(phase = str_replace(phase, "_", "-"),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalescent")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = phase, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  labs(
    # title = "Figure 2 Paired",
    y = "",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), 
        legend.position="top",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("early-acute", "late-acute"), 
                                        c("late-acute", "convalescent"),
                                        c("early-acute", "convalescent")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns")),
                     paired = TRUE) 

fig_2C <- qwb_deng %>%
  # fig_2C <- qwb_pair %>%  
  mutate(phase = str_replace(phase, "_", "-"),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalescent")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = capture, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  facet_wrap(~ phase) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  # geom_label(data = qwb_score_phase, aes(x = phase, y = 0.1, label = str_c("n=",n)), size = 8) +
  labs(
    # title = "Figure 3 ",
    y = "HRQoL score",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        legend.title = element_blank(), 
        legend.position="none",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("cluster", "community"), 
                                        c("community", "clinic"),
                                        c("cluster", "clinic")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns"))) 


fig_2D <- qwb_deng %>%
  # fig_2D <- qwb_pair %>%  
  mutate(phase = str_replace(phase, "_", "-"),
         phase = ifelse(phase == "convalescent", "convalesc", phase),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalesc"),
                        labels = c("early-ac",  "late-ac", "convale")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = phase, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  facet_wrap(~ capture) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  # geom_label(data = qwb_score_phase, aes(x = phase, y = 0.1, label = str_c("n=",n)), size = 8) +
  labs(
    # title = "FIGURE 1. HRQoL score by illness phase",
    # y = "HRQoL score",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        # legend.title = element_blank(), 
        legend.position="none",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("early-ac", "late-ac"), 
                                        c("early-ac", "convale"),
                                        c("late-ac", "convale")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns"))) 



fig_2E <- qwb_pair %>%  
  mutate(phase = str_replace(phase, "_", "-"),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalescent")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = capture, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  facet_wrap(~ phase) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  # geom_label(data = qwb_score_phase, aes(x = phase, y = 0.1, label = str_c("n=",n)), size = 8) +
  labs(
    # title = "Figure 1",
    y = "HRQoL score",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        # legend.title = element_blank(), 
        legend.position="none",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("cluster", "community"), 
                                        c("community", "clinic"),
                                        c("cluster", "clinic")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns"))) 


fig_2F <- qwb_pair %>%  
  mutate(phase = str_replace(phase, "_", "-"),
         phase = ifelse(phase == "convalescent", "convalesc", phase),
         phase = factor(phase, levels = c("early-acute",  "late-acute", "convalesc"),
                        labels = c("early-ac",  "late-ac", "convale")),
         capture = factor(capture, levels = c("clinic","community", "cluster"))) %>% 
  filter(!is.na(score)) %>% 
  ggplot(aes(x = phase, y = score)) +
  geom_boxplot(outlier.alpha = 0, width = 0.55, lwd = 0.3) +
  geom_jitter(aes(color = capture), width = 0.1, alpha = 0.7, size = 0.8) +
  facet_wrap(~ capture) +
  scale_fill_manual(values = c("red", "blue", "green")) +
  # geom_label(data = qwb_score_phase, aes(x = phase, y = 0.1, label = str_c("n=",n)), size = 8) +
  labs(
    # title = "Figure 2",
    y = "",
    x = "") +
  theme_bw() +
  # scale_x_discrete(expand=c(1,1)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.5,0.75, 1), limits = c(0, 1.35), expand = c(0, 0)) +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6),
        plot.margin = unit(c(0.1,1,1,1), "cm"),
        axis.title.y = element_text(size = 10, vjust = 4),
        axis.title.x = element_text(size = 10, hjust = 1, angle = 45),
        axis.ticks.x = element_blank(),
        strip.text = element_text(size = 6, face = "bold"),
        legend.text = element_text(size = 10),
        # legend.title = element_blank(), 
        legend.position="none",
        strip.background = element_rect(fill = "#f0f0f0")) +
  # coord_capped_cart(left='top') +
  stat_compare_means(comparisons = list(c("early-ac", "late-ac"), 
                                        c("early-ac", "convale"),
                                        c("late-ac", "convale")),
                     label.y = c(1.05, 1.15, 1.25),
                     tip.length = 0.01,
                     size = 5, label = str_c("p.signif"),
                     symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("**", "**", "**", "*", "ns")),
                     paired = TRUE) 



library("cowplot")
fig2_supplement <- plot_grid(fig_2A, fig_2B, fig_2C, fig_2E, fig_2D, fig_2F,
                  labels = c("A", "B", "C", "D", "E", "F"),
                  ncol = 2, nrow = 3)

fig2 <- plot_grid(fig_2A, fig_2B, fig_2C,  fig_2D, 
                  labels = c("A", "B", "C", "D"),
                  ncol = 2, nrow = 2)


