# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Panel Balance

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script compares regression estimates of the coefficient of 
# interest in the unbalanced and balanced panels.

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Analysis --------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df <- read_csv('Results/cleaned_filtered_data.csv')

# Make panel data frames
# df_storm <- pdata.frame(main_df, index = c("FIPS", "year"))
df_storm <- main_df
b_df_storm <- make.pbalanced(main_df, index = c("FIPS", "year"), 
                             balance.type = "shared.individuals")

# Write a function to make the standard errors
do_the_SE <- function(model){
  
  # Cluster the SEs
  se <- data.frame(summary(model, cluster = "FIPS")$coefficients)$`Std..Error`
  
  # Fix the position problem
  temp <- which(summary(model)[["aliased"]])
  
  for (i in 1:length(temp)){se <- append(se, 1, after=temp[i]-1)}
  
  return(se)
  
}


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### First Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_1 <- function (my_df, table_description, table_output){
  
  mod1 <- lm(log(density) ~ treatment*post + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(output) ~ treatment*post + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(dependence ~ treatment*post + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3)
  my_SE <- lapply(my_models, do_the_SE)
  my_outcome <- c("Log Density", "Log Output", "Dependence")
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 1", length(my_models))
  panel_type <- rep(table_description, length(my_models))
  outcome <- my_outcome
  
  main_coefs <- c(
    mod1$coefficients["treatment:post"],
    mod2$coefficients["treatment:post"],
    mod3$coefficients["treatment:post"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:post")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod1a_coefs <- regressions_1(df_storm, "Unbalanced", "reg1_unbalanced.tex")
mod1b_coefs <- regressions_1(b_df_storm, "Balanced", "reg1_balanced.tex")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Second Set of Regressions --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_2 <- function (my_df, table_description, table_output){
  
  mod1 <- lm(log(density) ~ treatment:post:log(fixedcont) + treatment*post + 
               log(fixedcont) + as.factor(year) + as.factor(FIPS),
             data = my_df[is.finite(log(my_df$fixedcont)),])
  
  mod2 <- lm(log(output) ~ treatment:post:log(fixedcont) + treatment*post + 
               log(fixedcont) + as.factor(year) + as.factor(FIPS),
             data = my_df[is.finite(log(my_df$fixedcont)),])
  
  mod3 <- lm(dependence ~ treatment:post:log(fixedcont) + treatment*post + 
               log(fixedcont) + as.factor(year) + as.factor(FIPS),
             data = my_df[is.finite(log(my_df$fixedcont)),])
  
  my_models = list(mod1, mod2, mod3)
  my_SE <- lapply(my_models, do_the_SE)
  my_outcome <- c("Log Density", "Log Output", "Dependence")
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 2", length(my_models))
  panel_type <- rep(table_description, length(my_models))
  outcome <- my_outcome
  
  main_coefs <- c(
    mod1$coefficients["treatment:post:log(fixedcont)"],
    mod2$coefficients["treatment:post:log(fixedcont)"],
    mod3$coefficients["treatment:post:log(fixedcont)"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:post:log(fixedcont)")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod2a_coefs <- regressions_2(df_storm, "Unbalanced", "reg2_unbalanced.tex")
mod2b_coefs <-regressions_2(b_df_storm, "Balanced", "reg2_balanced.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Third Set of Regressions ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

regressions_3 <- function (my_df, table_description){
  
  mod1 <- lm(log(density) ~  treatment:post:tpost + treatment*post +
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod2 <- lm(log(output) ~  treatment:post:tpost + treatment*post +
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  mod3 <- lm(dependence ~  treatment:post:tpost + treatment*post +
               tpost + as.factor(year) + as.factor(FIPS),
             data = my_df)
  
  my_models = list(mod1, mod2, mod3)
  my_SE <- lapply(my_models, do_the_SE)
  my_outcome <- c("Log Density", "Log Output", "Dependence")
  
  # Main Coefficient Reporting dataframe
  model_num <- rep("Model 3", length(my_models))
  panel_type <- rep(table_description, length(my_models))
  outcome <- my_outcome
  
  main_coefs <- c(
    mod1$coefficients["treatment:post:tpost"],
    mod2$coefficients["treatment:post:tpost"],
    mod3$coefficients["treatment:post:tpost"]
  )
  
  term_names <- data.frame(summary(mod1)$aliased)
  term_names <- rownames(term_names)
  main_se <- lapply(my_SE, function (x) data.frame(term_names, x))
  main_se <- bind_rows(main_se)
  main_se <- main_se %>% 
    filter(term_names == "treatment:post:tpost")
  main_se <- main_se$x
  
  out_df <- data.frame(model_num, panel_type, outcome, main_coefs, main_se)
  colnames(out_df) <- c("model","panel","outcome","coefs","se")
  out_df <- out_df %>% 
    mutate(
      lower_CI = coefs - 1.96*se,
      upper_CI = coefs + 1.96*se
    )
  
  return(out_df)
  
}

mod3a_coefs <- regressions_3(df_storm, "Unbalanced")
mod3b_coefs <- regressions_3(b_df_storm, "Balanced")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Unbalanced vs. Balanced Panel ----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

all_mods <- bind_rows(list(mod1a_coefs, mod1b_coefs, 
                           mod2a_coefs, mod2b_coefs, 
                           mod3a_coefs, mod3b_coefs))

all_mods$outcome_f <- factor(all_mods$outcome, 
                             levels = c("Log Density", "Log Output", "Dependence"))

png("Results/Robustness Checks/Panel Balance/Model1.png", 
    width=6, height=3, units="in", res=600)
v <- ggplot(all_mods %>% filter(model == "Model 1"), x=panel, y=coefs) +
  facet_wrap(vars(outcome_f), scales="free", nrow = 1) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
print(v)
dev.off()


png("Results/Robustness Checks/Panel Balance/Model2.png", 
    width=6, height=3, units="in", res=600)
v <- ggplot(all_mods %>% filter(model == "Model 2"), x=panel, y=coefs) +
  facet_wrap(vars(outcome_f), scales="free", nrow = 1) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
print(v)
dev.off()


png("Results/Robustness Checks/Panel Balance/Model3.png", 
    width=6, height=3, units="in", res=600)
v <- ggplot(all_mods %>% filter(model == "Model 3"), x=panel, y=coefs) +
  facet_wrap(~outcome_f, scales="free", nrow = 1) +
  geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
                color = natparks.pals("Yellowstone", 1), 
                size=0.5, width=0.5) +
  geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
             size=2) +
  geom_hline(color="black", yintercept = 0) +
  labs(x=NULL, y=NULL) +
  theme_bw()
print(v)
dev.off()

# temp <- all_mods %>% 
#   filter(grepl("Model 30", model)) %>% 
#   mutate(
#     Cutoff = paste(as.numeric(gsub("Model 30", "", model)) * 100, "%", sep="")
#   )
# 
# png("Results/Robustness Checks/Panel Balance/Model3.png", 
#     width=8, height=4, units="in", res=600)
# ggplot(temp, x=panel, y=coefs) +
#   facet_wrap(vars(outcome), scales="free", nrow = 3) +
#   geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI, color=Cutoff),
#                 size=0.5, width=0.5, position=position_dodge(width = 1)) +
#   geom_point(aes(x=panel, y=coefs, color=Cutoff), size=2, position=position_dodge(width = 1)) +
#   geom_hline(color="black", yintercept = 0) +
#   scale_color_manual(values = natparks.pals("Yellowstone", 3)) + 
#   labs(x=NULL, y=NULL) +
#   theme_bw()
# dev.off()
# 
# png("Results/Robustness Checks/Panel Balance/Model4.png", 
#     width=6, height=4, units="in", res=600)
# ggplot(all_mods %>% filter(model == "Model 4"), x=panel, y=coefs) +
#   facet_wrap(vars(outcome), scales="free", nrow = 3) +
#   geom_errorbar(aes(x=panel, ymin = lower_CI, ymax = upper_CI),
#                 color = natparks.pals("Yellowstone", 1), 
#                 size=0.5, width=0.5) +
#   geom_point(aes(x=panel, y=coefs), color = natparks.pals("Yellowstone", 1), 
#              size=2) +
#   geom_hline(color="black", yintercept = 0) +
#   labs(x=NULL, y=NULL) +
#   theme_bw()
# dev.off()
# 
# 
# rm(all_mods, mod1a_coefs, mod1b_coefs, mod2a_coefs, mod2b_coefs, 
#    mod3a_coefs, mod3b_coefs, mod3c_coefs, mod3d_coefs, mod3e_coefs, mod3f_coefs,
#    mod4a_coefs, mod4b_coefs, temp
# )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::