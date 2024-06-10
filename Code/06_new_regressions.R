# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: New Regressions

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2024-05-27

# Purpose: This script runs the new regressions.


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## New (Finalized) Regressions
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Clear environment
rm(list = ls())

#Load packages
# library(pacman)
# p_load(tidyverse, sandwich, lmtest, fastDummies, stargazer, margins, readr,
#        NatParksPalettes, ggpubr, install = F)

# # Write a function to make the standard errors
# do_the_SE <- function(model){
#   # Cluster the SEs
#   se <- data.frame(summary(model, cluster = "FIPS")$coefficients)$`Std..Error`
#   
#   # Fix the position problem
#   temp <- which(summary(model)[["aliased"]])
#   
#   for (i in 1:length(temp)){se <- append(se, 1, after=temp[i]-1)}
#   return(se)
# }
# 
# 

# Read in data
#df <- read_csv('Results/cleaned_filtered_data.csv')

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#Update yvars for final outputs
yvars <- c("output", "density")

# # Run regressions for each of the outcome variables in "yvars"
# firstModels <- lapply(yvars, function(x){
#   lm(formula =
#        paste0("log(`", x, "`) ~ treatment*post + as.factor(year) + as.factor(FIPS)"),
#      data = df)
# })
# 
# firstModels[[length(firstModels)+1]] <- 
#   lm(dependence ~ treatment*post + as.factor(year) + as.factor(FIPS),
#      data = df)
# 
# names(firstModels) <- c(yvars, "dependence") #label ea. regression by yvar
# 
# 
# 
# # Store clustered SEs
# firstSE <- lapply(firstModels, do_the_SE)
# 
# #Save regression table
# stargazer(firstModels,
#           title = "Preliminary Difference-in-Differences",
#           omit = c("factor","Constant"),
#           se = firstSE,
#           type = "latex",
#           order = c(
#             "treatment:post",
#             "treatment",
#             "post"
#           ),
#           font.size = 'small',
#           covariate.labels = c(
#             "Disaster * Post Storm",
#             "Disaster",
#             "Post Storm"
#           ),
#           dep.var.labels.include = F,
#           column.labels = c(yvars, "dependence"),
#           omit.stat = c("rsq", "adj.rsq"), 
#           out = "Results/Regression Tables/blj_reg1.tex")
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# # Run regressions for each of the outcome variables in "yvars"
# secondModels <- lapply(yvars, function(x){
#   lm(formula =
#        paste0("log(`", x, "`) ~ treatment:post:log(fixedcont) + treatment*post +
#               log(fixedcont) + as.factor(year) + as.factor(FIPS)"),
#      data = df[is.finite(log(df$fixedcont)),])
# })
# 
# secondModels[[length(secondModels)+1]] <- lm(
#   dependence ~ treatment:post:log(fixedcont) + treatment*post + log(fixedcont) + 
#     as.factor(year) + as.factor(FIPS),
#   data = df[is.finite(log(df$fixedcont)),])
# 
# names(secondModels) <- c(yvars, "dependence") #label ea. regression by yvar
# 
# # Store clustered SEs
# secondSE <- lapply(secondModels, do_the_SE)
# 
# #Save regression table
# stargazer(secondModels,
#           title = "Heterogeneous Effects Through Pre-Disaster Contributions",
#           omit = c("factor","Constant"),
#           se = secondSE,
#           type = "latex",
#           order = c(
#             "treatment:post:log(fixedcont)",
#             "treatment:post",
#             "treatment",
#             "post",
#             "log(fixedcont)"
#           ),
#           font.size = 'small',
#           covariate.labels = c(
#             "Disaster * Post Storm * Log 2007 Cont.",
#             "Disaster * Post Storm",
#             "Disaster",
#             "Post Storm",
#             "Log 2007 Cont."
#           ),
#           dep.var.labels.include = F,
#           column.labels = c(yvars, "dependence"),
#           omit.stat = c("rsq", "adj.rsq"), 
#           out = "Results/Regression Tables/blj_reg2.tex")
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# # Run regressions for each of the outcome variables in "yvars"
# thirdModels <- lapply(yvars, function(x){
#   lm(formula =
#        paste0("log(`", x, "`) ~ treatment:post:tpost + treatment*post + 
#                tpost + as.factor(year) + as.factor(FIPS)"),
#      data = df)
# })
# 
# thirdModels[[length(thirdModels)+1]] <- 
#   lm(dependence ~ treatment:post:tpost + treatment*post + tpost +
#        as.factor(year) + as.factor(FIPS), data = df)
# 
# names(thirdModels) <- c(yvars, "dependence") #label ea. regression by yvar
# 
# # Store clustered SEs
# thirdSE <- lapply(thirdModels, do_the_SE)
# 
# #Save regression table
# stargazer(thirdModels,
#           title = "Differences-in-Differences with Dynamic Effects",
#           omit = c("factor","Constant"),
#           se = thirdSE,
#           type = "latex",
#           order = c(
#             "treatment:post:tpost",
#             "treatment:post",
#             "treatment",
#             "post",
#             "tpost"
#           ),
#           font.size = 'small',
#           covariate.labels = c(
#             "Disaster * Post Storm * Years After",
#             "Disaster * Post Storm",
#             "Disaster",
#             "Post Storm",
#             "Years After"
#           ),
#           dep.var.labels.include = F,
#           column.labels = c(yvars, "dependence"),
#           omit.stat = c("rsq", "adj.rsq"), 
#           out = "Results/Regression Tables/blj_reg3.tex")
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# 
# 


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Helper Functions -------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Run the regression of choice
my_reg <- function(outcome, rhs, fe, sample_df){
  
  f = as.formula(paste(outcome, '~', rhs, "|", fe))
  reg_results = feols(f, sample_df, vcov = "hc1")
  return(reg_results)
  
}

# Make a proper table
my_table <- function(rhs, fe, sample_df, filename, title) {
  
  outcome_list <- list("log_output","log_density","dependence3")
  all_models <- lapply(outcome_list, function (x) my_reg(x , rhs, fe, sample_df))
  
  unlink(filename)
  etable(all_models,
         vcov = "hc1",
         tex = T,
         title = title,
         file = filename,
         dict = var_labs,
         order = c("Disaster", "Post", "log GC"),
         adjustbox = "max width = \\textwidth, center",
         view = T
      )
  return(all_models)

}

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read in and final data prep --------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

df <- read_csv('Results/cleaned_filtered_data.csv')

# Give some log transforms to the data (a little treat)
center1 <- df %>% filter(is.finite(log(fixedcont)))
center1 <- mean(log(center1$fixedcont))

center2 <- df %>% filter(fixedcont3 > 0) %>% filter(is.finite(log(fixedcont3)))
center2 <- mean(log(center2$fixedcont3))

df <- df %>% 
  mutate(
    log_output = log(output),
    log_density = log(density),
    log_fixedcont = log(fixedcont),
    log_fixedcont_centered = log_fixedcont - center1,
    log_fixedcont2 = log(fixedcont3),
    log_fixedcont2_centered = log_fixedcont2 - center2,
    log_pop = log(pop),
    ATT = post*treatment              #specify ATT for marginal effects plotting
  )
rm(center1, center2)


# And make a little data dictionary
var_labs <- c(
  `as.factor(year)` = "Year",
  `as.factor(FIPS)` = "County",
  treatment = "Disaster",
  post = "Post",
  tpost = "Years Post",
  log_fixedcont = "log GC$_{2007}$",
  log_output = "log Output",
  log_density = "log Density",
  dependence = "Dependence",
  dependence3 = "Dependence",
  log_pop = "log Population",
  # ATT = "average treatment effect on the treated"
  ATT = "Disaster $\\times$ Post",
  log_fixedcont_centered = "log GC$_{2007}$ (Centered)",
  log_fixedcont2_centered = "log GC$_{2007}$ (Centered)"
)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Basic DiD --------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

basicdid <- my_table(
  rhs = "ATT + post + treatment", 
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df,
  filename = "Results/Regression Tables/eap_baseline.tex",
  title = "Baseline TWFE"
  )

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Interaction with GC Scale ----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

intdid <- my_table(
  rhs = "ATT*log_fixedcont2_centered + ATT + treatment + post + log_fixedcont2_centered",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2_centered)),
  filename = "Results/Regression Tables/eap_thru_2007cont.tex",
  title = "Heterogenous Effects Through Contributions"
)

# "treatment*post*log_fixedcont",

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the coefficients --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# model1_coefs <- coefplot(basicdid)[['prms']]
# model1_coefs$outcome <- c("Output", "Density", "Dependence")
# model1_coefs$model <- 1

model2_coefs <- coefplot(intdid)[['prms']] %>% 
  filter(str_detect(estimate_names, "ATT")) 
model2_coefs$outcome <- c("Output", "Density", "Dependence")
model2_coefs$model <- 2

my_coefs <- bind_rows(model2_coefs)
my_coefs <- my_coefs %>% 
  mutate(y = case_when(
           outcome != "Dependence" & estimate_names == "ATT"  ~ (exp(y) - 1)*100,
           outcome != "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ (1.1^(y) -1)*100,
           outcome == "Dependence" & estimate_names == "ATT" ~ y,
           outcome == "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ y * log(1.1)
         ),
         ci_low = case_when(
           outcome != "Dependence" & estimate_names == "ATT" ~ (exp(ci_low) - 1)*100,
           outcome != "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ (1.1^(ci_low) -1)*100,
           outcome == "Dependence" & estimate_names == "ATT" ~ ci_low,
           outcome == "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ ci_low * log(1.1)
         ),
         ci_high = case_when(
           outcome != "Dependence" & estimate_names == "ATT" ~ (exp(ci_high) - 1)*100,
           outcome != "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ (1.1^(ci_high) -1)*100,
           outcome == "Dependence" & estimate_names == "ATT" ~ ci_high,
           outcome == "Dependence" & estimate_names == "ATT:log_fixedcont2_centered" ~ ci_high * log(1.1)
         ))

my_coefs$outcome <- factor(my_coefs$outcome, 
                           levels = c("Output", "Density", "Dependence"))
my_coefs$beta_hat <- paste0(my_coefs$model, my_coefs$estimate_names)


# tikzDevice::tikz("Results/Figures/coef_plot.tex", height = 3.5, width = 6)
# ggplot(my_coefs) +
#   geom_point(aes(x = as.factor(beta_hat), y = y, color = as.factor(beta_hat))) +
#   geom_errorbar(aes(x = as.factor(beta_hat), ymin = ci_low, ymax = ci_high, color = as.factor(beta_hat))) +
#   facet_wrap(~outcome, scales = "free_y",
#              labeller = as_labeller(
#                c(Output = "\\%-Change in Output", 
#                  Density = "\\%-Change in Density", 
#                  Dependence = "\\%-Point Change in Dependence")), 
#              strip.position = "left") +
#   labs(y = NULL) +
#   labs(x = "") +
#   scale_color_discrete(labels = c("Disaster times Post (Model 1)",
#                                   "Disaster times Post (Model 2)",
#                                   "Disaster times Post times Log GC (Model 2)")) +
#   theme(strip.background = element_blank(), strip.placement = "outside", 
#         legend.position = "bottom", axis.title.x = element_blank(), 
#         axis.text.x=element_blank(), axis.ticks.x = element_blank(),
#         legend.title = element_blank()) +
#   guides(color=guide_legend(nrow=2,byrow=TRUE))
# dev.off()


tikzDevice::tikz("Results/Figures/coef_plot.tex", height = 2.5, width = 6)
ggplot(my_coefs) +
  geom_point(aes(y = as.factor(beta_hat), x = y, color = as.factor(beta_hat))) +
  geom_errorbar(aes(y = as.factor(beta_hat), xmin = ci_low, xmax = ci_high, color = as.factor(beta_hat))) +
  facet_wrap(~outcome, scales = "free_x",
             labeller = as_labeller(
               c(Output = "\\%-Change in Output", 
                 Density = "\\%-Change in Density", 
                 Dependence = "\\%-Point Change in Dependence")), 
             strip.position = "top") +
  labs(x = NULL) +
  labs(y = "") +
  scale_color_discrete(labels = c("Disaster times Post",
                                  "Disaster times Post times Log GC")) +
  theme(strip.background = element_blank(), strip.placement = "outside", 
        legend.position = "bottom", axis.title.y = element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y = element_blank(),
        legend.title = element_blank()) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))
dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the marginal effects-----------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#Automatically uses het robust se because that's built into model estimation
#Extract estimates at specific values of log_fixedcont for graphing
slopeest <- list()
for (i in 1:3){
  slopeest[[i]]<- slopes(intdid[[i]], variables = "ATT",
                         newdata = datagrid(log_fixedcont = c(5, 10, 15, 20)))

  if (i<3){
    slopeest[[i]] <- slopeest[[i]] %>%
      mutate(pct = case_when(
        estimate > 0 ~ paste0(round(exp(estimate)*100,0),"%"),
        TRUE ~ paste0("-",round((1-exp(estimate))*100,0),"%")))
  } else {
    slopeest[[i]] <- slopeest[[i]] %>% 
      mutate(pct = round(estimate,0))
  }
}

slp1 <- plot_slopes(intdid[[1]],
            variables = "ATT",
            condition = "log_fixedcont") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in log(Output)") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[1]], mapping = aes(x=log_fixedcont, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[1]][1,'estimate']+0.1, label = slopeest[[1]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[1]][2,'estimate']+0.1, label = slopeest[[1]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[1]][3,'estimate']+0.1, label = slopeest[[1]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[1]][4,'estimate']+0.1, label = slopeest[[1]][4,'pct'])

slp2 <- plot_slopes(intdid[[2]],
                    variables = "ATT",
                    condition = "log_fixedcont") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in log(Density)") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[2]], mapping = aes(x=log_fixedcont, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[2]][1,'estimate']+0.1, label = slopeest[[2]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[2]][2,'estimate']+0.1, label = slopeest[[2]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[2]][3,'estimate']+0.1, label = slopeest[[2]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[2]][4,'estimate']+0.07, label = slopeest[[2]][4,'pct'])

slp3 <- plot_slopes(intdid[[3]],
                    variables = "ATT",
                    condition = "log_fixedcont") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in Resource Dependence") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[3]], mapping = aes(x=log_fixedcont, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[3]][1,'estimate']-2, label = slopeest[[3]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[3]][2,'estimate']-2, label = slopeest[[3]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[3]][3,'estimate']-2, label = slopeest[[3]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[3]][4,'estimate']+2, label = slopeest[[3]][4,'pct'])

slopesplot <- ggarrange(slp1, slp2, slp3, nrow = 3)
annotate_figure(slopesplot, 
                top = text_grob("Average Changes in Nonprofit Indicators Post-Hurricane by Scale of Pre-Disaster GC",
                                face = "bold", size = 14))
