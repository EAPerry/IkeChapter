# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: New Regressions

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2024-05-27

# Purpose: This script runs the new regressions.


# Clear environment
rm(list = ls())


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
  
  # Each table will run the specification entered using log output, log density,
  # and the revised dependence measure as the dependent variable
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

# Make a proper table for various dependence measures
my_table_dependence <- function(rhs, fe, sample_df, filename, title) {
  
  outcome_list <- list("dependence","dependence2","dependence3")
  
  var_labs_alt <- var_labs
  var_labs_alt["dependence"] = "Dependence (Cont / Revenue)"
  var_labs_alt["dependence2"] = "Dependence (Giving / Revenue)"
  var_labs_alt["dependence3"] = "Dependence (Giving / Cont)"
  
  all_models <- lapply(outcome_list, function (x) my_reg(x , rhs, fe, sample_df))
  
  unlink(filename)
  etable(all_models,
         vcov = "hc1",
         tex = T,
         title = title,
         file = filename,
         dict = var_labs_alt,
         order = c("Disaster", "Post", "log GC"),
         adjustbox = "max width = \\textwidth, center",
         view = T
  )
  return(all_models)
  
}


# Make the table but using the dependence variable that's computed as 
# dependence = (giving / revenue)
my_table_alt <- function(rhs, fe, sample_df, filename, title) {
  
  outcome_list <- list("log_output","log_density","dependence2")
  
  var_labs_alt <- var_labs
  var_labs_alt["dependence2"] = "Dependence (Giving / Revenue)"

  all_models <- lapply(outcome_list, function (x) my_reg(x , rhs, fe, sample_df))
  
  unlink(filename)
  etable(all_models,
         vcov = "hc1",
         tex = T,
         title = title,
         file = filename,
         dict = var_labs_alt,
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
df <- df %>% 
  mutate(
    log_output = log(output),
    log_density = log(density),
    log_fixedcont = log(fixedcont),
    log_fixedcont2 = log(fixedcont2),
    log_pop = log(pop),
    ATT = post*treatment              #specify ATT for marginal effects plotting
  )

# Create centered versions of the scaling variable(s)
center1 <- df %>% filter(is.finite(log(fixedcont)))
center1 <- mean(log(center1$fixedcont))
center2 <- df %>% filter(fixedcont2> 0) %>% filter(is.finite(log(fixedcont2)))
center2 <- mean(log(center2$fixedcont2))

df <- df %>% 
  mutate(
    log_fixedcont_centered = log_fixedcont - center1,
    log_fixedcont2_centered = log_fixedcont2 - center2
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
  log_fixedcont2 = "log GC$_{2007}$",
  log_output = "log Output",
  log_density = "log Density",
  dependence = "Dependence",
  dependence2 = "Dependence",
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
# Both of these will use the version where 
# dependence = (cont - grants - govservices) / cont * 100
# and where
# contributions = cont - grants - govservices

## Some of the interactions ----------------------------------------------------

# Do not center the contributions scaling variable
intdid <- my_table(
  rhs = "ATT*log_fixedcont2 + ATT + treatment + post + log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)),
  filename = "Results/Regression Tables/eap_thru_2007cont.tex",
  title = "Heterogenous Effects Through Contributions"
)

# Center the contributions scaling variable
intdid_centered <- my_table(
  rhs = "ATT*log_fixedcont2_centered + ATT + treatment + post + log_fixedcont2_centered",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2_centered)),
  filename = "Results/Regression Tables/eap_thru_2007cont_centered.tex",
  title = "Heterogenous Effects Through Contributions (Centered)"
)
# lol actually maybe i'm just dumb and centering it doesn't make any difference;
# the scaling coefficient doesn't change, just the difference coefficient...


## All of the interactions -----------------------------------------------------

# Do not center the contributions scaling variable
intdid_allint <- my_table(
  rhs = "post*treatment*log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)),
  filename = "Results/Regression Tables/eap_intdid_allint.tex",
  title = "Heterogenous Effects Through Contributions -- All Interactions"
)

## Check dependence measures ---------------------------------------------------

# Do not center the contributions scaling variable
dep1 <- my_table_dependence(
  rhs = "ATT*log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)),
  filename = "Results/Regression Tables/eap_dep1.tex",
  title = "Heterogenous Effects Through Contributions, Various Dependence"
)

dep2 <- my_table_dependence(
  rhs = "treatment*post*log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)),
  filename = "Results/Regression Tables/eap_dep2.tex",
  title = "Heterogenous Effects Through Contributions, Various Dependence, All Interactions"
)


## Adjust dependence variable & all interactions -------------------------------

# Try using the other dependence measure again: (giving / revenue), but with all
# the interactions this time

# Do not center the contributions scaling variable
intdid_alt <- my_table_alt(
  rhs = "post*treatment*log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)),
  filename = "Results/Regression Tables/eap_intdid_alt.tex",
  title = "Heterogenous Effects Through Contributions, Alt Dependence, All Interactions"
)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Plot the coefficients --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Note from Evan 6/11/24: I haven't updated this with the latest set of regression
# results, so this will not run well. I'll make the corrections so the coefficient
# plots and the ME plots run when we've settled on a regression table we like.

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
                         newdata = datagrid(log_fixedcont2 = c(5, 10, 15, 20)))

  if (i<3){
    slopeest[[i]] <- slopeest[[i]] %>%
      mutate(pct = case_when(
        estimate > 0 ~ paste0(round(exp(estimate)*100,0),"\\%"),
        TRUE ~ paste0("-",round((1-exp(estimate))*100,0),"\\%")))
  } else {
    slopeest[[i]] <- slopeest[[i]] %>% 
      mutate(pct = round(estimate,2))
  }
}

slp1 <- plot_slopes(intdid[[1]],
            variables = "ATT",
            condition = "log_fixedcont2") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in log(Output)") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[1]], mapping = aes(x=log_fixedcont2, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[1]][1,'estimate']+0.1, label = slopeest[[1]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[1]][2,'estimate']+0.1, label = slopeest[[1]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[1]][3,'estimate']+0.1, label = slopeest[[1]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[1]][4,'estimate']-0.1, label = slopeest[[1]][4,'pct'])

slp2 <- plot_slopes(intdid[[2]],
                    variables = "ATT",
                    condition = "log_fixedcont2") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in log(Density)") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[2]], mapping = aes(x=log_fixedcont2, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[2]][1,'estimate']+0.1, label = slopeest[[2]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[2]][2,'estimate']+0.1, label = slopeest[[2]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[2]][3,'estimate']+0.1, label = slopeest[[2]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[2]][4,'estimate']-0.1, label = slopeest[[2]][4,'pct'])

slp3 <- plot_slopes(intdid[[3]],
                    variables = "ATT",
                    condition = "log_fixedcont2") +
  labs(x = "Log of 2007 Contributions",
       y = "Average change in Resource Dependence") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(slopeest[[3]], mapping = aes(x=log_fixedcont2, y=estimate)) +
  annotate("text", x=5,  y=slopeest[[3]][1,'estimate']-2, label = slopeest[[3]][1,'pct']) +
  annotate("text", x=10, y=slopeest[[3]][2,'estimate']-2, label = slopeest[[3]][2,'pct']) +
  annotate("text", x=15, y=slopeest[[3]][3,'estimate']-2, label = slopeest[[3]][3,'pct']) +
  annotate("text", x=20, y=slopeest[[3]][4,'estimate']-2, label = slopeest[[3]][4,'pct'])

slopesplot <- ggarrange(slp1, slp2, slp3, nrow = 3)
annotate_figure(slopesplot, 
                top = text_grob("Average Changes in Nonprofit Indicators Post-Hurricane by Scale of Pre-Disaster GC",
                                face = "bold", size = 14))

tikzDevice::tikz("Results/Figures/slopes_plot.tex", height = 6, width = 5)
print({slopesplot})
dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Coefficient Plots ------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Re-estimate to manually include interaction terms for plotting
intdid <- my_table(
  rhs = "ATT + ATTxGC + treatment + post + log_fixedcont2",
  fe = "as.factor(year) + as.factor(FIPS)",
  sample_df = df %>% filter(is.finite(log_fixedcont2)) %>% mutate(ATTxGC = ATT*log_fixedcont2),
  filename = "Results/Regression Tables/blj_tabling_interactions.tex",
  title = "Heterogenous Effects Through Contributions"
)

#named vector to name variable labels
vlab <- c(ATT = "Disaster x Post",
          ATTxGC = " x Log GC 2007")

models <- list("Output" = basicdid[[1]],
               "Output" = intdid[[1]],
               "Density" = basicdid[[2]],
               "Density" = intdid[[2]],
               "Dependence" = basicdid[[3]],
               "Dependence" = intdid[[3]])

pbasic <- ggcoef_compare(models[c(1, 3, 5)], type = "d",
                         variable_labels = vlab) +
  ggtitle("Basic Models") +
  theme(legend.position = "none") +
  xlab(NULL)


pinter <- ggcoef_compare(models[c(2, 4, 6)], type = "d",
                         variable_labels = vlab) +
  ggtitle("Full Models") +
  #theme(legend.position = "none") +
  xlab("Coefficient Estimate")

ggarrange(pbasic, pinter, nrow = 2, common.legend = T, legend = "bottom")

#Alternate approach
p.outb <- ggcoef_compare(models[1], colour = NULL, variable_labels = vlab) + ggtitle("Output") + xlab("")
p.outi <- ggcoef_compare(models[2], colour = NULL, variable_labels = vlab)
p.denb <- ggcoef_compare(models[3], colour = NULL, variable_labels = vlab) + ggtitle("Density") + xlab("")
p.deni <- ggcoef_compare(models[4], colour = NULL, variable_labels = vlab)
p.depb <- ggcoef_compare(models[5], colour = NULL, variable_labels = vlab) + ggtitle("Dependence") + xlab("")
p.depi <- ggcoef_compare(models[6], colour = NULL, variable_labels = vlab)

coefplots <- ggarrange(p.outb, p.denb, p.depb, p.outi, p.deni, p.depi,
                       nrow = 2, ncol = 3, common.legend = T, legend = "bottom",
                       legend.grob = get_legend(ggcoef_compare(models[c(2, 4, 6)],
                                                               type = "f",
                                                               colour = NULL)))

annotate_figure(coefplots, top = text_grob("Coefficient Plots", size = 18))
