# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: New Regressions

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

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

# Write a function to make the standard errors
do_the_SE <- function(model){
  # Cluster the SEs
  se <- data.frame(summary(model, cluster = "FIPS")$coefficients)$`Std..Error`
  
  # Fix the position problem
  temp <- which(summary(model)[["aliased"]])
  
  for (i in 1:length(temp)){se <- append(se, 1, after=temp[i]-1)}
  return(se)
}



# Read in data
df <- read_csv('Results/cleaned_filtered_data.csv')

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#Update yvars for final outputs
yvars <- c("output", "density")

# Run regressions for each of the outcome variables in "yvars"
firstModels <- lapply(yvars, function(x){
  lm(formula =
       paste0("log(`", x, "`) ~ treatment*post + as.factor(year) + as.factor(FIPS)"),
     data = df)
})

firstModels[[length(firstModels)+1]] <- 
  lm(dependence ~ treatment*post + as.factor(year) + as.factor(FIPS),
     data = df)

names(firstModels) <- c(yvars, "dependence") #label ea. regression by yvar



# Store clustered SEs
firstSE <- lapply(firstModels, do_the_SE)

#Save regression table
stargazer(firstModels,
          title = "Preliminary Difference-in-Differences",
          omit = c("factor","Constant"),
          se = firstSE,
          type = "latex",
          order = c(
            "treatment:post",
            "treatment",
            "post"
          ),
          font.size = 'small',
          covariate.labels = c(
            "Disaster * Post Storm",
            "Disaster",
            "Post Storm"
          ),
          dep.var.labels.include = F,
          column.labels = c(yvars, "dependence"),
          omit.stat = c("rsq", "adj.rsq"), 
          out = "Results/Regression Tables/blj_reg1.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Run regressions for each of the outcome variables in "yvars"
secondModels <- lapply(yvars, function(x){
  lm(formula =
       paste0("log(`", x, "`) ~ treatment:post:log(fixedcont) + treatment*post +
              log(fixedcont) + as.factor(year) + as.factor(FIPS)"),
     data = df[is.finite(log(df$fixedcont)),])
})

secondModels[[length(secondModels)+1]] <- lm(
  dependence ~ treatment:post:log(fixedcont) + treatment*post + log(fixedcont) + 
    as.factor(year) + as.factor(FIPS),
  data = df[is.finite(log(df$fixedcont)),])

names(secondModels) <- c(yvars, "dependence") #label ea. regression by yvar

# Store clustered SEs
secondSE <- lapply(secondModels, do_the_SE)

#Save regression table
stargazer(secondModels,
          title = "Heterogeneous Effects Through Pre-Disaster Contributions",
          omit = c("factor","Constant"),
          se = secondSE,
          type = "latex",
          order = c(
            "treatment:post:log(fixedcont)",
            "treatment:post",
            "treatment",
            "post",
            "log(fixedcont)"
          ),
          font.size = 'small',
          covariate.labels = c(
            "Disaster * Post Storm * Log 2007 Cont.",
            "Disaster * Post Storm",
            "Disaster",
            "Post Storm",
            "Log 2007 Cont."
          ),
          dep.var.labels.include = F,
          column.labels = c(yvars, "dependence"),
          omit.stat = c("rsq", "adj.rsq"), 
          out = "Results/Regression Tables/blj_reg2.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Run regressions for each of the outcome variables in "yvars"
thirdModels <- lapply(yvars, function(x){
  lm(formula =
       paste0("log(`", x, "`) ~ treatment:post:tpost + treatment*post + 
               tpost + as.factor(year) + as.factor(FIPS)"),
     data = df)
})

thirdModels[[length(thirdModels)+1]] <- 
  lm(dependence ~ treatment:post:tpost + treatment*post + tpost +
       as.factor(year) + as.factor(FIPS), data = df)

names(thirdModels) <- c(yvars, "dependence") #label ea. regression by yvar

# Store clustered SEs
thirdSE <- lapply(thirdModels, do_the_SE)

#Save regression table
stargazer(thirdModels,
          title = "Differences-in-Differences with Dynamic Effects",
          omit = c("factor","Constant"),
          se = thirdSE,
          type = "latex",
          order = c(
            "treatment:post:tpost",
            "treatment:post",
            "treatment",
            "post",
            "tpost"
          ),
          font.size = 'small',
          covariate.labels = c(
            "Disaster * Post Storm * Years After",
            "Disaster * Post Storm",
            "Disaster",
            "Post Storm",
            "Years After"
          ),
          dep.var.labels.include = F,
          column.labels = c(yvars, "dependence"),
          omit.stat = c("rsq", "adj.rsq"), 
          out = "Results/Regression Tables/blj_reg3.tex")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
