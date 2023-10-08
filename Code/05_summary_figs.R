# Project Title: Natural Disasters and Giving - Book Chapter
# Script Title: Summary Figures

# Contributor(s): Brenna Jungers, Evan Perry, Drew Westberg
# Last Revised: 2023-04-02

# Purpose: This script generates the figures related to basic descriptive 
# statistics (e.g., this excludes figures that corresponding with a specific 
# analysis / robustness check)


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Data Summary & Visualization ------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df <- read_csv("Results/cleaned_filtered_data.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### The Lolipop Plot -----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Panel A
temp <- main_df %>% 
  filter(treatment == 1) %>% 
  filter(year == 2007 | year == 2009 | year == 2011 | year == 2013) %>% 
  group_by(year) %>% 
  summarise(
    density = mean(density),
    output = mean(output),
    dependence = mean(dependence)
  )

temp <- as.data.frame(t(temp))
temp$variable <- rownames(temp)
rownames(temp) <- NULL
colnames(temp) <- temp[1,]
temp <- temp[-1,]

# Rescale the variables
temp <- temp %>% 
  mutate(
    effect_2009 = (`2009`-`2007`)/`2007`,
    effect_2011 = (`2011`-`2007`)/`2007`,
    effect_2013 = (`2013`-`2007`)/`2007`
  )

l <- c("dependence",  "output", "density")

temp$variable <- factor(temp$year, levels= l)

temp <- temp %>% select(variable, effect_2009, effect_2011, effect_2013)
temp <- temp %>% 
  pivot_longer(cols=2:4, names_to = "year", values_to = "change")
temp$ref <- rep(0,nrow(temp))
temp$year <- factor(temp$year, levels = c("effect_2013", "effect_2011", "effect_2009"))

png("Results/Figures/lineplot_revised.png", width=6, height=5, 
    units="in", res=500)
v <- ggplot(temp, aes(y=variable, x=change)) +
  geom_linerange(
    aes(y = variable, xmin=ref, xmax=change, color=year), 
    position = position_dodge(width = .5),
    lwd=1) + 
  geom_point(size=3, aes(color=year), position = position_dodge(width=0.5)) +
  xlab("\nPercent Change") +
  scale_y_discrete(labels = str_to_title(l)) +
  scale_x_continuous(labels = scales::percent) +
  scale_color_manual(
    values = natparks.pals("Yellowstone", 3, type = "discrete"),
    labels=rev(c("2007-2009","2007-2011","2007-2013"))
  ) +
  theme_bw() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(face="bold"),
    axis.title.y = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )
print(v)
dev.off()


# p1 <- ggplot(temp, aes(x = post, y =  variable)) +
#   geom_segment( aes(x=pre ,xend=post, y=variable, yend=variable), color="grey", 
#                 lwd = 1) +
#   geom_vline(xintercept = 0) +
#   geom_point(size=5, color= natparks.pals("Yellowstone", 1, type = "discrete")) +
#   scale_y_discrete(labels = str_to_title(l)) + 
#   scale_x_continuous(labels = scales::percent) + 
#   theme_bw() +
#   theme(
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     axis.text.y = element_text(face="bold"),
#     axis.title.y = element_blank()
#   ) + 
#   xlab("\n% Change 2007 to 2009, Impacted Counties")
# dev.off()

# # Panel B
# temp <- main_df %>% 
#   filter(treatment == 1) %>% 
#   filter(year == 2007 | year == 2013) %>% 
#   group_by(year) %>% 
#   summarise(
#     density = mean(density),
#     concentration = mean(concentration),
#     wealth = mean(wealth),
#     distribution = mean(distribution),
#     output = mean(output),
#     use = mean(use),
#     dependence = mean(dependence)
#   )
# 
# temp <- as.data.frame(t(temp))
# temp$variable <- rownames(temp)
# colnames(temp) <- c("pre","post","variable")
# temp <- temp[temp$variable != "year",]
# 
# # Rescale the variables
# temp <- temp %>% 
#   mutate(
#     post = (post - pre)/pre,
#     pre = 0
#   )
# 
# temp$variable <- factor(temp$variable, levels= l)
# 
# p2 <- ggplot(temp, aes(x = post, y =  variable)) +
#   geom_segment( aes(x=pre ,xend=post, y=variable, yend=variable), color="grey", 
#                 lwd = 1) +
#   geom_vline(xintercept = 0) +
#   geom_point(size=5, color= natparks.pals("Yellowstone", 1, type = "discrete")) +
#   scale_y_discrete(labels = str_to_title(l)) + 
#   scale_x_continuous(labels = scales::percent) + 
#   theme_bw() +
#   theme(
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     axis.text.y = element_text(face="bold"),
#     axis.title.y = element_blank()
#   ) + 
#   xlab("\n% Change 2007 to 2013, Impacted Counties")
# 
# 
# # png("Results/Figures/differenced_pop_weighted.png", width=8, height=8, 
# #     units="in", res=450)
# print({
#   ggarrange(
#     p1, p2, 
#     ncol = 2, nrow = 1,
#     labels = c("A","B"),legend = "bottom")
# })
# # dev.off()

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Mapping the Geographic Sample ----------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# County shapefile
county_shp <- st_as_sf(county_laea)

# Get the proper geographic sample
dist <- read_csv("Data/sf12010countydistance500miles.csv")
dist <- dist[dist$mi_to_county < 250, ]

# The disaster data again---but just Ike
hurricanes <- read_csv("Data/hurricanes.csv")
hurricanes <- hurricanes[hurricanes$storm == "IKE",]
fema <- read_csv("Data/FEMA_Disasters.csv")
fema <- fema[fema$disasterNumber %in% hurricanes$disasterNumber,]
fema$FIPS <- paste(fema$fipsStateCode, fema$fipsCountyCode, sep="")

# The states with any place with a disaster declaration
my_states <- unique(fema$fipsStateCode)

# The counties within 250 miles of a disaster county
dist <- merge(dist, fema, by.x = "county1", by.y = "FIPS")
counties_250 <- unique(dist$county2)

# The potential geographic sample
county_shp <- county_shp %>% 
  mutate(
    state_fips = str_sub(GEOID, 1, 2)
  ) %>% 
  filter(
    state_fips %in% my_states | GEOID %in% counties_250
  )

# Temporary availability file
temp <- main_df %>% 
  group_by(FIPS) %>% 
  summarise(
    treatment = max(treatment),
    years_in_sample = n(),
    population = mean(pop),
    density = mean(density),
    concentration = mean(concentration),
    wealth = mean(wealth),
    distribution = mean(distribution),
    output = mean(output),
    use = mean(use),
    dependence = mean(dependence),
    CONT = mean(CONT),
    EXPS = mean(EXPS),
    TOTREV = mean(TOTREV),
    TOTASS = mean(TOTASS)
  ) %>% 
  ungroup()

county_shp <- merge(county_shp, temp, by.x="GEOID", by.y="FIPS", all.x=T)
county_shp$treatment <- ifelse(county_shp$treatment == 1, "Treatment", "Control")
county_shp$years_in_sample <- replace_na(county_shp$years_in_sample, 0)

# Make the map

png("Results/Figures/geographic_sample.png", width=7, height = 4, units="in",
    res=600)
v <- tm_shape(county_shp) +
  tm_polygons(
    "treatment",
    border.col = "white",
    title = "Treatment Status",
    palette = rev(natparks.pals("Yellowstone", 2, type="discrete"))) + 
  tm_layout(frame = FALSE)
print(v)
dev.off()


png("Results/Figures/geographic_balance.png", width=7, height = 4, units="in",
    res=600)
v <- tm_shape(county_shp) +
  tm_polygons(
    "years_in_sample",
    border.col = "white",
    title = "# of Years in Sample",
    style = "pretty",
    as.count=T,
    palette = natparks.pals("Denali", 6, type="discrete")) +
  tm_layout(frame = FALSE)
print(v)
dev.off()

rm(county_shp, dist, fema, hurricanes, temp, counties_250, my_states, l)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ### Summary Statistics Table (Old)
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
# summ_df <- main_df %>% 
#   mutate(
#     CONT = CONT/1000000,
#     EXPS = EXPS/1000000,
#     TOTREV = TOTREV/1000000,
#     TOTASS = TOTASS/1000000,
#     density = density * 1000,
#     concentration = concentration * 100,
#     wealth = wealth / 1000000,
#     use = use/1000
#   )
# 
# summ_df <- data.frame(
#   summ_df[
#     c("pop", "LAND_AREA", 
#       "NONPROFITS", "TOTASS", "CONT", "EXPS", "TOTREV",  
#       "density", "concentration", "wealth", "distribution", "output", "use", 
#       "dependence"
#     )
#   ]
# )
# 
# stargazer(summ_df, 
#           summary = TRUE, 
#           summary.stat = c(
#             "mean", "median", "sd", "min", "max"
#           ),
#           digits = 2,
#           notes = "Unbalanced panel; N = 7,745; T = 14; C = 566",
#           style = "aer",
#           covariate.labels = c(
#             "\\quad Population",
#             "\\quad Land Area (sq miles)",
#             "\\quad \\# of Nonprofits",
#             "\\quad Assets (millions USD)",
#             "\\quad Contributions (millions USD)",
#             "\\quad Expenses (millions USD)",
#             "\\quad Revenues (millions USD)",
#             "\\quad Density",
#             "\\quad Concentration",
#             "\\quad Wealth (millions USD)",
#             "\\quad Distribution",
#             "\\quad Output (USD)",
#             "\\quad Resource Use (thousands USD)",
#             "\\quad Resource Dependence"
#           ),
#           out = "Results/summary_stats.tex"
# )
# 
# rm(summ_df)
# 
# #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Contribution Scales --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

conts <- main_df %>% 
  group_by(FIPS) %>% 
  summarise(
    mean_pop = mean(pop),
    mean_cont = mean(CONT)
  )

coef_of_interest <- lm("log(mean_cont, 10) ~ log(mean_pop, 10)", conts)
coef_of_interest <- summary(coef_of_interest)
coef_of_interest <- round(coef_of_interest$coefficients[2], 3)

png("Results/Figures/cont_scale.png", width=6, height=4, units="in",
    res = 600)
v <- ggplot(conts, aes(x=mean_pop, y=mean_cont/1000000)) +
  geom_point(size = 3.5, shape = 21,
             color = "white",
             fill = natparks.pals("Yellowstone", 2, type = "discrete")[2]) +
  scale_x_continuous(trans = scales::log10_trans()) + 
  scale_y_continuous(trans = scales::log10_trans()) + 
  geom_smooth(method='lm', formula= y~x, 
              color = natparks.pals("Yellowstone", 1, type = "discrete"), 
              lwd = 2) +
  annotate(geom = "text", x = 10000, y=1000, 
           label = paste("Slope =", coef_of_interest)) +
  labs(x="\nMean Population (Log Scale)", 
       y="Mean Contributions, Millions USD (Log Scale)\n") +
  theme_bw()
print(v)
dev.off()

rm(conts, coef_of_interest)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### Natural Disasters Figure ---------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

disasters <- read_csv("Data/FEMA_Disasters.csv")

disasters_ts <- disasters %>% 
  filter(
    declarationType == "DR",
    fyDeclared < 2022,
  ) %>% 
  distinct(disasterNumber, .keep_all = T) %>% 
  count(fyDeclared)

png("Results/Figures/major_disasters.png", width=7, height=4, units="in",res=600)
v <- ggplot(disasters_ts, aes(x = fyDeclared, y = n)) +
  geom_line(lwd=1.25, color = natparks.pals("Yellowstone", 1)) +
  geom_point(color="white", size=4) +
  geom_point(color = natparks.pals("Yellowstone", 1), size = 3) + 
  labs(x="\nFiscal Year of Declaration", y="# of Major Disaster Declarations\n") + 
  theme_bw()
print(v)
dev.off()

rm(disasters, disasters_ts)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
### DiD Figures ----------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Population Weighted

ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  mutate(
    id_year_weight = pop/sum(pop)
  )

# DiD Figures
ike_did <- ike_did %>% 
  group_by(treatment, year) %>% 
  summarize(
    density = sum(id_year_weight * density),
    output = sum(output * id_year_weight),
    dependence = sum(dependence * id_year_weight)
  )

# Get the year Ike hit
my_hityear <- max(main_df$hit_year)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Density\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y=log(output), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Output\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y=dependence, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Dependence\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

png("Results/Figures/DiD_pop_weighted.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p5, p7,
    ncol = 2, nrow = 2,
    labels = c("a","b","c"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

ike_did <- ike_did %>% 
  pivot_wider(names_from = treatment, values_from = 3:5)

# Difference Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density_1) - log(density_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Density\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y = log(output_1) - log(output_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Output\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y =  dependence_1 - dependence_0 )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Dependence\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

png("Results/Figures/differenced_pop_weighted.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p5, p7, 
    ncol = 2, nrow = 2,
    labels = c("a","b","c"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()

# Non-population weight trend figure

ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  summarize(
    density = mean(density),
    output = mean(output),
    dependence = mean(dependence)
  )

# Get the year Ike hit
my_hityear <- max(main_df$hit_year)

# Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="",y="Log Density\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y=log(output), color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Log Output\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y=dependence, color=as.factor(treatment))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2) +
  labs(x="\nYear",y="Dependence\n") +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels=c("Control","Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9))

png("Results/Figures/DiD.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p5, p7,
    ncol = 2, nrow = 2,
    labels = c("a","b","c"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()


ike_did <- ike_did %>% 
  pivot_wider(names_from = treatment, values_from = 3:5)

# Difference Plots
p1 <- ggplot(ike_did, aes(x=year, y=log(density_1) - log(density_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="",y="Log Density\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p5 <- ggplot(ike_did, aes(x=year, y = log(output_1) - log(output_0))) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Log Output\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

p7 <- ggplot(ike_did, aes(x=year, y =  dependence_1 - dependence_0 )) +
  geom_vline(xintercept = my_hityear) +
  geom_line(lwd = 2, color = natparks.pals("Yellowstone", 1)) +
  labs(x="\nYear",y="Dependence\n") + 
  theme_bw() +
  theme(text = element_text(size = 9))

png("Results/Figures/differenced.png", width=8, height=6, 
    units="in", res=450)
print({
  ggarrange(
    p1, p5, p7, 
    ncol = 2, nrow = 2,
    labels = c("a","b","c"),
    common.legend = TRUE, legend = "bottom")
})
dev.off()


rm(ike_did, p1, p5, p7, my_hityear)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Contributions ---------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Visualize contributions through time

main_df %>%
  mutate(old_CONT = lag(CONT)) %>% 
  mutate(change_CONT = CONT - old_CONT) %>% 
  na.omit() %>% 
  filter(year >= 2002 & year <= 2011) %>% 
  ggplot(aes(x=year, y=change_CONT, color = County_name)) +
  geom_line(show.legend = F) + 
  facet_wrap(~treatment)

main_df %>%
  mutate(old_CONT = lag(CONT)) %>% 
  mutate(change_CONT = CONT - old_CONT) %>% 
  na.omit() %>%
  group_by(treatment) %>% 
  summarise(across(ends_with("CONT"), c(min, mean, max), .names = "{col}_{fn}"))

main_df %>% 
  ggplot(aes(x=as.factor(year), y=CONT)) +
  geom_boxplot() +
  facet_wrap(~treatment)

main_df %>% 
  filter(year==2007) %>% 
  ggplot(aes(fixedcont/1000000)) +
  geom_density() +
  xlim(0,250)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Dependence ------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df %>% 
  ggplot(aes(dependence)) +
  geom_density() +
  facet_grid(year~treatment)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Population ------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>% 
  group_by(treatment, year) %>% 
  summarise(mean = mean(pop),
            q_ = quantile(pop, c(0.05, 0.25, 0.5, 0.75, 0.95)),
            q = c("q05", "q25", "q50", "q75", "q95")) %>%
  pivot_wider(names_from = q, values_from = q_) %>%
  pivot_longer(!c("year", "treatment"), names_to = "quantile") %>%
  mutate(quantile = factor(quantile, levels = c("q05", "q25", "q50", "mean", "q75", "q95"))) %>%
  mutate(value = value/100000) %>% 
  ggplot(aes(x=year, y=value, group=quantile, color=quantile, linetype=quantile, size=quantile)) +
  geom_line() +
  theme_bw() +
  scale_size_manual(name = "quantile", values = c(0.5, 0.5, 0.5, 1, 0.5, 0.5)) +
  scale_linetype_manual(name = "quantile", values = c("dashed", "dotted", "dotdash", "solid", "dotted", "dashed")) +
  geom_vline(xintercept = "2008", color="red")+
  ylab("Population (100,000s)") +
  facet_wrap(~treatment)

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>%
  mutate(lpop = log(pop)) %>% 
  group_by(year, treatment) %>% 
  summarise(across(lpop, list(mean=mean, sd=sd), .names = "{fn}.pop"), n=n()) %>% 
  mutate(se.pop = sd.pop / sqrt(n),
         lower.ci = mean.pop - qt(1 - (0.05 / 2), n - 1) * se.pop,
         upper.ci = mean.pop + qt(1 - (0.05 / 2), n - 1) * se.pop) %>% 
  ggplot(aes(x=year, y=mean.pop, group=treatment, color=treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower.ci, ymax=upper.ci), alpha=.2) +
  #facet_wrap(~ treatment, ncol = 1) +
  theme_bw()+
  ylab("log(population)") +
  xlab("Year") +
  geom_vline(xintercept="2008", color="red", linetype="dashed", size=1) +
  labs(title = "Log(Population) Trends")

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>%
  group_by(year, treatment) %>% 
  summarise(across(pop, list(mean=mean, sd=sd), .names = "{fn}.{col}"), n=n()) %>% 
  mutate(se.pop = sd.pop / sqrt(n),
         lower.ci = mean.pop - qt(1 - (0.05 / 2), n - 1) * se.pop,
         upper.ci = mean.pop + qt(1 - (0.05 / 2), n - 1) * se.pop) %>% 
  ggplot(aes(x=year, y=mean.pop, group=treatment)) +
  geom_line() +
  facet_wrap(~ treatment, ncol = 1, scales = "free") +
  theme_bw()

# Population Test

m_pop_prepost <- lm(log(pop) ~ post + as.factor(year) + as.factor(FIPS), data = df %>% filter(treatment==1))
m_pop <- lm(pop ~ treatment*post + as.factor(year) + as.factor(FIPS), data = df)
m_popl <- update(m_pop, log(pop) ~ .)

stargazer(m_pop_prepost,m_popl,
          se = list(coef(summary(m_pop_prepost, cluster="FIPS"))[, "Std. Error"],
                    coef(summary(m_popl, cluster="FIPS"))[, "Std. Error"]),
          omit = c("factor","Constant"),
          type = "latex",
          omit.stat = c("rsq", "adj.rsq"))

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Density ---------------------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>% 
  group_by(treatment, year) %>% 
  summarise(mean = mean(density),
            q_ = quantile(density, c(0.05, 0.25, 0.5, 0.75, 0.95)),
            q = c("q05", "q25", "q50", "q75", "q95")) %>%
  pivot_wider(names_from = q, values_from = q_) %>%
  pivot_longer(!c("year", "treatment"), names_to = "quantile") %>%
  mutate(quantile = factor(quantile, levels = c("q05", "q25", "q50", "mean", "q75", "q95"))) %>%
  ggplot(aes(x=year, y=value, group=quantile, color=quantile, linetype=quantile, size=quantile)) +
  geom_line() +
  theme_bw() +
  scale_size_manual(name = "quantile", values = c(0.5, 0.5, 0.5, 1, 0.5, 0.5)) +
  scale_linetype_manual(name = "quantile", values = c("dashed", "dotted", "dotdash", "solid", "dotted", "dashed")) +
  geom_vline(xintercept = "2008", color="red")+
  ylab("Density (# Nonprofits/Population)") +
  facet_wrap(~treatment)

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>% 
  group_by(treatment, year) %>% 
  summarise(mean = mean(density),
            q_ = quantile(density, c(0.25, 0.5, 0.75)),
            q = c("q25", "q50", "q75")) %>%
  pivot_wider(names_from = q, values_from = q_) %>%
  pivot_longer(!c("year", "treatment"), names_to = "quantile") %>%
  mutate(quantile = factor(quantile, levels = c("q25", "q50", "mean", "q75"))) %>%
  ggplot(aes(x=year, y=value, group=quantile, color=quantile, linetype=quantile, size=quantile)) +
  geom_line() +
  theme_bw() +
  scale_size_manual(name = "quantile", values = c(0.5, 0.5, 1, 0.5)) +
  scale_linetype_manual(name = "quantile", values = c("dotted", "dotdash", "solid", "dotted")) +
  geom_vline(xintercept = "2008", color="red")+
  ylab("Density (# Nonprofits/Population)") +
  facet_wrap(~treatment)

main_df %>% 
  mutate(year = factor(year)) %>% 
  mutate(treatment = factor(treatment, levels = c(0, 1), labels = c("untreated", "treated"))) %>% 
  mutate(pop_percentile = case_when(pop>=quantile(pop, 0.75) ~ "75th - 100th",
                                    pop>=quantile(pop, 0.50) ~ "50th - 74th",
                                    pop>=quantile(pop, 0.25) ~ "25th - 49th",
                                    TRUE ~ "0 - 24th")) %>% 
  group_by(treatment, year, pop_percentile) %>% 
  summarise(mean = mean(density),
            q_ = quantile(density, c(0.25, 0.5, 0.75)),
            q = c("q25", "q50", "q75")) %>%
  pivot_wider(names_from = q, values_from = q_) %>%
  pivot_longer(!c("year", "treatment", "pop_percentile"), names_to = "quantile") %>%
  mutate(percentile = factor(quantile, levels = c("q25", "q50", "mean", "q75"))) %>%
  ggplot(aes(x=year, y=value, group=percentile, color=percentile, linetype=percentile, size=percentile)) +
  geom_line() +
  theme_bw() +
  scale_size_manual(name = "percentile", values = c(0.5, 0.5, 1, 0.5)) +
  scale_linetype_manual(name = "percentile", values = c("dotted", "dotdash", "solid", "dotted")) +
  geom_vline(xintercept = "2008", color="red")+
  ylab("Density (# Nonprofits/Population)") +
  facet_grid(pop_percentile ~ treatment)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Concentration and Dependence Trends -----------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

### Were larger (contributions) networks relatively more concentrated before 2008?

## Are these variables normally distributed?
main_df %>% 
  filter(year==2007) %>% 
  ggplot(aes(CONT)) + geom_density()

main_df %>% 
  filter(year==2007) %>% 
  ggplot(aes(concentration)) + geom_density()
#No -- long right tails

## Use a non-parametric correlation test for non-normal variables (Spearman)
df_cor <- main_df %>% filter(year==2007)

cor.test(df_cor$CONT, df_cor$concentration, method = "spearman")

#Spearman's rho (correlation coefficient) of -0.5 indicates negative correlation.
#Networks with more access to CONT tended to be less concentrated in 2007 (p<0.05)


### Were larger networks more dependent on contributions before 2008?

## Is dependence normally distributed?
df_cor %>% ggplot(aes(dependence)) + geom_density()
#No -- long right tail

cor.test(df_cor$CONT, df_cor$dependence, method = "spearman")

#Spearman's rho of 0.14 indicates positive correlation.
#Networks with more access to CONT tended to be more dependent on CONT relative
#to other revenue sources in 2007 (p<0.05)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## DiD Pretrends Figure --------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Population Weighted
ike_did <- main_df %>% 
  group_by(year, treatment) %>% 
  mutate(id_year_weight = pop/sum(pop)) %>% 
  group_by(year, treatment) %>% 
  mutate(across(c("density", "dependence", "output"), ~.*id_year_weight)) %>% 
  summarise(across(c("density", "dependence", "output"), list(mean),
                   .names = "{col}"))

# Add empty 2008 data for plotting
data2008 <- data.frame(rbind(c(2008, 0, NA, NA, NA),
                             c(2008, 1, NA, NA, NA)))
names(data2008) <- names(ike_did)

ike_did <- ike_did %>% bind_rows(data2008)

#Graphics

poutput <- ike_did %>% 
  ggplot(aes(year, log(output), color = as.factor(treatment))) +
  geom_vline(xintercept = 2008) +
  geom_line(lwd = 2) +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels = c("Control", "Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9)) +
  scale_x_continuous(breaks = seq(2000, 2013)) +
  labs(x="",y="log(Output)\n")

pdensity <- ike_did %>% 
  ggplot(aes(year, log(density), color = as.factor(treatment))) +
  geom_vline(xintercept = 2008) +
  geom_line(lwd = 2) +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels = c("Control", "Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9)) +
  scale_x_continuous(breaks = seq(2000, 2013)) +
  labs(x="",y="log(Density)\n")

pdependence <- ike_did %>% 
  ggplot(aes(year, dependence, color = as.factor(treatment))) +
  geom_vline(xintercept = 2008) +
  geom_line(lwd = 2) +
  scale_color_manual(values = rev(natparks.pals("Yellowstone", 2)),
                     labels = c("Control", "Treatment")) +
  theme_bw() +
  theme(legend.title = element_blank(), text = element_text(size = 9)) +
  scale_x_continuous(breaks = seq(2000, 2013)) +
  labs(x="\nYear",y="Resource Dependence\n")

ggarrange(poutput, pdensity, pdependence,
          ncol = 1, common.legend = T, legend = "bottom")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Summary Stats Table ----------------------------------------------------------
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

my_stats <- c("pop", "LAND_AREA", "NONPROFITS", "CONT", "EXPS", "TOTREV", "output",
              "density", "dependence")

summ_table <- main_df %>% 
  mutate(density = density * 1000) %>% 
  select(all_of(c(my_stats, 'treatment'))) %>% 
  mutate(across(all_of(c("CONT", "EXPS", "TOTREV")), ~.x / 1000000)) %>% 
  group_by(treatment) %>% 
  summarise(
    across(everything(), 
           list(mean = mean, median = median, sd = sd, min = min, max = max), 
           .names = "{col}_{fn}"), n = n()) %>% 
  pivot_longer(!treatment) %>% 
  mutate(treatment = factor(treatment, levels = c(0,1), 
         labels = c("Untreated", "Treated"))) %>% 
  pivot_wider(names_from = "treatment") %>% 
  mutate(
    var = stringi::stri_replace_all_regex(
      pattern =  c('_mean', '_median', '_sd', '_min', '_max'), 
      replacement = rep('', 5), 
      name,  vectorize = FALSE),
    var = stringi::stri_replace_all_regex(
      pattern =  my_stats, 
      replacement = c(
        'Population', 'Land Area', '# of Nonprofits', 'Contributions (mil USD)', 
        'Expenses (mil USD)', 'Revenues (mil USD)', 'Output (USD)', 'Density',
        'Resource Dependence'),
      var,  vectorize = FALSE),
    stat = stringi::stri_replace_all_regex(
      pattern = paste(my_stats, '_', sep=''),
      replacement = rep('', length(my_stats)),
      name, vectorize = FALSE
    ),
    stat = stringi::stri_replace_all_regex(
      pattern = c('mean', 'median', 'sd', 'min', 'max'),
      replacement = c('Mean', 'Median', 'SD', 'Min', 'Max'),
      stat, vectorize = FALSE)
  ) %>% 
  select(var, stat, Untreated, Treated)


print(xtable::xtable(summ_table, digits = 2), 
      include.rownames=FALSE,
      type = 'latex',
      'Results/summary-statistics.tex')

rm(summ_table, my_stats)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
