##################################################
## Project: Matching
## Script purpose: Use synthetic data to demo matching
## Date: October 7, 2019
## Author: Zack Larsen
##################################################


# setup -------------------------------------------------------------------

library(pacman)
library(tidyverse)
library(magrittr)

library(dtplyr)

library(gt)

p_load(skimr, synthpop, random, conflicted, data.table,
       infer, rsample, janitor, broom, glue, lobstr, sloop, vctrs, 
       fs, scales, furrr, tictoc, rsample, forcats, MASS, pryr,
       progress, vroom, 
       glm2, speedglm, pscl,
       ggthemes, ggforce, ggpubr, ggsci, treemapify,
       #gifski, transformr, gganimate,
       patchwork, see, vioplot, ggridges, sm,
       MatchIt, cobalt, AER)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
#conflict_prefer("View", "utils")




# View troubleshooting ----------------------------------------------------


# Option 1: Install and load fonts
install.packages("extrafont")
library(extrafont)


# Option 2: Don't load gganimate



# File System -------------------------------------------------------------

# create a new directory
tmp <- dir_create(file_temp())
tmp

# create new files in that directory
file_create(path(tmp, "my-file.txt"))
dir_ls(tmp)






# US National Medical Expenditure Survey  ---------------------------------

# The data set NMES1988 in the AER package contains a sample of 
# individuals over 65 who are covered by Medicare in order to 
# assess the demand for health care through physician office
# visits, outpatient visits, ER visits, hospital stays, etc. 

data(NMES1988)

glimpse(NMES1988)



# 1. Show through graphical means that there are more respondents 
#   with 0 visits than might be expected under a Poisson model.

NMES1988$visits

p.mean <- mean(NMES1988$visits) # 5.774399
var(NMES1988$visits) # 45.68712

plot(NMES1988$visits)
hist(NMES1988$visits)

dpois(0, p.mean) # 0.003106065
rpois(100, lambda = p.mean)


hist(NMES1988$visits)
plot(0:100, dpois(x=0:100, lambda=6), xlim=c(-2,20))






y = rpois(10^6, 6);  up=max(y)
hist(y, prob=T, br=(-1:up)+.5, col="skyblue2", xlab="x", 
     main="Simulated Sample from POIS(6) with Normal Approximation")
curve(dnorm(x, mean(y), sd(y)), col="red", lwd=2, add=T) 

w=0:36;  pdf=dbinom(w, 600, .01)
points(w, pdf, pch=19, col="darkgreen")





# 2. Fit a model for the number of physician office visits using 
#     chronic, health, and insurance as predictors for the Poisson 
#     count, and chronic and insurance as the predictors for the 
#     binary part of the model. Then, provide interpretations in 
#     context for the following model parameters:

# chronic in the Poisson part of the model
# poor health in the Poisson part of the model
# the Intercept in the logistic part of the model
# insurance in the logistic part of the model

NMES1988 %>% 
  select(visits, chronic, health, insurance)




p.mod1 <- glm(formula = visits ~ chronic + factor(health) + factor(insurance), 
              family = poisson, 
              data = NMES1988)

p.mod1
summary(p.mod1)
tidy(p.mod1)

# Exponentiated coefficients
exp(coef(p.mod1))

# Goodness-of-fit test
gof.ts = p.mod1$deviance
gof.pvalue = 1 - pchisq(gof.ts, p.mod1$df.residual)
gof.pvalue # 0





zip.mod2 <- pscl::zeroinfl(formula = visits ~ factor(health) + factor(insurance) | factor(health) + factor(insurance), 
         data = NMES1988)

exp(coef(zip.mod2)) # exponentiated coefficients




# Is there significant evidence that the ZIP model is an
# improvement over a simple Poisson regression model?










# DoctorVisits ------------------------------------------------------------

# Data was collected on doctor visits from a sample of 5,190 people
# in the 1977/1978 Australian Health Survey. Cameron and Trivedi
# (1986) sought to explain the variation in doctor visits using one 
# or more explanatory variables. The data can be found in an R data
# set from 
# library(AER)
# accessible with the command 
# data("DoctorVisits").
# Variable descriptions can be found under 
# help("DoctorVisits")

data("DoctorVisits")
help("DoctorVisits")



DoctorVisits


# Explore the use of a zero-inflated model for this data. Begin with 
# a histogram of the number of visits, some EDA, and fitting several
# models. Summarize your results.

hist(DoctorVisits$visits)















# Synthesize Data ---------------------------------------------------------

# https://arxiv.org/pdf/1712.04078.pdf

synth_df <- tibble(
  mid = sample(10000000:20000000, 10000, replace=FALSE), # http://www.cookbook-r.com/Numbers/Generating_random_numbers/
  treat = rbinom(10000,1,0.3),
  total_spend_bsln = (rgamma(10000,1,1) * 20000) + (runif(10000,0,1) / 100),
  adm_spend_bsln= 0.4 * total_spend_bsln,
  pro_spend_bsln = 0.2 * total_spend_bsln,
  out_spend_bsln = 0.2 * total_spend_bsln,
  pdp_spend_bsln = 0.1 * total_spend_bsln,
  er_ipa_spend_bsln = 0.09 * total_spend_bsln,
  er_op_spend_bsln = 0.01 * total_spend_bsln,
  adm_count_bsln = rpois(10000,c(1,2,5)),
  pro_count_bsln = rpois(10000,c(1,2,5,10,20)),
  out_count_bsln = rpois(10000,c(1,2,5,10)),
  pdp_count_bsln = rpois(10000,c(5,20,3,50)),
  er_ipa_count_bsln = rpois(10000,c(1,2,5,10)),
  er_op_count_bsln = rpois(10000,c(1,10,5,10)),
  
  total_spend_perf = (rgamma(10000,1,1) * 20000) + (runif(10000,0,1) / 100),
  adm_spend_perf= 0.4 * total_spend_perf,
  pro_spend_perf = 0.2 * total_spend_perf,
  out_spend_perf = 0.2 * total_spend_perf,
  pdp_spend_perf = 0.1 * total_spend_perf,
  er_ipa_spend_perf = 0.09 * total_spend_perf,
  er_op_spend_perf = 0.01 * total_spend_perf,
  adm_count_perf = rpois(10000,c(1,2,5)),
  pro_count_perf = rpois(10000,c(1,2,5,10,20)),
  out_count_perf = rpois(10000,c(1,2,5,10)),
  pdp_count_perf = rpois(10000,c(5,20,3,50)),
  er_ipa_count_perf = rpois(10000,c(1,2,5,10)),
  er_op_count_perf = rpois(10000,c(1,10,5,10))
)

glimpse(synth_df)

skim(synth_df)

synth_df %>% 
  group_by(treat) %>% 
  skim()










# Add in the treatment effect in the performance period:
synth_df %<>% 
  mutate_at(vars(matches("spend"), -contains("bsln")),
          list(~ifelse(treat == 0, .*1.1, .*0.6)
          )
  ) %>% 
  mutate_at(vars(matches("cnt"), -contains("bsln")),
          list(~ifelse(treat == 0, .*1.09, .*0.64)
          )
  )
  








# Add penny to spend vars in perf:
synth_df %<>% 
  mutate_at(vars(contains("spend"), -contains("bsln")), function(x){x + 0.01})



# Round:
synth_df %<>% 
  mutate_at(vars(contains("spend")), round, 2)



synth_df %>% 
  glimpse()


synth_df %>% 
  select(mid, treat) %>% 
  head(5) %>% 
  View()





# Baseline Means Bootstrap ------------------------------------------------

glimpse(synth_df)

synth_df %>% 
  select(treat, contains("bsln")) %>% 
  glimpse()

synth_df %>% 
  select(treat, contains("bsln"))

baseline_vars <- synth_df %>% 
  select(contains("bsln")) %>% 
  colnames()







split_means <- function(split, var_list){
  means_list <- vector(mode = "list", length = length(var_list))
  names(means_list) <- var_list
  for(var in var_list){
    means_list[[var]] <- mean(split %>% pull(var), na.rm = TRUE)
  }
  return(means_list)
}

split_means(synth_df, baseline_vars)






# Nested list column workflow:
synth_df %>% 
  select(-mid) %>% 
  group_by(cyl) %>% 
  nest()

synth_rowcount <- nrow(synth_df)



Combined <- bind_rows(replicate(1000, synth_df %>% select(treat, contains("bsln")) %>% sample_n(size = synth_rowcount, replace = TRUE), simplify=F), .id="Obs")

Combined


# Means only:
Combined %>% 
  group_by(Obs) %>% 
  summarise_at(vars(everything(), -treat), mean) %>% 
  summarise_at(vars(everything(), -Obs), mean, na.rm = TRUE) %>% 
  mutate(x=1) %>% 
  pivot_longer(-x, names_to = "Variable", values_to = "Baseline Mean") %>% 
  select(-x) %>% 
  arrange(Variable)

# Means only but taking the overall average instead of average of averages:
Combined %>% 
  summarise_at(vars(everything(), -Obs, -treat), mean, na.rm = TRUE) %>% 
  mutate(x=1) %>% 
  pivot_longer(-x, names_to = "Variable", values_to = "Baseline Mean") %>% 
  select(-x) %>% 
  arrange(Variable)





# Percentile confidence interval approach:
Combined %>% 
  group_by(Obs) %>% 
  summarise_at(vars(everything(), -treat), mean) %>% 
  summarise_at(
    vars(everything(), -Obs), 
    list(
      lower = ~quantile(., 0.025),
      midpoint = ~quantile(., 0.5),
      upper = ~quantile(., 0.975)
    )
  ) %>% 
  mutate(x=1) %>% 
  pivot_longer(-x, names_to = "Variable", values_to = "value") %>% 
  select(-x) %>% 
  arrange(Variable)












bootstrap_distribution <- synth_df %>% 
  select(treat, contains("bsln")) %>% 
  specify(response = total_spend_bsln) %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean")

bootstrap_distribution

visualize(bootstrap_distribution)


percentile_ci <- bootstrap_distribution %>% 
  get_confidence_interval(level = 0.95, type = "percentile")

percentile_ci

visualize(bootstrap_distribution) + 
  shade_confidence_interval(endpoints = percentile_ci)

bootstrap_distribution %>% 
  visualize(bins = 15, boundary = 20000, fill = "pink") + 
  shade_confidence_interval(endpoints = percentile_ci, fill = "skyblue", color = "dodgerblue", alpha = 0.6) +
  geom_vline(xintercept = 20000, size = 0.75, color = "red", linetype = "dashed") + 
  theme_bw()











# Take samples of size n = nrows(synth_df) with replacement;
# do this 1000 times
samples <- synth_df %>% 
  select(-contains("perf")) %>% 
  rep_sample_n(size = nrow(synth_df), reps = 1000, replace = TRUE)

samples

samples %>% 
  ungroup() %>% 
  summarise_at(vars(everything(), -replicate, -mid, -treat), mean, na.rm = TRUE) %>% 
  mutate(x=1) %>% 
  pivot_longer(-x, names_to = "Variable", values_to = "Baseline Mean") %>% 
  select(-x) %>% 
  arrange(Variable)




















# rsample workflow:
# https://tidymodels.github.io/rsample/articles/Working_with_rsets.html
set.seed(1234)
boots <- bootstraps(synth_df %>% select(treat, contains("bsln")), times = 1000, apparent = FALSE)

boots

object_size(boots) # 41.6 MB

# Object size per resample
object_size(boots)/nrow(boots) # 41.6 kB

# Fold increase is <<< 50 even though we resampled 1,000 times!
as.numeric(object_size(boots)/object_size(synth_df %>% select(treat, contains("bsln"))))
# 49.42134

boots[1,]$splits
boots$splits[[1]]

as.data.frame(boots$splits[[1]])


split_means <- function(splits, var_list){
  means_list <- vector(mode = "list", length = length(var_list))
  names(means_list) <- var_list
  for(var in var_list){
    means_list[[var]] <- mean(analysis(splits) %>% pull(var), na.rm = TRUE)
  }
  return(means_list)
}

# Map the mean calculations function to each of the splits:
boots$results <- map(boots$splits,
                     split_means,
                     baseline_vars)

# boots %>% 
#   unnest(cols = results) %>% 
#   unnest(cols = results)

bind_rows(boots$results) %>% 
  summarise_all(mean)




















# dtplyr approach for speed:

dtplyr_samples <- lazy_dt(synth_df %>% 
  select(-contains("perf"), -mid, -treat) %>% 
  rep_sample_n(size = nrow(synth_df), reps = 1000, replace = TRUE))


dtplyr_means <- dtplyr_samples %>% 
  group_by(replicate) %>% 
  summarise_all(mean) %>% 
  summarise_all(mean) %>% 
  select(-replicate) %>% 
  as_tibble()

dtplyr_means
















# Compare Groups At Baseline ----------------------------------------------

synth_df %>% 
  select(-mid) %>% 
  group_by(treat) %>% 
  summarise_all(list(mean=mean, max=max))


baseline_plotdf <- synth_df %>% 
  select(-mid) %>% 
  gather(key = 'var', value = 'amount', -treat)

baseline_plotdf %>% 
  head()


ggplot(baseline_plotdf %>% filter(var == 'total_spend_bsln'), 
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.8) + 
  facet_grid(rows = vars(var)) + 
  scale_fill_brewer(palette="Set1") +
  theme_classic() + 
  theme(legend.position = "top")

ggplot(baseline_plotdf %>% filter(var == 'total_spend_bsln'), 
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.8) + 
  facet_grid(rows = vars(var)) + 
  scale_fill_brewer(palette="Set1") +
  theme_blackboard() + 
  theme(legend.position = "top")

ggplot(baseline_plotdf %>% filter(var == 'total_spend_bsln'), 
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.8) + 
  facet_grid(rows = vars(var)) + 
  theme_modern() + 
  theme(legend.position = "top")

ggplot(baseline_plotdf %>% filter(var == 'total_spend_bsln'), 
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.8) + 
  facet_grid(rows = vars(var)) + 
  theme_abyss() + 
  theme(legend.position = "top")






vio_df <- baseline_plotdf %>% 
  mutate(
    period = ifelse(str_detect(var, "_bsln"), "Baseline", "Performance")
    ) %>% 
  filter(
    str_detect(var, "spend"),
    !str_detect(var, "total")
  ) %>% 
  mutate(var = str_replace_all(var, "_bsln|_perf", ""))

vio_df




#https://rpkgs.datanovia.com/ggpubr/reference/ggviolin.html
anim <- ggviolin(
  vio_df,
  "var", 
  "amount", 
  color = as.factor("treat"),
  add = "boxplot"
  ) + 
  ylab(label = dollar) + 
  theme_modern() + 
  coord_flip() + 
  # Here comes the gganimate code
  transition_states(
    period,
    transition_length = 1,
    state_length = 0.5
  ) +
  labs(title = 'Distribution of spend by period: {closest_state}') + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim

anim_save("spend_by_period.gif", anim)









ggviolin(iris, "Species", "Sepal.Length", color = "Species",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         add = "boxplot", add.params = list(fill = "white"))

iris

ggviolin(iris, "Species", "Sepal.Length", color = "Species",
         #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
         #palette = "rickandmorty",
         palette = "simpsons",
         add = "boxplot", add.params = list(fill = "white"))






ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) + 
  geom_violindot(fill_dots = "white") + 
  scale_fill_material_d() +
  theme_blackboard()





















?scale_fill_brewer



ggplot(baseline_plotdf %>% 
         filter(
           var %like% 'count'
          ),
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.4) + 
  facet_grid(rows = vars(var)) + 
  scale_fill_brewer(palette="Set1") + 
  theme_gdocs() + 
  theme(legend.position = "top")



ggplot(baseline_plotdf %>% 
         filter(
           amount < 10000,
           var %like% 'spend'
         ),
       aes(x=amount, fill=as.factor(treat))) +
  geom_density(alpha=0.4) + 
  facet_grid(rows = vars(var)) + 
  scale_fill_brewer(palette="Set1") + 
  theme_gdocs() + 
  theme(legend.position = "top")



# Add mean lines
p + 
  geom_vline(data=mu, aes(xintercept=grp.mean, color=sex),
             linetype="dashed")




# Compare between group and period ----------------------------------------


synth_df %>% 
  select(-mid) %>% 
  group_by(treat) %>% 
  summarise_all(mean) %>% 
  pivot_longer(names_to = "var", values_to = "mean", -treat) %>% 
  mutate(period = ifelse(var %like% "_bsln", "bsln", "perf")) %>% 
  mutate(var = stringr::str_replace_all(var, "_bsln", "_perf")) %>% 
  mutate(var = stringr::str_replace_all(var, "_perf", "")) %>% 
  pivot_wider(names_from = "period", values_from = mean) %>% 
  mutate(diff = perf - bsln) %>% 
  arrange(desc(var), desc(treat)) %>% 
  select(-bsln, -perf) %>% 
  pivot_wider(names_from = "treat", values_from = "diff")
  





# Treemap -----------------------------------------------------------------



synth_tree <- synth_df %>% 
  select(contains("spend_bsln")) %>% 
  summarise_all(mean) %>% 
  pivot_longer(names_to = "Category", values_to = "Spend", cols = everything())

synth_tree


ggplot(synth_tree, aes(area = Spend, fill = "blue", label = Category)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", 
                    colour = "white",
                    place = "centre",
                    grow = TRUE) + 
  ggtitle("Baseline Spend Breakdown")





ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE) + 
  ggtitle("Treemap of G20 countries' GDP")




# Boxplots ----------------------------------------------------------------

baseline_plotdf <- synth_df %>% 
  select(-mid) %>% 
  gather(key = 'var', value = 'amount', -treat)

vio_df <- baseline_plotdf %>% 
  mutate(
    period = ifelse(str_detect(var, "_bsln"), "Baseline", "Performance")
  ) %>% 
  filter(
    str_detect(var, "spend"),
    !str_detect(var, "total")
  ) %>% 
  mutate(var = str_replace_all(var, "_bsln|_perf", ""))


vio_df



bp <- ggplot(vio_df %>% filter(amount < 15000), aes(var, amount, fill=factor(treat))) +
  ggtitle("Distribution of Spend by Claim Category and Treatment Status") + 
  theme_modern() + 
  xlab("Claim Category") + 
  ylab("Spend (PMPM)") + 
  scale_y_continuous(labels = dollar) + 
  coord_flip() + 
  geom_boxplot(
    show.legend = TRUE,
    outlier.alpha = 0.1,
    outlier.shape = 10, 
    outlier.colour = "black",
    outlier.size = 0.5
  )


bp




bp_animated <- bp + 
  # Here comes the gganimate code
  transition_states(
    period,
    transition_length = 1,
    state_length = 0.5
  ) +
  labs(title = 'Distribution of total spend by period: {closest_state}') + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')



anim_save("spend_by_period_boxplots.gif", bp_animated)






# ggridges ----------------------------------------------------------------


baseline_plotdf <- synth_df %>% 
  select(-mid) %>% 
  gather(key = 'var', value = 'amount', -treat)

baseline_plotdf


vio_df <- baseline_plotdf %>% 
  mutate(
    period = ifelse(str_detect(var, "_bsln"), "Baseline", "Performance")
  ) %>% 
  filter(
    str_detect(var, "spend"),
    !str_detect(var, "total")
  ) %>% 
  mutate(var = str_replace_all(var, "_bsln|_perf", ""))

vio_df




#https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html

ggr <- vio_df %>%
  mutate(
    treat_fct = as.factor(treat),
    var_fct = as.factor(var)
  ) %>%
  ggplot(aes(y = var_fct)) +
  geom_density_ridges(
    aes(x = amount, fill = treat_fct), 
    alpha = .8, color = "white", from = 0, to = 10000,
    show.legend = TRUE
  ) +
  scale_x_continuous(label = dollar) + 
  theme(legend.position = "top") + 
  labs(
    x = "Spend ($PMPM)",
    y = "Claim Category",
    title = "Treat vs. Control Spend by Category",
    subtitle = "Units: dollars PMPM"
  ) +
  theme_ridges(grid = FALSE)

ggr



# Same as above but with animation for time period:
ggr + 
# Here comes the gganimate code
  transition_states(
    period,
    transition_length = 1,
    state_length = 0.5
  ) +
  labs(title = 'Distribution of total spend by period: {closest_state}') + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')






# gganimate ---------------------------------------------------------------




animate_df <- synth_df %>% 
  select(treat, total_spend_bsln, total_spend_perf) %>% 
  pivot_longer(names_to = "var", values_to = "spend", -treat) %>% 
  mutate(period = ifelse(var %like% "_bsln", "Baseline", "Performance")) %>% 
  mutate(var = stringr::str_replace_all(var, "_bsln|_perf", ""))

animate_df




anim <- ggplot(animate_df, aes(factor(treat), spend)) + 
  geom_boxplot(fill = "dodgerblue") + 
  xlab("Treatment Assignment") + 
  ylab("Total Spend Amount") + 
  theme_economist() + 
  scale_y_continuous(labels = dollar) + 
  coord_flip() +
  # Here comes the gganimate code
  transition_states(
    period,
    transition_length = 1,
    state_length = 0.5
  ) +
  labs(title = 'Distribution of total spend by period: {closest_state}') + 
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out')

anim



anim_save("spend_by_period.gif", anim)

gganimate_save(anim, filename = "spend_by_period.gif", saver = NULL, fps = 10)





# Matching ----------------------------------------------------------------

synth_df %>% 
  glimpse()



formula1 <- f.build('treat', 
                   c(
                     'adm_count_bsln',
                     'pro_count_bsln',
                     'out_count_bsln',
                     'pdp_count_bsln',
                     'er_ipa_count_bsln',
                     'er_op_count_bsln'
                     )
                   )

M1 <- matchit(
  data = synth_df,
  formula = formula1,
  method = 'nearest',
  caliper = 0.2
)

M1

M1_smry <- summary(M1)
M1_smry$nn
M1_smry$sum.all

M1_smry$sum.matched
M1_smry$sum.matched$`Means Treated`
M1_smry$sum.matched$`Mean Diff`




match.data(M1)

bal.tab(M1)

love.plot(bal.tab(M1))

love.plot(M1, threshold = 0.1, stat = "ks.statistics")
love.plot(M1, threshold = 0.1, abs = TRUE, var.order = "adjusted")
love.plot(M1, threshold = 0.1, abs = TRUE, var.order = "adjusted", drop.distance = TRUE)

love.plot(M1, threshold = .1, 
          var.order = "unadjusted", line = TRUE)

love.plot(M1, 
          stats = c("mean.diffs", "ks.statistics"), 
          threshold = c(mean.diffs = .1, ks.statistics = .05), 
          size=4,
          binary = "std", abs = TRUE,
          var.order = "unadjusted", 
          #var.names = v,
          limits = c(0, 1), grid = FALSE, wrap = 20,
          sample.names = c("Unmatched", "Matched"),
          position = "top", shapes = c("diamond filled", "triangle"),
          colors = c("salmon", "dodgerblue"))



bal.plot(M1, var.name = "er_op_count_bsln")
bal.plot(M1, var.name = "pro_count_bsln")



# Single Regression -------------------------------------------------------

match_data <- match.data(M1)
match_data





gamma_model <- glm(formula = total_spend_perf ~ treat + distance, 
                 data = match_data, 
                 family = Gamma(link = "log"))

gamma_model
summary(gamma_model)
tidy(gamma_model)

# Treatment effect:
1/exp(tidy(gamma_model) %>% filter(term == 'treat') %>% select(estimate))
1/exp(-0.5969114)

exp(tidy(gamma_model) %>% filter(term == 'treat') %>% select(estimate)) - 1
exp(-0.5969114) - 1

100 * (exp(tidy(gamma_model) %>% filter(term == 'treat') %>% select(estimate)) - 1)

# Exponentiated coefficients
exp(coef(gamma_model))

# Goodness-of-fit test
gof.ts = gamma_model$deviance
gof.pvalue = 1 - pchisq(gof.ts, gamma_model$df.residual)
gof.pvalue
# Very small p-value, so...












# Poisson:

mean(match_data$pro_count_perf) # 7.54973
var(match_data$pro_count_perf) # 55.59709
# We see from above that the mean is not even 
# close to the variance for professional counts.

# Under a Poisson model, we would expect the means and variances 
# of the response to be about the same in various groups. 
# Without adjusting for overdispersion, we use incorrect, 
# artificially small standard errors leading to artificially 
# small p-values for model coefficients. We may also end up
# with artificially complex models.

# We can take overdispersion into account in several different ways.
# The simplest is to use an estimated dispersion factor to
# inflate standard errors. Another way is to use a
# negative-binomial regression model.

# To use a dispersion factor, we specify the model with family=quasipoisson
# and multiply (inflate) the standard errors by the square root
# of the dispersion parameter. The coefficients will be unchanged.




hist(match_data$pro_count_perf)
# Huge right skew here


poisson_model <- glm2(formula = pro_count_perf ~ treat + distance, 
     data = match_data, 
     family = poisson(link = "log"))

poisson_model
summary(poisson_model)

# exponentiated coefficients - this gives us the RATE RATIO 
# (a.k.a. relative risk):
exp(coef(poisson_model)[2]) # 1.008166 

0.008133 # - beta, or treatment coefficient, is the DIFFERENCE
# in the log-rate for treatment vs. control
# To get the rate RATIO, exponentiate the beta for treatment status:
exp(0.008133) # 1.008166



0.008133 # treat coefficient
0.009451 # treat SE

lower_ci <- exp(0.008133) - 1.96*(0.009451)
lower_ci

upper_ci <- exp(0.008133) + 1.96*(0.009451)
upper_ci

# Interpretation:
# Professional visits either increase by 2.67%
#   or decrease by 1.14% because of the program engagement.



# Exponentiating the endpoints yields a confidence interval
# for the relative risk:
0.008133 - 1.96*(0.009451) # -0.01039096
0.008133 + 1.96*(0.009451) # 0.02665696

exp(-0.01039096) # 0.9896628
exp(0.02665696) # 1.027015
# The above numbers mean we are 95% confident that the mean number
# of professional visits changes between -1.14% and +2.7% based
# on engaging in the program. Because this CI overlaps a percentage
# change of zero, the effect is not statistically signficant.








n <- nrow(match_data)
k <- 2
yhat <- predict(poisson_model, type="response")
z <- (match_data$pro_count_perf-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")

# Overdispersion ratio should be close to 1 if no dispersion present,
# larger than 1 if dispersion IS present.











# Zero-inflated Poisson:
zeroinfl(formula = drinks ~ off.campus + sex | firstYear, data = zip.data)

# exponentiated coefficients
exp(coef(zip.m2))













# Regression Examples -----------------------------------------------------



#https://purrr.tidyverse.org/reference/map.html
# A more realistic example: split a data frame into pieces, fit a
# model to each piece, summarise and extract R^2
mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_dbl("r.squared")

mtcars %>%
  split(.$cyl) %>%
  map(~ lm(mpg ~ wt, data = .x)) %>%
  map(summary) %>%
  map_df("coefficients")



lm_res <- lm(mpg ~ wt, data = mtcars)
lm_res$coefficients
lm_res$terms




glm_df <- match.data(M1) %>% 
  select(treat, contains("perf"), -contains("bsln"), distance)

glm_df %>% 
  head()



glm_model <- glm(formula = total_spend_perf ~ treat + distance, 
     data = glm_df, 
     family = Gamma(link = "log"))

glm_model

summary(glm_model)

tidy(glm_model)

# Treatment effect:
1/exp(tidy(glm_model) %>% filter(term == 'treat') %>% select(estimate))










glm_model <- glm(formula = adm_count_perf ~ treat + distance, 
                 data = glm_df, 
                 family = poisson(link = "log"))

#glm_model
#summary(glm_model)
tidy(glm_model)

# Treatment effect:
1/exp(tidy(glm_model) %>% filter(term == 'treat') %>% select(estimate))

exp(tidy(glm_model) %>% filter(term == 'treat') %>% select(estimate)) - 1
100 * (exp(tidy(glm_model) %>% filter(term == 'treat') %>% select(estimate)) - 1)












glm2(formula = total_spend_perf ~ treat + distance, 
    data = glm_df, 
    family = Gamma(link = "log"))


speedglm(formula = total_spend_perf ~ treat + distance, 
     data = glm_df, 
     family = Gamma(link = "log"))






# furrr example -----------------------------------------------------------



help(attrition)
?attrition
data("attrition")
names(attrition)
dim(attrition)


set.seed(4622)
rs_obj <- vfold_cv(attrition, v = 20, repeats = 100)
rs_obj


mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)


## splits will be the `rsplit` object with the 90/10 partition
holdout_results <- function(splits, ...) {
  # Fit the model to the 90%
  mod <- glm(..., data = analysis(splits), family = binomial)
  # Save the 10%
  holdout <- assessment(splits)
  # `augment` will save the predictions with the holdout data set
  res <- broom::augment(mod, newdata = holdout)
  # Class predictions on the assessment set from class probs
  lvls <- levels(holdout$Attrition)
  predictions <- factor(ifelse(res$.fitted > 0, lvls[2], lvls[1]),
                        levels = lvls)
  # Calculate whether the prediction was correct
  res$correct <- predictions == holdout$Attrition
  # Return the assessment data set with the additional columns
  res
}




# Sequential
tic()
rs_obj$results <- map(rs_obj$splits, holdout_results, mod_form)
toc()
#27.953 sec elapsed
rs_obj$results[[1]]





# Parallel
plan(multiprocess)

tic()
rs_obj$results <- future_map(rs_obj$splits, holdout_results, mod_form)
toc()
#10.315 sec elapsed
rs_obj$results[[1]]










# Bootstrap ---------------------------------------------------------------


rep_df <- synth_df %>% 
  rep_sample_n(size = nrow(synth_df), reps = 1000, replace = TRUE)


# Linear model

tic()
rep_df %>% 
  select(replicate, treat, total_spend_perf) %>% 
  split(.$replicate) %>%
  map(~ lm(total_spend_perf ~ treat, data = .x)) %>%
  map(summary) %>%
  map_df("coefficients")
toc()













# GLM


# Simple example:
rep_df %>% 
  filter(replicate == 1)

glm_mod1 <- glm2(formula = er_ipa_count_perf ~ treat, 
     data = rep_df %>% filter(replicate == 1), 
     family = poisson(link="log"))

glm_mod1
summary(glm_mod1)
tidy(glm_mod1)
glm_mod1$coefficients
glm_mod1$coefficients[[2]]



confint(glm_mod1)
exp(confint(glm_mod1))







grab_tx_coef <- function(mod){
  mod$coefficients[[2]]
}










# Using purrr:

tic()
map_df <- rep_df %>% 
  head(50000) %>% 
  select(replicate, treat, er_ipa_count_perf) %>% 
  split(.$replicate) %>%
  map(~ glm2(formula = er_ipa_count_perf ~ treat, data = .x, family = poisson(link="log"))) %>%
  map(grab_tx_coef)
toc()
#0.323 sec elapsed
mean(unlist(map_df))




tic()
map_df <- rep_df %>% 
  head(50000) %>% 
  select(replicate, treat, er_ipa_count_perf) %>% 
  split(.$replicate) %>%
  map(~ glm2(formula = er_ipa_count_perf ~ treat, data = .x, family = poisson(link="log"))) %>%
  map(tidy)
toc()
# 0.34 sec elapsed

map_df

map_df[[1]]
map_df[1]

unlist(map_df)

map_df %>% 
  filter(term == 'treat') %>% 
  select(estimate)

mean(unlist(map_df)) %>%
  map_df("coefficients")






mod_form <- as.formula(er_ipa_count_perf ~ treat)

train_and_tidy <- function(splits, ...) {
  # Fit the model
  mod <- glm2(..., data = splits, family = poisson(link="log"))
  
  # Grab coefficient for treatment variable only
  res <- mod$coefficients[[2]]
  # Grab tidied version of model results
  #res <- tidy(mod)
  
  # Return
  res
}



tic()
#rep_df %>% filter(replicate < 100)
results_df <- map(rep_df %>% split(.$replicate), train_and_tidy, mod_form, .progress = TRUE)
toc()
# 4.834 sec elapsed for 100
# 173.232 sec elapsed for 1000

results_df[[1]]





tic()
#rep_df %>% filter(replicate < 100)
results_df <- future_map(rep_df %>% split(.$replicate), train_and_tidy, mod_form, .progress = TRUE)
toc()
# 10.963 sec elapsed for 100
# 90.161 sec elapsed for 1000
results_df[[1]]


mean(unlist(results_df))

quantiles <- quantile(unlist(results_df), 
         probs = c(0.025, 0.500, 0.975))

quantiles
quantiles['2.5%']
quantiles['50%']
quantiles['97.5%']

unlisted <- tibble(unlist(results_df))
unlisted %<>% 
  rename(beta = `unlist(results_df)`)


unlisted %>% 
  ggplot(aes(x = beta)) + 
  geom_histogram(bins = 20, col = "white") + 
  ggtitle("Distribution of Beta Coefficients") + 
  theme_fivethirtyeight() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    text = element_text(size=20)
  ) +
  geom_vline(aes(xintercept = quantiles['2.5%']), col = "red", size = 2, linetype="dotted") + 
  geom_vline(aes(xintercept = quantiles['50%']), col = "black", size = 1) +   
  geom_vline(aes(xintercept = quantiles['97.5%']), col = "red", size = 2, linetype="dotted") + 
  geom_vline(aes(xintercept = 0), col = "skyblue", size = 2)















tic()
map_df <- rep_df %>% 
  head(5000000) %>% 
  select(replicate, treat, total_spend_perf) %>% 
  split(.$replicate) %>%
  map(~ glm2(formula = total_spend_perf ~ treat, data = .x, family = poisson(link="log"))) %>%
  map(summary) %>%
  map_df("coefficients")
toc()








# Using future and furrr:

plan(multiprocess)
tic()
future_df <- rep_df %>% 
  head(50000) %>% 
  select(replicate, treat, total_spend_perf) %>% 
  split(.$replicate) %>%
  future_map(~ glm2(formula = total_spend_perf ~ treat, data = .x, family = poisson(link="log"))) %>%
  future_map(summary) %>%
  map_df("coefficients")
toc()

#8.837 sec elapsed

future_df





plan(multiprocess)
tic()
future_df <- rep_df %>% 
  head(5000000) %>% 
  select(replicate, treat, total_spend_perf) %>% 
  split(.$replicate) %>%
  future_map(~ glm2(formula = total_spend_perf ~ treat, data = .x, family = poisson(link="log"))) %>%
  future_map(summary) %>%
  map_df("coefficients")
toc()

future_df







# GLM Interpretation ------------------------------------------------------

synth_df


# https://bookdown.org/roback/bookdown-bysh/ch-poissonreg.html#sec-overdispPois

# After finishing this chapter, you should be able to:
  
# Describe why simple linear regression is not ideal for Poisson data.
# Write out a Poisson regression model and identify the assumptions 
# for inference.
# Write out the likelihood for a Poisson regression and describe how 
# it could be used to estimate coefficients for a model.
# Interpret estimated coefficients from a Poisson regression and 
# construct confidence intervals for them.
# Use deviances for Poisson regression models to compare and 
# assess models.
# Use an offset to account for varying effort in data collection.
# Fit and use a zero-inflated Poisson (ZIP) model.


# This outputs the probability of y events given λ
dpois(1, 5)
dpois(3, 5)
dpois(5, 5)
dpois(10, 5)

# A small town’s police department issues 5 speeding tickets
# per month on average. Using a Poisson random variable,
# what is the likelihood that the police department issues
# 3 or fewer tickets in one month?
sum(dpois(0:3, lambda = 5))   
ppois(3, 5)








# gamma
# Once again consider a Poisson process. When discussing 
# exponential random variables, we modeled the wait time 
# before one event occurred. If Y represents the wait time
# before r events occur in a Poisson process with rate λ, Y 
# follows a gamma distribution.

# Two friends are out fishing. On average they catch one 
# fish an hour, and their goal is to catch 5 fish. 
# What is the probability that they take less than 3 
# hours to reach their goal?

#Using a gamma random variable, we set r=5 and λ=1. 
pgamma(3, shape = 5, rate = 1)
# There is a 18.5% chance of catching 5 fish within
# the first 3 hours.






# Normal (Gaussian)
# The weight of a box of Fruity Tootie cereal is 
# approximately normally distributed with an average 
# weight of 15 ounces and a standard deviation of 0.5 ounces.
# What is the probability that the weight of a randomly selected 
# box is more than 15.5 ounces?
pnorm(15.5, mean = 15, sd = 0.5, lower.tail = FALSE)










# https://bookdown.org/roback/bookdown-bysh/ch-poissonreg.html#sec-overdispPois

# Much like OLS, using Poisson regression to make inferences
# requires model assumptions.

# Poisson Response 
#   The response variable is a count per unit of time or space,
#   described by a Poisson distribution.
# Independence 
#   The observations must be independent of one another.
# Mean=Variance 
#   By definition, the mean of a Poisson random variable must be
#   equal to its variance.
# Linearity 
#   The log of the mean rate, log(λ), must be a linear function of x.

# For Poisson random variables, the variance of Y
# (i.e., the square of the standard deviation of Y),
# is equal to its mean, where Y represents the size of 
# an individual household. As the mean increases,
# the variance increases. So, if the response is a 
# count and the mean and variance are approximately
# equal for each group of X, a Poisson regression model 
# may be a good choice.



# These results suggest that by exponentiating the coefficient 
# on age we obtain the multiplicative factor by which the mean 
# count changes. In this case, the mean number in the house
# changes by a factor of e−0.0047=0.995 or decreases by 0.5%
# with each additional year older the household head is;
# or, we predict a 0.47% increase in mean household size 
# for a 1 year decrease in age of the household head 
# (1/.995=1.0047). The quantity on the left hand side of 
# Equation (4.2) is referred to as a rate ratio or relative 
# risk, and it represents a percent change in the response 
# for a unit change in X. In fact, for regression models in 
# general, whenever a variable (response or explanatory) is
# logged, we make interpretations about multiplicative 
# effects on that variable, while with unlogged variables
# we can reach our usual interpretations about additive effects.

# Typically the standard errors for the estimated coefficients are
# included in Poisson regression output. Here the standard error 
# for the estimated coefficient for age is 0.00094. We can use the
# standard error to construct a confidence interval for β1.
# A 95% CI provides a range of plausible values for the age 
# coefficient and can be constructed: 
#   ^β1−Z∗⋅SE(^β1),^β1+Z∗⋅SE(^β1)
# −0.0047−1.96∗0.00094,−0.0047+1.96∗0.00094 (−0.0065,−0.0029).

# Exponentiating the endpoints yields a confidence interval for
# the relative risk; i.e., the percent change in household size
# for each additional year older. 
# Thus (e−0.0065,e−0.0029)=(0.993,0.997) suggests that we 
# are 95% confident that the mean number in the house decreases 
# between 0.7% and 0.3% for each additional year older the 
# head of household is.

# It is best to construct a confidence interval for the coefficient 
# and then exponentiate the endpoints because the estimated 
# coefficients more closely follow a normal distribution than 
# the exponentiated coefficients. There are other approaches
# to constructing intervals in these circumstances, including 
# profile likelihood, the delta method, and bootstrapping.

# Another way to test the significance of the age term is to
# calculate a Wald-type statistic. A Wald-type test statistic 
# is the estimated coefficient divided by its standard error.




# CI for betas using profile likelihood
confint(modela)

exp(confint(modela))



# There is another way in which to assess how useful age is in 
# the model. A deviance is a way in which to measure how the
# observed data deviates from the model predictions. Tt is similar
# to sum of squared errors (unexplained variability in the response)
# in OLS regression. Because we want models that minimize deviance, 
# we calculate the drop-in-deviance when adding age to the model
# with no covariates (the null model).
# 
# If the null model were true, we would expect the drop-in-deviance
# to follow a χ2 distribution with 1 df. Therefore the p-value
# for comparing the null model to the model with age is found by
# determining the probability that the value for a χ2 random
# variable with one degree of freedom exceeds 25.4, which is
# essentially 0.




# p-value for test comparing the null and first order models
# drop.in.dev <- modela$null.deviance - modela$deviance; drop.in.dev
# diff.in.df <- modela$df.null - modela$df.residual; diff.in.df
# 1-pchisq(drop.in.dev, diff.in.df)
model0 = glm(total~1,family=poisson,data=fHH1)
anova(model0, modela, test = "Chisq")



# GOF test comparing deviance to residual degrees of freedom.
# A value close to 0 means evidence of lack-of-fit:
1-pchisq(modela2$deviance, modela2$df.residual)

# There are several reasons why lack-of-fit may be observed.
# We may be missing important covariates or interactions; 
# a more comprehensive data set may be needed. There may be 
# extreme observations that may cause the deviance to be 
# larger than expected; however, our residual plots did not 
# reveal any unusual points. Lastly, there may be a problem 
# with the Poisson model. In particular, the Poisson model 
# has only a single parameter, λ, for each combination of the 
# levels of the predictors which must describe both the mean 
# and the variance. This limitation can become manifest when 
# the variance appears to be larger than the corresponding means. 
# In that case, the response is more variable than the Poisson 
# model would imply, and the response is considered to be overdispered.

# Interpreting coefficients:
# PoissonRegression: e^(β1)= percent change in λ for unit change in X

# While a Poisson regression model is a good first choice because 
# the responses are counts per year, it is important to note that 
# the counts are not directly comparable because they come from
# different size schools. This issue sometimes is referred to 
# as the need to account for sampling effort; in other words, 
# we expect schools with more students to have more reports of 
# violent crime since there are more students who could be affected.
# 
# We can take the differences in enrollments into account by
# including an offset in our model.

# Here is an example of a model with an offset AND an interaction effect:
glm(formula = nv ~ type + region + region:type, family = poisson, 
    data = c.data, offset = log(enroll1000))


# In the presence of overdispersion or lack-of-fit, some options
# would be the quasipoisson model and negative binomial.




















# Gelman and Hill text:

# The coefficients β can be exponentiated and treated as
# multiplicative effects. For example, suppose the traffic
# accident model is
# yi ∼ Poisson(exp(2.8 + 0.012Xi1 − 0.20Xi2)),
# where Xi1 is average speed (in miles per hour, or mph) on the
# nearby streets and Xi2 = 1 if the intersection has a traffic
# signal or 0 otherwise. We can then interpret each coefficient
# as follows:
# 
# The constant term gives the intercept of the regression, that is,
# the prediction if Xi1 = 0 and Xi2 = 0.
# Since this is not possible (no street will have an average speed of 0),
# we will not try to interpret the constant term.
# 
# The coefficient of Xi1 is the expected difference in y
# (on the logarithmic scale) for each additional mph of traffic speed.
# Thus, the expected multiplicative increase is e0.012 = 1.012,
# or a 1.2% positive difference in the rate of traffic accidents per
# mph. Since traffic speeds vary by tens of mph, it would actually
# make sense to define Xi1 as speed in tens of mph, in which case
# its coefficient would be 0.12, corresponding to a 12% increase
# (more precisely, e0.12 = 1.127: a 12.7% increase) in accident
# rate per ten mph.
# 
# The coefficient of Xi2 tells us that the predictive difference
# of having a traffic signal can be found be multiplying the
# accident rate by exp(−0.20) = 0.82 yielding a reduction of 18%.




# It is usually a good idea to add a parameter to this model to 
# capture overdispersion, that is, variation in the data beyond 
# what would be predicted from the Poisson distribution alone.

# In most applications of Poisson regression, the counts can be 
# interpreted relative to some baseline or “exposure,” for example,
# the number of vehicles that travel through the intersection.
# In the general Poisson regression model, we think of yi as the 
# number of cases in a process with rate θi and exposure ui.
# The logarithm of the exposure, log(ui), is called the offset 
# in generalized linear model terminology.
# 
# Poisson regressions do not supply an independent variance
# parameter σ, and as a result can be overdispersed and usually are.
# 
# Under the Poisson distribution the variance equals 
# the mean—that is, the standard deviation equals the square
# root of the mean.
# 
# If the Poisson model is true, then the standardized residuals (zi’s)
# should be approximately independent (not exactly independent, 
# since the same estimate ˆ β is used in computing all of them),
# each with mean 0 and standard deviation 1. If there is overdispersion,
# however, we would expect the standardized residuals (zi’s) to
# be larger, in absolute value, reflecting the extra variation
# beyond what is predicted under the Poisson model.

# We can test for overdispersion in classical Poisson regression
# by computing the sum of squares of the n standardized residuals,
# and comparing this to the chi-squared n−k distribution, which is
# what we would expect under the model (using n−k rather than n 
# degrees of freedom to account for the estimation of k regression
# coefficients).

yhat <- predict (glm.police, type="response")
z <- (stops-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(n-k), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), n-k), "\n")

# More simply, we can fit an overdispersed model using the
# quasipoisson family:
glm(formula = stops ~ factor(eth) + factor(precinct), family=quasipoisson, 
    offset=log(arrests))


# A specific model commonly used in this scenario is the so-called 
# negative-binomial distribution:
#   yi ∼ Negative-binomial (mean = ui exp(Xiβ), overdispersion = ω).
# Unfortunately, the negative-binomial distribution is conventionally 
# expressed not based on its mean and overdispersion but rather in 
# terms of parameters a and b, where the mean of the distribution 
# is a/b and the overdispersion is 1 + 1/b. One must check the
# parameterization when fitting such models, and it can be helpful to
# double-check by simulating datasets from the fitted model and checking
# that they look like the actual data.

# In the usual regression context, predictive inference relates
# to comparisons between units, whereas causal inference addresses 
# comparisons of different treatments if applied to the same units.

# More generally, causal inference can be viewed as a special case of
# prediction in which the goal is to predict what would have happened
# under different treatment options.

# Causal interpretations of regression coefficients can only be 
# justified by relying on much stricter assumptions than are needed
# for predictive inference.

# In general, then, causal effects can be estimated using regression
# if the model includes all confounding covariates 
# (predictors that can affect treatment assignment or the outcome) 
# and if the model is correct. If the confounding covariates are all 
# observed (as in this example), then accurate estimation comes down 
# to proper modeling and the extent to which the model is forced to
# extrapolate beyond the support of the data. If the confounding 
# covariates are not observed (for example, if we suspect that 
# healthier patients received the treatment, but no accurate measure 
# of previous health status is included in the model), 
# then they are “omitted” or “lurking” variables that complicate 
# the quest to estimate causal effects.






# Situations arise when there are not enough controls in the
# overlapping region to fully provide one match per treated unit.
# In this case it can help to use some control observations as matches 
# for more than one treated unit. This approach is often called
# matching with replacement, a term which commonly refers to with 
# one-to-one matching but could generalize to multiple control matches
# for each control. Such strategies can create better balance, 
# which should yield estimates that are closer to the truth on 
# average. Once such data are incorporated into a regression, however, 
# the multiple matches reduce to single data points, which suggests
# that matching with replacement has limitations as a general strategy.

# A limitation of one-to-one matching is that it may end up 
# “throwing away” many informative units if the control group is
# substantially bigger than the treatment group. One way to make
# better use of the full sample is simply to subclassify based on
# values of the propensity score—perhaps discarding some noncomparable
# units in the tails of the propensity score distribution. 
# Then separate analyses can be performed within each subclass
# (for example, difference in outcome averages across treatment 
# groups or linear regressions of the outcome on an indicator
# variable for treatment and other covariates).





# More simply, propensity scores can be used in a regression of
# the outcome on the treatment and the scores rather than the full
# set of covariates. However, if observations that lie in areas where
# there is no overlap across treatment groups are not removed, the
# same problems regarding model extrapolation will persist. Also, this 
# method once again places a great deal of faith in precise and 
# correct estimation of the propensity score.











