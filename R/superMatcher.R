library(pacman)
library(tidyverse)
library(magrittr)

p_load(docstring, cobalt, MatchIt, progress, broom, glm2, scales, data.table, stddiff)


calculate_sample_size <- function(df, group_var = 'mbr_sta', outcome_var = 'adm_cnt_perf', power = 0.8, sig.level = 0.05){
  #' Sample size calculations
  #'
  #' This function is used to calculate the sample size needed for both groups to be able to perform analyses at a
  #' given statistical power level and statistical significance level.
  #'
  #' @param df The dataset to be used
  #' @param group_bar The variable indicating group membership (defaults to 'mbr_sta')
  #' @param outcome_var The variable related to outcome being analyzed (defaults to adm_cnt_perf)
  #' @param power The statistical power level desired (defaults to 0.8)
  #' @param sig.level The statistical significance level desired (defaults to 95%, or alpha=0.05)
  #' 
  #' @examples
  #' calculate_sample_size(df = AH_FOLLOWUP_PREP, group_var = 'Mbr_sta', outcome_var = 'adm_cnt_perf)

  group_var_sym <- ensym(group_var)
  outcome_var_enquo <- enquo(outcome_var)
  
  mean_a <- mean(as.numeric(flatten(df %>% na.omit() %>% filter(!!group_var_sym == 0) %>% select(!!outcome_var_enquo))))
  mean_b <- mean(as.numeric(flatten(df %>% na.omit() %>% filter(!!group_var_sym == 1) %>% select(!!outcome_var_enquo))))
  sd_a <- sd(as.numeric(flatten(df %>% na.omit() %>% filter(!!group_var_sym == 0) %>% select(!!outcome_var_enquo))))
  sd_b <- sd(as.numeric(flatten(df %>% na.omit() %>% filter(!!group_var_sym == 1) %>% select(!!outcome_var_enquo))))
  
  pooled_sd <- sqrt(mean(sd_a, sd_b))
  d <- (mean_a - mean_b)/pooled_sd
  results <- pwr.t.test(d=d, power=power, sig.level=sig.level, type="two.sample", alternative="two.sided")
  
  return(results)
}


power_analysis <- function(df, outcome_vars){
  #' Sample size calculations
  #'
  #' This function is used to calculate the sample size needed for both groups to be able to perform analyses at a
  #' given statistical power level and statistical significance level.
  #'
  #' @param df The dataset to be used
  #' @param outcome_vars The list of outcome variables being tracked in the analysis
  #' @param power The statistical power level desired (defaults to 0.8)
  #' @param sig.level The statistical significance level desired (defaults to 95%, or alpha=0.05)
  #' 
  #' @examples
  #' power_analysis(df = bhccei, outcome_vars)
  
  for(var in outcome_vars){
    try({
      result <- calculate_sample_size(df = bhccei, group_var = 'mbr_sta', outcome_var = var, power = power, sig.level = sig.level)
      print(paste("Number of members needed for", var, "is:", ceiling(result$n)))
    },
    silent = TRUE
    )
  }
}


calculate_imbalance <- function(raw_data, matched_data, treat_var = 'mbr_status', variable_list){
  #' Calculate imbalance
  #'
  #' This function is used to calculate imbalance between treated and
  #' control groups between pre-match and post-match for all baseline variables
  #'
  #' @param raw_data The raw dataframe before matching
  #' @param matched_data The matched data
  #' @param treat_var Treatment indicator variable
  #' @param variable_list List of variables we want to balance on in baseline
  #' 
  #' @examples
  #' calculate_imbalance(my_data, matched_data, balance_vars)
  
  require(stddiff)
  
  before <- stddiff.numeric(raw_data, treat_var, variable_list) %>% 
    as.data.frame(row.names = variable_list) %>% 
    select(stddiff)
  
  after <- stddiff.numeric(matched_data, treat_var, variable_list) %>% 
    as.data.frame(row.names = variable_list) %>% 
    select(stddiff)
  
  improvement <- data.frame(before, after) %>% 
    rename(
      before = stddiff,
      after = stddiff.1) %>% 
    mutate(
      improvement = after - before,
      rownames = variable_list
    ) %>% 
    column_to_rownames('rownames') %>% 
    rownames_to_column(var = 'varname')
  
  return(improvement)
}


SRS <- function(pair_ids, 
                match_data_engaged, 
                match_data_control,
                seed_number){
  #' Simple Random Sample
  #'
  #' Takes match data and match pair numbers and creates a simple random
  #' sample of the same number of rows of the matched data
  #'
  #' @param pair_ids unique match pair numbers
  #' @param match_data_engaged Data frame of engaged members who were matched
  #' @param match_data_control Data frame of control members who were matched
  #' @param seed The seed used for the random number generator
  #' @usage SRS(pair_ids, match_data_engaged, match_data_control)
  #' @return Simple random sample of matched data
  #' @details Details about input or anything else
  #' @note And here is a note. Isn't it nice?
  #' @references None
  #' @examples
  #' SRS(pair_ids, match_data_engaged, match_data_control)
  
  set.seed(seed_number)
  
  sample <- sample(pair_ids,length(pair_ids),replace = TRUE)
  SRS_data <- match_data_engaged[sample,] %>% 
    rbind(match_data_control[sample,]) %>%
    na.omit()
  
  return(SRS_data)
}


spend_util_modeling <- function(var,
                                df = SRS_data,
                                cost_vars, 
                                utilization_vars, 
                                treatment_indicator = 'mbr_sta',
                                distance_var = 'distance', 
                                maxiters = 5000,
                                iter){
  #' Simple Random Sample
  #'
  #' Takes match data and match pair numbers and creates a simple random
  #' sample of the same number of rows of the matched data
  #'
  #' @param var Outcome variable to serve as dependent variable in regressions
  #' @param df Data frame of simple random sample data (defaults to SRS_data)
  #' @param cost_vars List of cost-related outcome variables. Defined earlier in script
  #' @param utilization_vars List of utilization- or count-related variables. Defined earlier in script
  #' @param treatment_indicator Treatment status indicator variable (defaults to 'mbr_sta')
  #' @param distance_var Distance measure variable
  #' @param maxiters Maximum number of iterations to run the regressions
  #' @param iter Which iteration of the bootstrap we are in
  #' @usage SRS(pair_ids, match_data_engaged, match_data_control)
  #' @return Simple random sample of matched data
  #' @details Details about input or anything else
  #' @note And here is a note. Isn't it nice?
  #' @references None
  #' @examples
  #' spend_util_modeling(var, df = SRS_data, iter = 5)
  #' spend_util_modeling(var, df = SRS_data, cost_vars, utilization_vars, treatment_indicator = 'mbr_sta', 
  #' distance_var = 'distance', iter = 5)
  
  var_quo <- enquo(var)
  
  formula <- f.build(var, c(treatment_indicator, distance_var))
  
  try(
    if(var %in% cost_vars){
      model <- glm2(formula, 
                    family = Gamma(link = "log"),
                    data = df,
                    maxit = maxiters)
      results <- as.data.frame(tidy(model) %>% 
                                 mutate(
                                   aic = model$aic,
                                   nd = model$null.deviance,
                                   converged = model$converged,
                                   model_iters = model$iter, 
                                   boot_iter = iter,
                                   outcome_var = var
                                 )
      ) %>% 
        select(term, everything())
      return(results)
    }else if(var %in% utilization_vars){
      model <- glm2(formula, 
                    family = poisson(link = "log"), 
                    data = df)
      results <- as.data.frame(tidy(model) %>% 
                                 mutate(
                                   aic = model$aic,
                                   nd = model$null.deviance,
                                   converged = model$converged,
                                   model_iters = model$iter, 
                                   boot_iter = iter,
                                   outcome_var = var
                                 )
      ) %>% 
        select(term, everything())
      return(results)
    }
    , silent = TRUE
  )
}


bootstrap_tfx2 <- function(match_data, 
                           treatment_indicator = 'mbr_sta',
                           distance_var = 'distance',
                           identifier = 'mid',
                           cost_vars, 
                           utilization_vars,
                           balance_vars,
                           outcome_vars,
                           k = 1000, 
                           max_iters = 500){
  #' Bootstrap treatment effect estimation procedure
  #'
  #' This function is used to take the matched dataset and bootstrap the 
  #' treatment effect estimate by taking k simple random samples with replacement
  #' and running regression models to obtain the coefficient for treatment status.
  #'
  #' @param match_data The matched dataset. Can be from call to MatchIt() or from other match.
  #' @param treatment_indicator Treatment indicator variable. Defaults to 'mbr_status'
  #' @param distance_var Propensity score scalar
  #' @param identifier Variable to identify members uniquely
  #' @param cost_vars List of variables related to spend
  #' @param utilization_vars List of variables related to counts or visits
  #' @param balance_vars List of variables to balance in baseline
  #' @param outcome_vars List of variables to measure in performance in regression models
  #' @param k Number of iterations to run. Defaults to 1,000
  #' 
  #' @examples
  #' bootstrap_tfx2(match_data, cost_vars, utilization_vars, balance_vars, outcome_vars)
  #' bootstrap_tfx2(match.data(M1), cost_vars, utilization_vars, balance_vars, outcome_vars)
  
  # Setup
  treat_enquo <- enquo(treatment_indicator)
  treat_quo <- quote(treatment_indicator)
  
  boot_data <- match_data %>% select(!!treat_enquo, pair_number, distance, balance_vars, outcome_vars)
  match_data_engaged <- boot_data %>% 
    filter(!!sym(treatment_indicator) == 1) %>% 
    column_to_rownames(var = "pair_number")
  match_data_control <- boot_data %>% 
    filter(!!sym(treatment_indicator) == 0) %>%  
    column_to_rownames(var = "pair_number")
  pair_ids <- unique(match_data$pair_number)
  
  # Begin bootstrap
  group_means <- vector("list", length = k)
  treatment_fx <- vector("list", length = k)
  pb <- progress_bar$new(total = k)
  for(i in 1:k){
    pb$tick()
    SRS_data <- SRS(pair_ids, match_data_engaged, match_data_control, seed_number = i)
    group_means[[i]] <- bootstrap_group_means(SRS_data, balance_vars = balance_vars, outcome_vars = outcome_vars, iteration_number = i)
    rowlist <- vector("list", length = length(outcome_vars))
    for(j in seq_along(outcome_vars)){
      spend_util_modeling(var = outcome_vars[j], df = SRS_data, cost_vars, utilization_vars, 
                          treatment_indicator = 'mbr_sta', distance_var = 'distance', maxiters = max_iters, iter = i)
    }
    treatment_fx[[i]] <- do.call(rbind,rowlist)
  }
  group_means_all <- do.call(rbind,group_means)
  treatment_fx_all <- do.call(rbind,treatment_fx)
  return(list(group_means_all, treatment_fx_all))
}


bootstrap_tfx <- function(match_data, 
                          treatment_indicator = 'mbr_status',
                          distance_var = 'distance', 
                          identifier = 'mid',
                          cost_vars,
                          utilization_vars,
                          balance_vars,
                          outcome_vars, 
                          group_mean_vars,
                          k = 1000){
  #' Bootstrap treatment effect estimation procedure
  #'
  #' This function is used to take the matched dataset and bootstrap the 
  #' treatment effect estimate by taking k simple random samples with replacement
  #' and running regression models to obtain the coefficient for treatment status.
  #'
  #' @param match_data The matched dataset. Can be from call to MatchIt() or from other match.
  #' @param treatment_indicator Treatment indicator variable. Defaults to 'mbr_status'
  #' @param distance_var Propensity score scalar
  #' @param identifier Variable to identify members uniquely
  #' @param spend_vars List of variables related to spend
  #' @param count_vars List of variables related to counts or visits
  #' @param balance_vars List of variables to balance in baseline
  #' @param outcome_vars List of variables to measure in performance in regression models
  #' @param k Number of iterations to run. Defaults to 1,000
  #' 
  #' @examples
  #' bootstrap_tfx(match_data, cost_vars, utilization_vars, balance_vars, outcome_vars)
  #' bootstrap_tfx(match.data(M1), cost_vars, utilization_vars, balance_vars, outcome_vars)
  
  treat_enquo <- enquo(treatment_indicator)
  
  ## Prep
  boot_data <- match_data %>% 
    select(!!treat_enquo, pair_number, distance, balance_vars, outcome_vars, group_mean_vars)
  match_data_engaged <- boot_data %>% 
    filter(!!sym(treatment_indicator) == 1)
  rownames(match_data_engaged) <- match_data_engaged$pair_number
  match_data_control <- boot_data %>% 
    filter(!!sym(treatment_indicator) == 0)
  rownames(match_data_control) <- match_data_control$pair_number
  pair_ids <- unique(match_data$pair_number)
  ## Initialize vectors instead of "growing" them, which is slow
  group_means <- vector("list", length = k)
  treatment_fx <- vector("list", length = k)
  pb <- progress_bar$new(total = k)
  for(i in 1:k){
    pb$tick()
    sample <- sample(pair_ids,length(pair_ids),replace = TRUE)
    SRS_data <- match_data_engaged[sample,] %>% rbind(match_data_control[sample,]) %>% na.omit()
    mean_sd <- SRS_data %>% 
      select(!!treat_enquo, group_mean_vars) %>% 
      group_by(!!sym(treatment_indicator)) %>% 
      summarise_all(list(mean=mean,sd=sd)) %>% 
      ungroup() %>% 
      mutate(iteration = i)
    group_means[[i]] <- mean_sd
    rowlist <- vector("list", length = length(outcome_vars))
    for(j in seq_along(outcome_vars)){
      try(
        if(outcome_vars[j] != treatment_indicator){
          dependent <- outcome_vars[j]
          if(dependent %in% cost_vars){
            spend_formula <- f.build(dependent, c(treatment_indicator, distance_var))
            model <- glm2(spend_formula, 
                          family = Gamma(link = "log"),
                          data = SRS_data,
                          maxit = 5000)
          }else if(dependent %in% utilization_vars){
            count_formula <- f.build(dependent, c(treatment_indicator, distance_var))
            model <- glm2(count_formula, 
                          family = poisson(link = "log"), 
                          data = SRS_data)
          }
          coef <- as.data.frame(tidy(model))
          results <- coef %>%
            mutate(outcome_var = dependent) %>%
            select(term, everything())
          results$iteration <- i
          # Add to accumulator
          rowlist[[j]] <- results
        }
        , silent = TRUE)
    }
    rowlist <- do.call(rbind,rowlist)
    treatment_fx[[i]] <- rowlist
  }
  group_means_all <- do.call(rbind,group_means)
  treatment_fx_all <- do.call(rbind,treatment_fx)
  
  return(list(group_means_all, treatment_fx_all))
}


group_mean_summarizer <- function(group_means_df, treatment_indicator = 'mbr_status'){
  #' Calculate the group means after bootstrap procedure
  #'
  #' This function takes the bootstrap data for the group means and aggregates
  #' across all iterations to summarize the group means for all SRS datasets
  #'
  #' @param group_means_df The group_means dataset from the bootstrap results.
  #' @param treatment_indicator Treatment indicator variable. Defaults to 'mbr_status'
  #' 
  #' @examples
  #' group_mean_summarizer(group_means, tfx, treatment_indicator = 'mbr_sta')
  
  treat_enquo <- enquo(treatment_indicator)
  
  gm <- group_means_df %>% 
    select(iteration, !!treat_enquo, contains("_mean")) %>% 
    gather(contains("_mean"), key = 'mean_var', value = 'mean') %>% 
    mutate(var = str_replace(mean_var, "_mean", "")) %>% 
    select(var, iteration, !!treat_enquo, mean) %>% 
    group_by(var) %>% 
    summarise(mean = mean(mean, na.rm = TRUE)) %>% 
    mutate(
      period = ifelse(var %like% "_bsln","Baseline_Mean","Performance_Mean"),
      new_var = str_replace(var, "_bsln", ""),
      new_var = str_replace(new_var, "_perf", "")
    ) %>% 
    select(-var) %>% 
    spread(key = 'period', value = 'mean') %>% 
    rename(outcome_var = new_var)
  
  return(gm)
}


tfx_summarizer <- function(tfx_df, treatment_indicator = 'mbr_status'){
  #' Calculate the average beta coefficients
  #'
  #' This function takes the bootstrap data for the regression coefficients
  #' and aggregates across all iterations to return the average beta coefficient 
  #' per outcome variable. It uses the percentile approach for calculating the
  #' 95% confidence interval.
  #'
  #' @param tfx_df The treatment effects (tfx) dataset from the bootstrap results.
  #' @param treatment_indicator Treatment indicator variable. Defaults to 'mbr_status'
  #' 
  #' @examples
  #' tfx_summarizer(tfx, treatment_indicator = 'mbr_sta')
  
  treat_enquo <- enquo(treatment_indicator)
  
  tfx <- tfx_df %>% 
    filter(term == !!treat_enquo) %>% 
    group_by(outcome_var) %>% 
    summarise(
      mean_beta = mean(estimate, na.rm = TRUE),
      median_beta = median(estimate, na.rm = TRUE),
      sd_beta = sd(estimate),
      ci_lower = mean_beta - (1.962 * sd_beta),
      ci_upper = mean_beta + (1.962 * sd_beta)
    ) %>% 
    mutate_if(is.numeric,round,3) %>% 
    mutate(outcome_var = str_replace(outcome_var,"_perf",""))
  
  return(tfx)
}


gm_tfx_merger <- function(group_means_df, tfx_df, cost_vars, utilization_vars, treatment_indicator = 'mbr_status'){
  #' Merge the group means and treatment effects summary dataframes together
  #'
  #' This function takes the group means summary and treatment effects summary
  #' data frames and returns a dataframe that can be used to construct the confidence
  #' interval plots and final results summary tables.
  #'
  #' @param group_means_df The group means (gm) dataset from the bootstrap results.
  #' @param tfx_df The treatment effects (tfx) dataset from the bootstrap results.
  #' @param cost_vars The list of cost-related outcome variables.
  #' @param utilization_vars The list of utilization-related outcome variables.
  #' @param treatment_indicator Treatment indicator variable. Defaults to 'mbr_status'
  #' 
  #' @examples
  #' gm_tfx_merger(group_means, tfx, cost_vars = cost_vars, utilization_vars = utilization_vars, treatment_indicator = 'mbr_sta')
  
  treat_quo <- enquo(treatment_indicator)
  
  gm_summary <- group_mean_summarizer(group_means_df, treatment_indicator = treatment_indicator)
  
  tfx_summary <- tfx_summarizer(tfx_df, treatment_indicator = treatment_indicator)
  
  merged_df <- merge(gm_summary, tfx_summary, by.x = 'outcome_var', by.y = 'outcome_var') 
  
  transformed_df <- merged_df %>% 
    mutate(
      transformed_beta = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, exp(mean_beta) - 1, exp(mean_beta)),
      ci_lower_pct = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, exp(ci_lower) - 1, exp(ci_lower) / Baseline_Mean),
      percent_change = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, round(transformed_beta, 3), transformed_beta / Baseline_Mean),
      ci_upper_pct = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, exp(ci_upper) - 1, exp(ci_upper) / Baseline_Mean),
      ci_lower_actual = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, (exp(ci_lower) - 1) * Baseline_Mean, exp(ci_lower)),
      ci_mean_actual = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, transformed_beta * Baseline_Mean, transformed_beta),
      ci_upper_actual = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, (exp(ci_upper) - 1) * Baseline_Mean, exp(ci_upper)),
      tx_fx_est = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, transformed_beta * Baseline_Mean, transformed_beta)
    )
  
  labels_df <- transformed_df %>% 
    mutate_if(is.numeric, round, 2) %>% 
    select(outcome_var, Baseline_Mean, sd_beta, ci_lower_actual, ci_mean_actual, ci_upper_actual, 
           tx_fx_est, transformed_beta, percent_change) %>% 
    mutate(
      lower_color = ifelse(ci_lower_actual < 0, "green", "red"),
      mean_color = ifelse(transformed_beta < 0, "green", "red"),
      upper_color = ifelse(ci_upper_actual < 0, "green", "red")
    ) %>% 
    mutate(
      mean_lbl = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, dollar(ci_mean_actual, negative_parens = FALSE), ci_mean_actual),
      ci_lower_actual_lbl = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, dollar(ci_lower_actual, negative_parens = FALSE), ci_lower_actual),
      ci_mean_actual_lbl = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, dollar(ci_mean_actual, negative_parens = FALSE), ci_mean_actual),
      ci_upper_actual_lbl = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, dollar(ci_upper_actual, negative_parens = FALSE), ci_upper_actual),
      tx_fx_est_lbl = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, dollar(tx_fx_est, negative_parens = FALSE), tx_fx_est),
      percent_change = percent(percent_change, big.mark = ",")
    )
  
  return(labels_df)
}


format_dollars <- function(df){
  #' Dollar Formatter
  #'
  #' This function is used to format the final results dataframe with dollar signs for the spend variables and
  #' appropriate rounding for the count variables.
  #'
  #' @param df The final results dataframe from the output of the bootstrap step.
  #' 
  #' @examples
  #' format_dollars(final_df)
  
  df %>% 
    select(outcome_var, Baseline_Mean, Performance_Mean, mean_beta, sd_beta, ci_lower_actual, ci_upper_actual, tx_fx_est, transformed_beta, percent_change) %>% 
    filter(outcome_var %like% "spend") %>% 
    mutate_at(vars(Baseline_Mean, Performance_Mean, ci_lower_actual, ci_upper_actual, tx_fx_est), dollar_format(negative_parens = FALSE)) %>% 
    mutate(
      percent_change = percent(percent_change, big.mark = ",")
    ) %>% 
    rbind(
      df %>% 
        select(outcome_var, Baseline_Mean, Performance_Mean, mean_beta, sd_beta, ci_lower_actual, ci_upper_actual, tx_fx_est, transformed_beta, percent_change) %>% 
        filter(grepl(paste(c("count","cnt"), collapse="|"), outcome_var)) %>% 
        mutate_if(is.numeric,round,2) %>% 
        mutate(
          percent_change = percent(percent_change, big.mark = ",")
        )
    )
}


CI_plot <- function(merged_summary_df, var_label, spend_util_indicator = "cost", dollar_formatting = FALSE){
  #' Confidence Interval Plot
  #'
  #' This function is used to format the final results dataframe with dollar signs for the spend variables and
  #' appropriate rounding for the count variables.
  #'
  #' @param merged_summary_df The final results dataframe from the output of the bootstrap step.
  #' @param spend_util_indicator X axis to be used in the plot.
  #' @param var_type Variable type. Defaults to "cost".
  #' @param dollar_formatting Boolean value for creating a scale_y_continuous with dollar labels for cost
  #' 
  #' @examples
  #' CI_plot(merged_summary, var_label = "Total Count", spend_util_indicator = "utilization")
  #' CI_plot(merged_summary, var_label = "Total net eligible amount (U.S. Dollars)", spend_util_indicator = "cost", dollar_formatting = TRUE)
  
  spend_util_indicator_enquo <- enquo(spend_util_indicator)
  
  CI_plot_df <- merged_summary_df %>% 
    mutate(var_type = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, 'cost', 'utilization')) %>% 
    select(outcome_var, var_type, Baseline_Mean, transformed_beta, 
           ci_lower_actual, ci_mean_actual, ci_upper_actual, ci_lower_actual_lbl, ci_upper_actual_lbl,
           tx_fx_est, percent_change, lower_color, upper_color) %>% 
    filter(var_type == !!spend_util_indicator_enquo)
  
  p <- ggplot(CI_plot_df) +
    geom_segment(aes(x=outcome_var, xend=outcome_var, y=ci_lower_actual, yend=ci_upper_actual), color="pink", size=3) +
    geom_point(aes(x=outcome_var, y=ci_lower_actual), color="red", size=3) +
    geom_point(aes(x=outcome_var, y=ci_upper_actual), color="blue", size=3) +
    geom_hline(yintercept = 0, colour="red", linetype = "longdash") +
    
    geom_label(aes(x=outcome_var,y=ci_lower_actual,label=ci_lower_actual_lbl), colour = CI_plot_df$lower_color, fontface = "bold") + 
    geom_label(aes(x=outcome_var,y=ci_upper_actual,label=ci_upper_actual_lbl), colour = CI_plot_df$upper_color, fontface = "bold") + 
    
    coord_flip() +
    theme_economist() +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
    ) +
    ggtitle(paste0("Treatment Effect Estimate 95% Confidence Intervals", ' (',spend_util_indicator,')')) + 
    xlab("Outcome variable") +
    ylab(var_label)
  
  if(dollar_formatting){
    p <- p + 
      scale_y_continuous(labels = dollar)
  }
  
  p
}


CI_plot_means <- function(merged_summary_df, var_label, spend_util_indicator = "cost", dollar_formatting = FALSE){
  #' Confidence Interval Plot With Means
  #'
  #' This function is used to format the final results dataframe with dollar signs for the spend variables and
  #' appropriate rounding for the count variables. It includes a label for the mean of the treatment effect.
  #'
  #' @param merged_summary_df The final results dataframe from the output of the bootstrap step.
  #' @param spend_util_indicator X axis to be used in the plot.
  #' @param var_type Variable type. Defaults to "cost".
  #' @param dollar_formatting Boolean value for creating a scale_y_continuous with dollar labels for cost
  #' 
  #' @examples
  #' CI_plot(merged_summary, var_label = "Total Count", spend_util_indicator = "utilization")
  #' CI_plot(merged_summary, var_label = "Total net eligible amount (U.S. Dollars)", spend_util_indicator = "cost", dollar_formatting = TRUE)
  
  spend_util_indicator_enquo <- enquo(spend_util_indicator)
  
  CI_plot_df <- merged_summary_df %>% 
    mutate(var_type = ifelse(paste0(outcome_var,'_perf') %in% cost_vars, 'cost', 'utilization')) %>% 
    select(outcome_var, var_type, Baseline_Mean, 
           ci_lower_actual, ci_mean_actual, ci_upper_actual, ci_lower_actual_lbl, ci_mean_actual_lbl,  ci_upper_actual_lbl,
           tx_fx_est, transformed_beta, percent_change, lower_color, mean_color, upper_color) %>% 
    filter(var_type == !!spend_util_indicator_enquo)
  
  p <- ggplot(CI_plot_df) +
    geom_segment(aes(x=outcome_var, xend=outcome_var, y=ci_lower_actual, yend=ci_upper_actual), color="pink", size=3) +
    geom_point(aes(x=outcome_var, y=ci_lower_actual), color="red", size=3) +
    geom_point(aes(x=outcome_var, y=ci_mean_actual), color="blue", size=3) +
    geom_point(aes(x=outcome_var, y=ci_upper_actual), color="blue", size=3) +
    geom_hline(yintercept = 0, colour="red", linetype = "longdash") +
    
    geom_label(aes(x=outcome_var,y=ci_lower_actual,label=ci_lower_actual_lbl), colour = CI_plot_df$lower_color, fontface = "bold") + 
    geom_label(aes(x=outcome_var,y=ci_mean_actual,label=ci_mean_actual_lbl), colour = CI_plot_df$mean_color, fontface = "bold") + 
    geom_label(aes(x=outcome_var,y=ci_upper_actual,label=ci_upper_actual_lbl), colour = CI_plot_df$upper_color, fontface = "bold") + 
    
    coord_flip() +
    theme_economist() +
    theme(
      legend.position = "none",
      panel.border = element_blank(),
    ) +
    ggtitle(paste0("Treatment Effect Estimate 95% Confidence Intervals", ' (',spend_util_indicator,')')) + 
    xlab("Outcome variable") +
    ylab(var_label)
  
  if(dollar_formatting){
    p <- p + 
      scale_y_continuous(labels = dollar)
  }
  
  p
}


distance_jitter <- function(df, treatment_indicator = 'mbr_sta', title = "Default title"){
  #' Jitter plot of distance measure
  #'
  #' This function is used to create a jitter plot of the distance measure, faceted by
  #' treatment status and match inclusion.
  #'
  #' @param df The dataset to be used. Should be obtained like so:
  #'  match_data_with_weight_and_distance <- get_weights(my_data, M1)
  #' @param treatment_indicator The variable indicating group membership (defaults to 'mbr_sta')
  #' 
  #' @examples
  #' distance_jitter(match_data_with_weight_and_distance, treatment_indicator = 'mbr_sta')
  
  treatment_indicator_quo <- enquo(treatment_indicator)
  
  jitter_df <- df %>% 
    select(mid, !!treatment_indicator_quo, distance, weights) %>% 
    mutate(
      segment = case_when(
        weights == 0 & mbr_sta == 0 ~ "Control Unmatched",
        weights == 1 & mbr_sta == 0 ~ "Control Matched",
        weights == 0 & mbr_sta == 1 ~ "Engaged Unmatched",
        weights == 1 & mbr_sta == 1 ~ "Engaged Matched"
      )
    )
  
  p <- ggplot(jitter_df, aes(as.factor(segment), distance))
  
  p <- p + geom_jitter(aes(colour = as.factor(!!sym(treatment_indicator))), size = 1.5, width = 0.25) +
    scale_colour_brewer(type = "qual", aesthetics = "colour", palette = "Pastel1") + 
    ggtitle("Distribution of distance metric from match", subtitle = "Logit of PS") + 
    theme_gdocs() + 
    labs(colour = "Treatment Status") +
    coord_flip() + 
    ylab("Propensity Score") + 
    xlab("")
  
  p + ggtitle(title)
  
}


distance_histogram <- function(df, 
                               treatment_indicator = 'mbr_sta', 
                               title = "Distribution of distance metric", 
                               subtitle = "Logit of propensity score"){
  #' Histogram plot of distance measure
  #'
  #' This function is used to create a histogram plot of the distance measure, faceted by
  #' treatment status and raw vs. matched.
  #'
  #' @param df The dataset to be used. Should be obtained like so:
  #'  match_data_with_weight_and_distance <- get_weights(my_data, M1)
  #' @param treatment_indicator The variable indicating group membership (defaults to 'mbr_sta')
  #' @param title Optional
  #' @param subtitle Optional
  #' 
  #' @examples
  #' distance_histogram(match_data_with_weight_and_distance)
  
  starter_df <- df %>% 
    select(mid, !!treatment_indicator, distance, weights) %>% 
    mutate(
      segment = case_when(
        weights == 0 & mbr_sta == 0 ~ "control_unmatched",
        weights == 1 & mbr_sta == 0 ~ "control_matched",
        weights == 0 & mbr_sta == 1 ~ "engaged_unmatched",
        weights == 1 & mbr_sta == 1 ~ "engaged_matched"
      )
    )
  
  histo_df <- starter_df %>% 
    filter(weights == 1) %>% 
    mutate(
      engaged = ifelse(mbr_sta == 1, "Engaged", "Control"),
      raw = "Matched"
    ) %>% 
    rbind(starter_df %>% 
            mutate(
              engaged = ifelse(mbr_sta == 1, "Engaged", "Control"),
              raw = "Raw")
    )
  
  # A histogram of propensity scores
  hp <- ggplot(histo_df, aes(x=distance)) + 
    geom_histogram(binwidth=0.01, colour="black", fill="pink")
  
  # Histogram of propensity scores, divided by treatment status and weights
  hp + 
    xlab("Propensity Score") + 
    facet_grid(as.factor(engaged) ~ raw) + 
    ggtitle(title, subtitle = subtitle) + 
    theme_light() + 
    theme(
      strip.background = element_rect(
        color="black", fill="skyblue", size=1, linetype="solid"
      ),
      strip.text = element_text(face = "bold.italic", size = 12, colour = "black")
    )
}


distance_density <- function(matchit_object, 
                             treatment_indicator = 'mbr_sta',
                             title = "Distribution of distance metric", 
                             subtitle = "Logit of propensity score"){
  #' Density plot of distance measure
  #'
  #' This function is used to create a density plot of the distance measure, faceted by
  #' treatment status and raw vs. matched.
  #'
  #' @param matchit_object The result from the call to MatchIt
  #' @param treatment_indicator The variable indicating group membership (defaults to 'mbr_sta')
  #' @param title Optional
  #' @param subtitle Optional
  #' 
  #' @examples
  #' distance_density(M2_match_data, title = "Distribution of distance metric from match 2", subtitle = "Logit of propensity score")
  ggplot(matchit_object, aes(x = distance, fill = as.factor(mbr_sta))) + 
    geom_density(alpha = 0.9) + 
    scale_colour_brewer(type = "qual", aesthetics = "fill", palette = "Pastel1") + 
    ggtitle(title, subtitle = subtitle) + 
    xlab("Logit of propensity score (a.k.a. distance)") + 
    labs(fill = "Treatment Status") + 
    theme_gdocs()
}


mirrored_histogram <- function(matchit_object, title = "Propensity score balance"){
  #' Mirrored histogram plot of distance metric
  #'
  #' This function is used to create a mirrored histogram
  #'
  #' @param matchit_object The result of the call to MatchIt()
  #' @param title The title for the graph (optional - defaults to "Propensity Score Balance")
  #' 
  #' @examples
  #' mirrored_histogram(M1, "Match 1 propensity score balance")

  bal.plot(matchit_object, var.name = "distance", which = "both",
           type = "histogram", mirror = TRUE, colors = c("skyblue","pink")) + 
    theme_light() + 
    theme(
      strip.background = element_rect(
        color="black", fill="skyblue", size=1, linetype="solid"
      ),
      strip.text = element_text(face = "bold.italic", size = 12, colour = "black")
    ) + 
    ggtitle(title)
}


GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })


geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  #' Split density plotter
  #'
  #' This function is used to create the split density plots
  #' https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2
  #'
  #' @param mapping
  #' @param data
  #' @param stat
  #' @param position
  #' @param draw_quantiles
  #' @param trim
  #' @param scale
  #' @param na.rm
  #' @param show.legend
  #' @param inherit.aes
  #' 
  #' @examples
  #' geom_split_violin()
  
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

