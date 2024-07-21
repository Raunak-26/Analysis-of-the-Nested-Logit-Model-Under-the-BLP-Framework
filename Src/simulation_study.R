  # Load required libraries
  library(gmm)
  library(mlogit)
  library(dplyr)
  #install.packages('systemfit')
  #install.packages('ggplot2')
  library(systemfit)
  library(AER)
  library(ggplot2)
  
  rm(list=ls())
  
  set.seed(100)
  
  # Set the number of markets and products
  n_markets <- 25
  n_products <- 100
  n = 1000
  
  
  # Define MNL --------------------------------------------------------------
  
  
  results_logit = data.frame(price = matrix(NA,n,1),
                             sd_price = matrix(NA,n,1),
                             tvalue_price = matrix(NA,n,1),
                             df = matrix(NA,n,1),
                             pvalue_price = matrix(NA,n,1),
                             r_sq = matrix(NA,n,1),
                             adj_r_sq = matrix(NA,n,1),
                             fstat_instrument = matrix(NA,n,1),
                             agg_x1 = matrix(NA,n,1),
                             sd_agg_x1 = matrix(NA,n,1),
                             pvalue_fstat = matrix(NA,n,1))
  
  modelselection_logit = data.frame(AIC = matrix(NA,n,1),
                                    BIC = matrix(NA,n,1))
  
  lower_confint_logit = matrix(NA,n,1)
  colnames(lower_confint_logit) = c("price")
  upper_confint_logit = matrix(NA,n,1)
  colnames(upper_confint_logit) = c("price")
  
  loglik_logit = matrix(NA,n,1)
  colnames(loglik_logit) = c("Log_liklihood")
  
  elasticity_logit = matrix(NA,n,2)
  colnames(elasticity_logit) = c("Own",
                                 "Cross")
  
  
  # Define NL ---------------------------------------------------------------
  
  results = data.frame(ln_s_within=matrix(NA,n,1),
                       ln_s_within_group = matrix(NA,n,1),
                       price = matrix(NA,n,1),
                       sd_s_within = matrix(NA,n,1),
                       sd_s_within_group = matrix(NA,n,1),
                       sd_price = matrix(NA,n,1),
                       tvalue_within = matrix(NA,n,1),
                       tvalue_within_group = matrix(NA,n,1),
                       tvalue_price = matrix(NA,n,1),
                       df = matrix(NA,n,1),
                       pvalue_within = matrix(NA,n,1),
                       pvalue_within_group = matrix(NA,n,1),
                       pvalue_price = matrix(NA,n,1),
                       r_sq = matrix(NA,n,1),
                       adj_r_sq = matrix(NA,n,1),
                       fstat_instrument = matrix(NA,n,1),
                       agg_x1 = matrix(NA,n,1),
                       sd_agg_x1 = matrix(NA,n,1),
                       pvalue_fstat = matrix(NA,n,1))
  
  model_selection = data.frame(AIC = matrix(NA,n,1),
                               BIC = matrix(NA,n,1))
  
  lower_confint = matrix(NA,n,3)
  colnames(lower_confint) = c("ln_s_within","ln_s_within_group","price")
  upper_confint = matrix(NA,n,3)
  colnames(upper_confint) = c("ln_s_within","ln_s_within_group","price")
  
  loglik_nested = matrix(NA,n,1)
  colnames(loglik_nested) = c("Log_liklihood")
  
  elasticity = matrix(NA,n,4)
  colnames(elasticity) = c("Own",
                           "Cross_same_subgroup",
                           "Cross_diff_subgroup",
                           "Cross_diff_group")
  
  
  # Simulation Study --------------------------------------------------------
  
  
  for (i in 1:n) {
    
    # Generate data for the first level
    data1 <- data.frame(
      mkt_id = rep(1:n_markets, each = n_products),
      prod_id = rep(1:n_products, times = n_markets))
    #agg_x1 = rnorm(n_markets * n_products,5,2),# Aggregate characteristics
    #agg_x3 = rnorm(n_markets * n_products,0,1) #unobserved random var)
    #)
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(cat= sample(c(0,1),n_products,replace=T))%>%
      ungroup()
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(sub_cat = sample(2:4,n_products,replace=T))%>%
      ungroup()
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(agg_x1 = rnorm(n(),10,3))%>%
      ungroup()
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(agg_x3 = rnorm(n(),0,1))%>%
      ungroup()
    
    data1$price = rlnorm(n_markets * n_products,1,0.5) + 1*data1$agg_x3 + 0.3*data1$agg_x1
    
    # Simulate demand parameters
    beta <- c(-1,-2,-0.5)  # Coefficients
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(utility = beta[1] + beta[2] * price + 
               beta[3]*agg_x1 + agg_x3)%>%
      ungroup()
    
    # Calculate instruments
    data1$instrument1 <- data1$agg_x1 + rnorm(n_markets*n_products)
    data1$instrument2 = data1$agg_x3 + rnorm(n_markets*n_products)
  
    data1$exp_utility <- exp(data1$utility)
    
    data1 = data1%>%
      group_by(mkt_id)%>%
      mutate(share = exp_utility/(1 + sum(exp_utility)))%>%
      ungroup()
    
    data1$share_0 = 1/ 1 + sum(data1$exp_utility)
    data1$ln_share = log(data1$share/data1$share_0)
    
    data1 = data1%>%
      group_by(mkt_id,cat,sub_cat)%>%
      mutate(denom1 = sum(share))%>%
      ungroup()
    
    data1$ln_s_within_group = log(data1$share/data1$denom1)
    
    data1 = data1%>%
      group_by(mkt_id,cat)%>%
      mutate(denom = sum(share))%>%
      ungroup()
    
    data1$ln_s_within = log(data1$denom1/data1$denom)
    
    data1$ln_share_within = log(data1$share/data1$denom)
    
    # First-stage regression to test instrument relevance in MNL
    first_stage = lm(price ~ instrument1+instrument2+agg_x1, data = data1)
    results_logit[i,8] = summary(first_stage)$fstatistic[1]
    results_logit[i,11] = pf(summary(first_stage)$fstatistic[1],
                             summary(first_stage)$fstatistic[2],
                             summary(first_stage)$fstatistic[3],
                             lower.tail = FALSE)
    
    #Logit regression
    ab1 = ivreg(ln_share~ price + agg_x1|.-price + agg_x1 + instrument1 +
                  instrument2,data=data1)
    #summary(ab1)
    
    #Confidence intervals
    
    lower_confint_logit[i,1] = confint(ab1)[2,1]
    upper_confint_logit[i,1] = confint(ab1)[2,2]
    
    results_logit[i,1] = ab1$coefficients[2]
    results_logit[i,2] = sqrt(diag(vcov(ab1)))[2]
    results_logit[i,3] = summary(ab1)$coefficients[2,"t value"]
    results_logit[i,4] = ab1$df.residual
    results_logit[i,5] = 2*(1 - pt(abs(results_logit[i,3]),df = results_logit[i,4]))
    results_logit[i,6] = summary(ab1)$r.squared
    results_logit[i,7] = summary(ab1)$adj.r.squared
    results_logit[i,9] = ab1$coefficients[3]
    results_logit[i,10] = sqrt(diag(vcov(ab1)))[3]
    
    #Model Selection
    
    #loglik_logit[i,] <- sum(dnorm(residuals(ab1), mean = 0, sd = summary(ab1)$sigma, log = TRUE))
    loglik_logit[i,] = -n/2 * log(2 * pi * summary(ab1)$sigma^2) - sum(residuals(ab1)^2) / (2 * summary(ab1)$sigma^2)
    
    # Number of parameters (intercept + X)
    k <- length(coef(ab1))
    
    # Calculate AIC and BIC
    modelselection_logit[i,1] <- -2 * loglik_logit[i,] + 2 * k
    modelselection_logit[i,2] <- -2 * loglik_logit[i,] + log(n_markets*n_products) * k
    
    #Elasticity
    elasticity_logit[i,1] = ab1$coefficients[2]*data1$price[i]*(1 - data1$share[i])
    elasticity_logit[i,2] = ab1$coefficients[2]*data1$price[i]*(- data1$share[i])
    
    # First-stage regression to test instrument relevance in NL
    first_stage = lm(price ~ instrument1+instrument2+agg_x1, data = data1)
    results[i,16] = summary(first_stage)$fstatistic[1]
    results$pvalue_fstat[i] = pf(summary(first_stage)$fstatistic[1],
                             summary(first_stage)$fstatistic[2],
                             summary(first_stage)$fstatistic[3],
                             lower.tail = FALSE)
    
    #Nested logit regression
    ab2 = ivreg(ln_share~ln_s_within + ln_s_within_group + price  + 
                  agg_x1|.-price + agg_x1 + instrument1 + instrument2,data=data1)
    #summary(ab2)
    
    #Confidence intervals
    
    lower_confint[i,1:3] = confint(ab2)[2:4,1]
    upper_confint[i,1:3] = confint(ab2)[2:4,2]
    
    results[i,1:3] = ab2$coefficients[2:4]
    results[i,4:6] = sqrt(diag(vcov(ab2)))[2:4]
    results[i,7:9] = summary(ab2)$coefficients[2:4,"t value"]
    results[i,10] = ab2$df.residual
    results[i,11] = 2*(1 - pt(abs(results[i,7]),df = results[i,10]))
    results[i,12] = 2*(1 - pt(abs(results[i,8]),df = results[i,10]))
    results[i,13] = 2*(1 - pt(abs(results[i,9]),df = results[i,10]))
    results[i,14] = summary(ab2)$r.squared
    results[i,15] = summary(ab2)$adj.r.squared
    results[i,17] = ab2$coefficients[5]
    results[i,18] = sqrt(diag(vcov(ab2)))[5]
    
    #Model Selection
    
    #loglik_nested[i,] = sum(dnorm(residuals(ab2), mean = 0, sd = summary(ab2)$sigma, log = TRUE))
    loglik_nested[i,] = -n/2 * log(2 * pi * summary(ab2)$sigma^2) - sum(residuals(ab2)^2) / (2 * summary(ab2)$sigma^2)
    
    # Number of parameters (intercept + X)
    k <- length(coef(ab2))
    
    # Calculate AIC and BIC
    model_selection[i,1] <- -2 * loglik_nested[i,] + 2 * k
    model_selection[i,2] <- -2 * loglik_nested[i,] + log(n_markets*n_products) * k
    
    #Elasticity
    elasticity[i,1] = ab2$coefficients[4]*(1/(1 - ab2$coefficients[3]) - 
                                             (1/(1-ab2$coefficients[3]) - 1/(1-ab2$coefficients[2]))*exp(data1$ln_s_within_group[i]) - 
                                             (ab2$coefficients[2]/(1-ab2$coefficients[2]))*exp(data1$ln_share_within[i]) - 
                                             data1$share[i])*data1$price[i] 
    elasticity[i,2] = ab2$coefficients[4]*(-((1/(1-ab2$coefficients[3])) - 
                                               (1/(1-ab2$coefficients[2])))*exp(data1$ln_s_within_group[i])- 
                                             (ab2$coefficients[2]/(1-ab2$coefficients[2]))*exp(data1$ln_share_within[i]) - 
                                             data1$share[i])*data1$price[i]
    elasticity[i,3] = ab2$coefficients[4]*(-(ab2$coefficients[2]/(1- ab2$coefficients[2]))*exp(data1$ln_share_within[i]) - 
                                             data1$share[i])*data1$price[i]
    elasticity[i,4] = ab2$coefficients[4]*(-data1$share[i])*data1$price[i]
  }
  
  
  # Post-Estimation ---------------------------------------------------------
  
  #Logit
  
  avg_elasticity_logit = data.frame(Own =  mean(elasticity_logit[,1]),
                                    Cross =  mean(elasticity_logit[,2]))
  
  confidence_interval_logit = data.frame(price = results_logit$price,
                                   lower = lower_confint_logit,
                                   upper = upper_confint_logit)
  
  # Calculate the proportion of significant p-values
  significance_threshold <- 0.05
  propsignificant_logit = mean(results_logit$pvalue_price < significance_threshold)
  
  avg_results_logit = data.frame(avg_price = mean(results_logit$price),
                                 avg_sd_price = sd(results_logit$price)/sqrt(n),
                                 avg_r_sq = mean(results_logit$r_sq),
                                 avg_r_adj_sq = mean(results_logit$adj_r_sq),
                                 avg_fstat_instrument = mean(results_logit$fstat_instrument),
                                 avg_agg_x1 = mean(results_logit$agg_x1),
                                 avg_sd_agg_x1 = sd(results_logit$agg_x1)/sqrt(n),
                                 avg_pvalue_fstat = mean(results_logit$pvalue_fstat))
  
  avg_zscore_logit = data.frame(zscore_price = avg_results_logit$avg_price / avg_results_logit$avg_sd_price)
  
  avg_pvalue_logit = data.frame(pvalue_avg_price = 2 * (1 - pnorm(abs(avg_zscore_logit$zscore_price))))
  
  
  #Avg Model_selection values
  
  avg_AIC_logit = mean(modelselection_logit$AIC)
  avg_BIC_logit = mean(modelselection_logit$BIC)
  
  #Nested Logit 
  
  avg_elasticity = data.frame(Own =  mean(elasticity[,1]),
                              Cross_same_subgroup =  mean(elasticity[,2]),
                              Cross_diff_subgroup =  mean(elasticity[,3]),
                              Cross_diff_group =  mean(elasticity[,4]))
  
  
  confidence_interval = data.frame(within = results$ln_s_within,
                                   within_group = results$ln_s_within_group,
                                   price = results$price,
                                   lower = lower_confint,
                                   upper = upper_confint)
  
  # Calculate the proportion of significant p-values
  significance_threshold <- 0.05
  prop_significant = data.frame(ln_s_within = c(0),ln_s_within_group = c(0),price = c(0),rum=c(0))
  prop_significant[,1] = mean(results$pvalue_within < significance_threshold)
  prop_significant[,2] = mean(results$pvalue_within_group < significance_threshold)
  prop_significant[,3] = mean(results$pvalue_price < significance_threshold)
  prop_significant[,4] = mean(results$ln_s_within_group > results$ln_s_within)
  
  avg_results = data.frame(avg_ln_s_within = mean(results$ln_s_within),
                           avg_ln_s_within_group = mean(results$ln_s_within_group),
                           avg_price = mean(results$price),
                           avg_sd_s_within = sd(results$ln_s_within)/sqrt(n),
                           avg_sd_within_group = sd(results$ln_s_within_group)/sqrt(n),
                           avg_sd_price = sd(results$price)/sqrt(n),
                           avg_r_sq = mean(results$r_sq),
                           avg_adj_r_sq = mean(results$adj_r_sq),
                           avg_fstat_instrument = mean(results$fstat_instrument),
                           avg_agg_x1 = mean(results$agg_x1),
                           avg_sd_agg_x1 = sd(results$agg_x1)/sqrt(n),
                           avg_pvalue_fstat = mean(results$pvalue_fstat))
  
  avg_zscore = data.frame(zscore_within = avg_results$avg_ln_s_within / avg_results$avg_sd_s_within,
                          zscore_within_group = avg_results$avg_ln_s_within_group / avg_results$avg_sd_within_group,
                          zscore_price = avg_results$avg_price / avg_results$avg_sd_price)
  
  avg_pvalue = data.frame(pvalue_avg_within = 2 * (1 - pnorm(abs(avg_zscore$zscore_within))),
                          pvalue_avg_within_group = 2 * (1 - pnorm(abs(avg_zscore$zscore_within_group))),
                          pvalue_avg_price = 2 * (1 - pnorm(abs(avg_zscore$zscore_price))))
  
  
  #Avg Model_selection values
  
  avg_AIC = mean(model_selection$AIC)
  avg_BIC = mean(model_selection$BIC)
  
  
  
  # Plots -------------------------------------------------------------------
  
  #Logit
  
  # Histogram of parameter estimates
  
  histogram_price <- ggplot(results_logit, aes(x = price)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Price Estimates", x = "Price Estimates", y = "Frequency") +
    theme_classic() +
    theme(panel.background = element_rect(fill = "white"))
  
  # Density plot of parameter estimates
  density_plot <- ggplot(results_logit, aes(x = price)) +
    geom_density(color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Density Plot of Parameter Estimates", x = "Estimates", y = "Density") +
    theme_classic() +
    theme(panel.background = element_rect(fill = "white"))
  
  #Confidence Interval plot for parameter estimates 
  
  cd_data = slice_sample(confidence_interval_logit,n=100) #Random draws of simulation estimates
  
  cd_price = ggplot(cd_data)+
    geom_line(aes(x = 1:length(price), y = price))+
    geom_line(aes(x = 1:length(price), y = price.1),color = 'red')+
    geom_line(aes(x = 1:length(price), y = price.2),color = 'red')+
    labs(title = "Confidence Interval of Price Estimate", x = "Frequency", y = "Sub-Category Estimate") +
    theme_classic()+
    theme(panel.background = element_rect(fill = "white"))
  
  # Plot the graphs
  print(histogram_price)
  print(density_plot)
  print(cd_price)
  
  #Nested Logit
  #Plots
  
  # Histogram of parameter estimates
  histogram_within <- ggplot(results, aes(x = ln_s_within)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Category Estimates", x = "Category_Estimates", y = "Frequency") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  histogram_within_group <- ggplot(results, aes(x = ln_s_within_group)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Sub-Category Estimates", x = "Sub-Category_Estimates", y = "Frequency") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  histogram_price <- ggplot(results, aes(x = price)) +
    geom_histogram(binwidth = 0.05, color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Price Estimates", x = "Price Estimates", y = "Frequency") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  # Density plot of parameter estimates
  density_plot <- ggplot(results, aes(x = ln_s_within)) +
    geom_density(color = "black", fill = "blue", alpha = 0.7) +
    labs(title = "Density Plot of Parameter Estimates", x = "Estimates", y = "Density") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  #Confidence Interval plot for parameter estimates 
  
  cd_data = slice_sample(confidence_interval,n=100) #Random draws of simulation estimates
  
  cd_within = ggplot(cd_data, aes(x = 1:length(within))) +
    geom_line(aes(y = within, color = "Nesting Parameter 2")) +
    geom_line(aes(y = lower.ln_s_within, color = "Confidence Intervals"), linetype = "dashed", show.legend = TRUE) +
    geom_line(aes(y = upper.ln_s_within, color = "Confidence Intervals"), linetype = "dashed", show.legend = TRUE) +
    labs(title = "Confidence Interval of Sub-Category Estimate", x = "Frequency", y = "Nesting Parameter 2 Estimate", color = "Legend") +
    scale_color_manual(values = c("Nesting Parameter 2" = "blue", "Confidence Intervals" = "red")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  cd_within_group = ggplot(cd_data, aes(x = 1:length(within_group))) +
    geom_line(aes(y = within_group, color = "Nesting Parameter 1")) +
    geom_line(aes(y = lower.ln_s_within_group, color = "Confidence Intervals"), linetype = "dashed", show.legend = TRUE) +
    geom_line(aes(y = upper.ln_s_within_group, color = "Confidence Intervals"), linetype = "dashed", show.legend = TRUE) +
    labs(title = "Confidence Interval of Sub-Category Estimate", x = "Frequency", y = "nesting Parameter 1 Estimate", color = "Legend") +
    scale_color_manual(values = c("Nesting Parameter 1" = "blue", "Confidence Intervals" = "red")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  cd_price <- ggplot(cd_data, aes(x = 1:length(price))) +
    geom_line(aes(y = price, color = "Price")) +
    geom_line(aes(y = lower.price, color = "Confidence Intervals"), linetype = "dashed") +
    geom_line(aes(y = upper.price, color = "Confidence Intervals"), linetype = "dashed") +
    labs(title = "Confidence Interval of Sub-Category Estimate", x = "Frequency", y = "Price Estimate", color = "Legend") +
    scale_color_manual(values = c("Price" = "blue", "Confidence Intervals" = "red")) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  #Show that RUM holds
  
  rum <- ggplot(cd_data, aes(x = 1:length(price))) +
    geom_line(aes(y = within, color = "Within")) +
    geom_line(aes(y = within_group, color = "Within Group")) +
    labs(title = "Relationship between Nesting Parameters", x = "Frequency", y = "Nesting Parameters", color = "Legend") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())+
    theme(plot.title = element_text(hjust=0.5))
  
  # Plot the graphs
  print(histogram_within)
  print(histogram_within_group)
  print(histogram_price)
  print(density_plot)
  print(cd_within)
  print(cd_within_group)
  print(cd_price)
  print(rum)
  
  png(file="C:/Users/Raunak/OneDrive/Desktop/Master Thesis/in/rum_plot.png",
      width=700, height=480, pointsize = 10)
  print(rum)
  dev.off()
  
  
  
  
  # Likelihood Ratio test ----------------------------------------------------
  lrt = data.frame(lrt_stats= matrix(NA,n,1),
                   p_values_lrt = matrix(NA,n,1))
  
  for (j in 1:n) {
    
    # Compute LRT statistic
    lrt[j,1] = -2 * (loglik_logit[j,] - loglik_nested[j,])
    
    # Compute p-value
    df_diff = results_logit$df[j] - results$df[j]
    lrt[j,2] = pchisq(lrt[j,1], df = df_diff, lower.tail = FALSE)
  }
  
  # Summary statistics
  avg_lrt_stat = mean(lrt$lrt_stats)
  avg_p_value <- mean(lrt$p_values_lrt)
  propsignificant_lrt <- mean(lrt$p_values_lrt < 0.05)
  
  # Plot histograms and density plots for log-likelihoods
  data_loglik <- data.frame(
    Model = rep(c("Logit", "Nested Logit"), each = n),
    LogLikelihood = c(loglik_logit, loglik_nested)
  )
  
  ggplot(data_loglik, aes(x = LogLikelihood, fill = Model)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    geom_density(alpha = 0.5) +
    labs(title = "Log-Likelihood Distribution", x = "Log-Likelihood", y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  # Plot histograms and density plots
  data_aic <- data.frame(Model = rep(c("Logit", "Nested Logit"), each = n),
                         AIC = c(modelselection_logit$AIC,model_selection$AIC))
  
  data_bic <- data.frame(Model = rep(c("Logit", "Nested Logit"), each = n),
                         BIC = c(modelselection_logit$BIC, model_selection$BIC))
  
  # AIC Plot
  ggplot(data_aic, aes(x = AIC, fill = Model)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    geom_density(alpha = 0.5) +
    labs(title = "AIC Distribution", x = "AIC", y = "Frequency") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
  # BIC Plot
  ggplot(data_bic, aes(x = BIC, fill = Model)) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    geom_density(alpha = 0.5) +
    labs(title = "BIC Distribution", x = "BIC", y = "Frequency") +
    theme_minimal()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank())
  
