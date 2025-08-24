library(extRemes)
library(dplyr)
library(lubridate)

# Get data
clim_wind2000 = read.csv("climate_model2000.csv")
clim_wind2000$time = as.Date(clim_wind2000$time)
clim_wind2000$Year = year(clim_wind2000$time)
clim_wind2000$Month = month(clim_wind2000$time)

clim_wind2000 = clim_wind2000 %>% dplyr::select(-plev, -ncol, -lat, -lon)
clim_wind2000 = na.omit(clim_wind2000)

clim_wind2090 = read.csv("climate_model2090.csv")
clim_wind2090$time = as.Date(clim_wind2090$time)
clim_wind2090$Year = year(clim_wind2090$time)
clim_wind2090$Month = month(clim_wind2090$time)

clim_wind2090 = clim_wind2090 %>% dplyr::select(-plev, -ncol, -lat, -lon)
clim_wind2000 = unique(clim_wind2000)
clim_wind2090 = unique(clim_wind2090)
clim_wind2000 = clim_wind2000 %>% mutate(day_of_year = yday(time))
clim_wind2090 = clim_wind2090 %>% mutate(day_of_year = yday(time))

# Mean Climatology 2000
yearly_mean_climatology = clim_wind2000 %>% group_by(day_of_year) %>% 
  summarise(mean_wind_speed = mean(surface_wind))
ggplot(yearly_mean_climatology, aes(x = day_of_year, y = mean_wind_speed)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(60, 151, 244, 335),  # MAM, JJA, SON, DJF
             linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = max(yearly_mean_climatology$mean_wind_speed), label = "MAM", angle = 90, vjust = -0.5) +
  annotate("text", x = 152, y = max(yearly_mean_climatology$mean_wind_speed), label = "JJA", angle = 90, vjust = -0.5) +
  annotate("text", x = 244, y = max(yearly_mean_climatology$mean_wind_speed), label = "SON", angle = 90, vjust = -0.5) +
  annotate("text", x = 335, y = max(yearly_mean_climatology$mean_wind_speed), label = "DJF", angle = 90, vjust = -0.5) +
  
  labs(title = "Yearly Mean Climatology of Wind Speeds",
       x = "Day of Year",
       y = "Mean Wind Speed (m/s)") +
  theme_minimal()

# Maximum Climatology 2000
yearly_max_climatology = clim_wind2000 %>% group_by(day_of_year) %>% 
  summarise(max_wind_speed = max(surface_wind))
ggplot(yearly_max_climatology, aes(x = day_of_year, y = max_wind_speed)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(60, 151, 244, 335),  # MAM, JJA, SON, DJF
             linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = max(yearly_max_climatology$max_wind_speed), label = "MAM", angle = 90, vjust = -0.5) +
  annotate("text", x = 152, y = max(yearly_max_climatology$max_wind_speed), label = "JJA", angle = 90, vjust = -0.5) +
  annotate("text", x = 244, y = max(yearly_max_climatology$max_wind_speed), label = "SON", angle = 90, vjust = -0.5) +
  annotate("text", x = 335, y = max(yearly_max_climatology$max_wind_speed), label = "DJF", angle = 90, vjust = -0.5) +
  
  labs(title = "Yearly Maximum Climatology of Wind Speeds",
       x = "Day of Year",
       y = "Mean Wind Speed (m/s)") +
  theme_minimal()

# Maximum Climatology Comparison Historical and Future
yearly_max_climatology_hist = clim_wind2000 %>% group_by(day_of_year) %>% 
  summarise(max_wind_speed = max(surface_wind))
clim_wind2090 = clim_wind2090 %>% mutate(day_of_year = yday(time))
yearly_max_climatology_future = clim_wind2090 %>% group_by(day_of_year) %>% 
  summarise(max_wind_speed = max(surface_wind))

temp <- data.frame(
  time = c(1:366),
  hist = yearly_max_climatology_hist$max_wind_speed,
  future = yearly_max_climatology_future$max_wind_speed
)

temp_long = data.frame(
  time = rep(temp$time, 2),
  y = c(temp$hist, temp$future),
  group = rep(c("Historical (2000s)", "Future (2090s)"), each = 366)
)

ggplot(temp_long, aes(x = time, y = y, color = group)) +
  geom_line() +
  labs(title = "Yearly Maximum Climatology of Wind Speeds in Toronto (Historical vs Future)",
       x = "Day of Year",
       y = "Maximum Wind Speed (m/s)",
       color = "Time Period") +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.15),  # x and y coordinates inside plot
    legend.background = element_rect(fill = "white", color = "black")  # optional styling
  ) + 
  geom_vline(xintercept = c(60, 151, 244, 335),  # MAM, JJA, SON, DJF
             linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = max(yearly_max_climatology_hist$max_wind_speed) - 0.6, label = "Spring", angle = 90, vjust = -0.5) +
  annotate("text", x = 152, y = max(yearly_max_climatology_hist$max_wind_speed) - 0.6, label = "Summer", angle = 90, vjust = -0.5) +
  annotate("text", x = 244, y = max(yearly_max_climatology_hist$max_wind_speed) - 0.6, label = "Autumn", angle = 90, vjust = -0.5) +
  annotate("text", x = 335, y = max(yearly_max_climatology_hist$max_wind_speed) - 0.6, label = "Winter", angle = 90, vjust = -0.5)

# Baseline Threshold Models
threshold = quantile(clim_wind2000$surface_wind, probs = 0.98)
threshold2 = quantile(clim_wind2090$surface_wind, probs = 0.98)
clim_fit = fevd(surface_wind, clim_wind2000, threshold = threshold, type = "GP")
plot(clim_fit)
plot(clim_fit, "trace")
return.level(clim_fit, return.period = c(2, 20, 50, 100, 500), do.ci=TRUE)
ci(clim_fit, type="return.level", method="proflik", return.period = 500, xrange = c(18, 35), verbose = TRUE)

clim_fit1 = fevd(surface_wind, clim_wind2090, threshold = threshold2, type = "GP")
plot(clim_fit1)
plot(clim_fit1, "trace")
return.level(clim_fit1, return.period = c(2, 20, 50, 100, 500), do.ci=TRUE)
ci(clim_fit1, type="return.level", method="proflik", return.period = 500, xrange = c(17, 29), verbose = TRUE)

# Test for difference between extreme wind speeds historical vs future
temp = clim_wind2000 %>% mutate(period = "Historical") %>% filter(surface_wind > 10.5)
temp1 = clim_wind2090 %>% mutate(period = "Future") %>% filter(surface_wind > 10.5)
combined = rbind(temp, temp1)
ggplot(combined, aes(x = surface_wind, fill = period)) +
  geom_density(alpha=0.4) + 
  labs(title = "Density Plots of Wind Speeds Historical vs. Future", fill = "Period", 
       x = "Wind Speed (m/s)", y = "Probability") + 
  theme_minimal() + 
  theme(
    legend.position = c(0.85, 0.5),  # x and y coordinates inside plot
    legend.background = element_rect(fill = "white", color = "black")  # optional styling
  )
wilcox.test(temp$surface_wind, temp1$surface_wind, 
            alternative = "two.sided",
            paired = FALSE,
            exact = TRUE)

# Split into months 
extreme_winds2000 = clim_wind2000 %>% filter(surface_wind > 8)
extreme_winds2090 = clim_wind2090 %>% filter(surface_wind > 8)

monthly_mean1 = extreme_winds2000 %>% group_by(Month) %>% summarise(mean1 = mean(surface_wind), 
                                                                    median1 = median(surface_wind))
monthly_mean2 = extreme_winds2090 %>% group_by(Month) %>% summarise(mean2 = mean(surface_wind), 
                                                                    median2 = median(surface_wind))
monthly_diff <- left_join(monthly_mean1, monthly_mean2, by = "Month") %>%
  mutate(diff_mean = mean1 - mean2, diff_median = median1 - median2)

extreme_winds2000$Month <- factor(month.abb[extreme_winds2000$Month], levels = month.abb)
extreme_winds2090$Month <- factor(month.abb[extreme_winds2090$Month], levels = month.abb)
month_colors <- c(
  "Jan" = "#1F78B4",  # Deep winter blue
  "Feb" = "#4DA6FF",  # Frosty blue
  "Mar" = "#A6CEE3",  # Icy sky blue
  "Apr" = "#B2DF8A",  # Early spring green
  "May" = "#FFFF99",  # Pale yellow
  "Jun" = "#FDBF6F",  # Warm golden orange
  "Jul" = "#FB8072",  # Coral red
  "Aug" = "#E31A1C",  # Fiery red
  "Sep" = "#FF7F00",  # Harvest orange
  "Oct" = "#FFB347",   # Light pumpkin orange
  "Nov" = "#D8BFD8",   # Soft thistle purple
  "Dec" = "#1F78B4"   # Back to winter blue
)


# Plot boxplots 2000
ggplot(extreme_winds2000, aes(x = factor(Month), y = surface_wind, fill = factor(Month))) + geom_boxplot() +
  labs(title = "Extreme Wind Speeds above 8 m/s in Toronto (2000)",
       x = "Month",
       y = "Wind Speed (m/s)") +
  scale_fill_manual(values = month_colors) + 
  theme_minimal()

# Plot boxplots 2090
ggplot(extreme_winds2090, aes(x = factor(Month), y = surface_wind, fill = factor(Month))) + geom_boxplot() +
  labs(title = "Extreme Wind Speeds above 8 m/s in Toronto (2090)",
       x = "Month",
       y = "Wind Speed (m/s)") +
  scale_fill_manual(values = month_colors) + 
  theme_minimal()

ggplot(monthly_diff, aes(x = Month)) +
  geom_line(aes(y = diff_mean, color = "Mean"), size = 1) +
  geom_line(aes(y = diff_median, color = "Median"), size = 1) +
  geom_point(aes(y = diff_mean), size = 2) +
  geom_point(aes(y = diff_median), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Monthly Extreme Wind Speed Mean and Median Difference",
       x = "Month",
       y = "Mean Difference (2000 - 2090)") +
  scale_x_continuous(breaks = 1:12) +
  theme_minimal()

# Test for mean difference using ANOVA and Wilcoxon tests on each month
#ANOVA
extreme_winds2000 = extreme_winds2000 %>% mutate(source = "2000")
extreme_winds2090 = extreme_winds2090 %>% mutate(source = "2090")
combined_data = bind_rows(extreme_winds2000, extreme_winds2090)

anova_results <- combined_data %>%
  group_by(Month) %>%
  group_map(~ aov(surface_wind ~ source, data = .x), .keep = TRUE)

anova_list <- lapply(anova_results, summary)
names(anova_list) <- unique(combined_data$Month)
anova_list

# Wilcoxon signed-rank test
wilcox_results <- combined_data %>%
  group_by(Month) %>%
  group_map(~ wilcox.test(surface_wind ~ source, data = .x, alternative = "less"), .keep = TRUE)
p_values <- sapply(wilcox_results, function(x) x$p.value)

wilcox_summary <- data.frame(
  month = unique(combined_data$Month),
  p_value = p_values
)
wilcox_summary = wilcox_summary %>% mutate(significance = case_when(p_value < 0.05 ~ 1,
                                                                    TRUE ~ 0))
wilcox_summary

# Construct Regression Tree for both 2000 and 2090
extreme_winds2000 = clim_wind2000 %>% filter(surface_wind > 8)
extreme_winds2000 = extreme_winds2000 %>% mutate(summer_index = case_when(Month %in% c(6, 7, 8) ~ 1,
                                                                          TRUE ~ 0))
extreme_winds2000 = extreme_winds2000 %>% dplyr::select(-time, -Year, -Month, -day_of_year)

extreme_winds2090 = clim_wind2090 %>% filter(surface_wind > 8)
extreme_winds2090 = extreme_winds2090 %>% mutate(summer_index = case_when(Month %in% c(6, 7, 8) ~ 1,
                                                                          TRUE ~ 0))
extreme_winds2090 = extreme_winds2090 %>% dplyr::select(-time, -Year, -Month, -day_of_year)

regress_anova2000 = rpart(surface_wind ~ ., extreme_winds2000, method = "anova", 
                          control = rpart.control(cp=0, minsplit=25, minbucket=15, maxcompete=0, maxsurrogate=0))
regress_anova_prune2000 = prune(regress_anova2000, cp = 0.019)
rpart.plot(regress_anova_prune2000, branch = 0.3, compress = TRUE, extra = 101)

regress_anova2090 = rpart(surface_wind ~ ., extreme_winds2090, method = "anova", 
                          control = rpart.control(cp=0, minsplit=25, minbucket=15, maxcompete=0, maxsurrogate=0))
regress_anova_prune2090 = prune(regress_anova2090, cp = 0.018)
rpart.plot(regress_anova_prune2090, branch = 0.3, compress = TRUE, extra = 101)

# 2000
# Create cluster covariates 
clim_wind2000 = clim_wind2000 %>% mutate(cluster1 = case_when(low.level.wind >= 12 & atmos_stability1 >= 0.0014 & low.level.wind < 21 ~ 1,
                                                              TRUE ~ 0),
                                         cluster2 = case_when(low.level.wind >= 21 & atmos_stability1 >= 0.0052 ~ 1,
                                                              TRUE ~ 0),
                                         cluster3 = case_when(atmos_stability1 >= 0.0014 & low.level.wind >= 21 & atmos_stability1 < 0.0052 & low.level.wind < 28 ~ 1,
                                                              TRUE ~ 0),
                                         cluster4 = case_when(atmos_stability1 >= 0.0014 & low.level.wind >= 21 & atmos_stability1 < 0.0052 & low.level.wind >= 28 ~ 1,
                                                              TRUE ~ 0),
                                         cluster5 = case_when(low.level.wind >= 12 & atmos_stability1 < 0.0014 & low.level.wind < 18 ~ 1,
                                                              TRUE ~ 0),
                                         cluster6 = case_when(atmos_stability1 < 0.0014 & low.level.wind >= 18 ~ 1,
                                                              TRUE ~ 0))

# Threshold Selection
cluster0 = clim_wind2000 %>% filter(low.level.wind < 12)
cluster1 = clim_wind2000 %>% filter(cluster1 == 1)
cluster2 = clim_wind2000 %>% filter(cluster2 == 1)
cluster3 = clim_wind2000 %>% filter(cluster3 == 1)
cluster4 = clim_wind2000 %>% filter(cluster4 == 1)
cluster5 = clim_wind2000 %>% filter(cluster5 == 1)
cluster6 = clim_wind2000 %>% filter(cluster6 == 1)

prob = 0.95
t0 = quantile(cluster0$surface_wind, prob = prob)
t1 = quantile(cluster1$surface_wind, prob = prob)
t2 = quantile(cluster2$surface_wind, prob = prob)
t3 = quantile(cluster3$surface_wind, prob = prob)
t4 = quantile(cluster4$surface_wind, prob = prob)
t5 = quantile(cluster5$surface_wind, prob = prob)
t6 = 12
x = c(1, 2, 3, 4, 5, 6, 7)
threshold = c(t0, t1, t2, t3, t4, t5, t6)
temp = data.frame(x, threshold)

ggplot(temp, aes(x = x, y = threshold)) +
  geom_point(color = "darkgreen", size = 3) + geom_line() +
  labs(title = "Threshold Selection for each Cluster", x = "Cluster Index", 
       y = "Threshold (95th Percentile Winds)") +
  scale_x_continuous(breaks = 1:7) + 
  theme_minimal()

# Fit multiple models for model selection
fit1 = fevd(surface_wind, clim_wind2000, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit2 = fevd(surface_wind, clim_wind2000, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit3 = fevd(surface_wind, clim_wind2000, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            scale.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, use.phi = TRUE, type="PP")
fit4 = fevd(surface_wind, clim_wind2000, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            shape.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
lr.test(fit1, fit2)
lr.test(fit2, fit3)
lr.test(fit2, fit4)
plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)

# Conditional return levels, select fit3 as model, omit bootstrapping conditional RL
v = make.qcov(fit3, vals = list(threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0),
                                mu1 = c(0, 1, 0, 0, 0, 0, 0),
                                mu2 = c(0, 0, 1, 0, 0, 0, 0),
                                mu3 = c(0, 0, 0, 1, 0, 0, 0),
                                mu4 = c(0, 0, 0, 0, 1, 0, 0),
                                mu5 = c(0, 0, 0, 0, 0, 1, 0),
                                mu6 = c(0, 0, 0, 0, 0, 0, 1),
                                phi1 = c(0, 1, 0, 0, 0, 0, 0),
                                phi2 = c(0, 0, 1, 0, 0, 0, 0),
                                phi3 = c(0, 0, 0, 1, 0, 0, 0),
                                phi4 = c(0, 0, 0, 0, 1, 0, 0),
                                phi5 = c(0, 0, 0, 0, 0, 1, 0),
                                phi6 = c(0, 0, 0, 0, 0, 0, 1)))
return.level(fit3, return.period = c(2, 20, 50, 75, 100), qcov=v)

# Joint Return Levels
clusters = c(1, 2, 3, 4, 5, 6, 7)
prop_0 = 1 - mean(clim_wind2000$cluster1) - mean(clim_wind2000$cluster2) -
  mean(clim_wind2000$cluster3) - mean(clim_wind2000$cluster4) - 
  mean(clim_wind2000$cluster5) - mean(clim_wind2000$cluster6)
prop = c(prop_0, mean(clim_wind2000$cluster1), mean(clim_wind2000$cluster2),
         mean(clim_wind2000$cluster3), mean(clim_wind2000$cluster4),
         mean(clim_wind2000$cluster5), mean(clim_wind2000$cluster6))

cond_rl = function(z, k, fit) {
  params = fit$results$par
  if(k != 1) {
    mu = params["mu0"] + params[k]
    sigma = exp(params["phi0"] + params[7+k])
  } else {
    mu = params["mu0"]
    sigma = exp(params['phi0'])
  }
  xi = params["shape"]
  return(1 - pevd(z, loc = mu, scale = sigma, shape = xi, threshold = threshold[k], type = "PP"))
}

joint_rl = function(z, fit) {
  return(sum(prop * sapply(clusters, function(clusters) cond_rl(z, clusters, fit))))
}

# Bootstrap joint return levels, parallel compute for faster results
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

bootstrap_joint_rl = function(data, return_period) {
  sample_data = data[sample(1:nrow(data), replace = TRUE), ]
  prop_0 = 1 - mean(sample_data$cluster1) - mean(sample_data$cluster2) - mean(sample_data$cluster3) - 
    mean(sample_data$cluster4) - mean(sample_data$cluster5) - mean(sample_data$cluster6)
  prop = c(prop_0, mean(sample_data$cluster1), mean(sample_data$cluster2), mean(sample_data$cluster3),
           mean(sample_data$cluster4), mean(sample_data$cluster5), mean(sample_data$cluster6))
  
  cluster0 = sample_data %>% filter(low.level.wind < 12)
  cluster1 = sample_data %>% filter(cluster1 == 1)
  cluster2 = sample_data %>% filter(cluster2 == 1)
  cluster3 = sample_data %>% filter(cluster3 == 1)
  cluster4 = sample_data %>% filter(cluster4 == 1)
  cluster5 = sample_data %>% filter(cluster5 == 1)
  cluster6 = sample_data %>% filter(cluster6 == 1)
  
  prob = 0.95
  t0 = quantile(cluster0$surface_wind, prob = prob)
  t1 = quantile(cluster1$surface_wind, prob = prob)
  t2 = quantile(cluster2$surface_wind, prob = prob)
  t3 = quantile(cluster3$surface_wind, prob = prob)
  t4 = quantile(cluster4$surface_wind, prob = prob)
  t5 = quantile(cluster5$surface_wind, prob = prob)
  t6 = quantile(cluster6$surface_wind, prob = prob)
  fit = fevd(surface_wind, sample_data, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
             threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             scale.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, use.phi = TRUE, type="PP")
  
  clusters = 1:7
  return(uniroot(function(z) joint_rl(z, fit) - 1/return_period, lower = 0, upper = 100)$root)
}

results2000 <- foreach(i = c(2, 20, 50, 75, 100), .combine = rbind, 
                       .packages = c("dplyr", "extRemes")) %dopar% {
                         replicate(1000, bootstrap_joint_rl(clim_wind2000, i))
                       }

stopCluster(cl)

# Plot bootstrap CI
temp = results2000
lower_ci = c(quantile(temp[1, ], prob = 0.025), quantile(temp[2, ], prob = 0.025), 
             quantile(temp[3, ], prob = 0.025), quantile(temp[4, ], prob = 0.025),
             quantile(temp[5, ], prob = 0.025))
upper_ci = c(quantile(temp[1, ], prob = 0.975), quantile(temp[2, ], prob = 0.975), 
             quantile(temp[3, ], prob = 0.975), quantile(temp[4, ], prob = 0.975),
             quantile(temp[5, ], prob = 0.975))
estimate = c(uniroot(function(z) joint_rl(z, fit3) - 1/(2), lower = 0, upper = 100)$root,
             uniroot(function(z) joint_rl(z, fit3) - 1/(20), lower = 0, upper = 100)$root,
             uniroot(function(z) joint_rl(z, fit3) - 1/(50), lower = 0, upper = 100)$root,
             uniroot(function(z) joint_rl(z, fit3) - 1/(75), lower = 0, upper = 100)$root,
             uniroot(function(z) joint_rl(z, fit3) - 1/(100), lower = 0, upper = 100)$root)
temp_df = data.frame(
  return_period = c(2, 20, 50, 75, 100),
  lower_ci = lower_ci, 
  upper_ci = upper_ci,
  estimate = estimate
)

# Same as above but for 2090
clim_wind2090 = clim_wind2090 %>% mutate(cluster1 = case_when(low.level.wind >= 12 & atmos_stability1 >= 0.0019 & low.level.wind < 21 ~ 1,
                                                              TRUE ~ 0),
                                         cluster2 = case_when(low.level.wind >= 21 & atmos_stability1 >= 0.0019 ~ 1,
                                                              TRUE ~ 0),
                                         cluster3 = case_when(low.level.wind >= 12 & atmos_stability1 < 0.0019 & low.level.wind < 16 ~ 1,
                                                              TRUE ~ 0),
                                         cluster4 = case_when(low.level.wind >= 16 & atmos_stability1 < 0.0019 & low.level.wind < 22 & atmos_stability1 >= 941e-6 ~ 1,
                                                              TRUE ~ 0),
                                         cluster5 = case_when(low.level.wind >= 16 & low.level.wind < 22 & atmos_stability1 < 941e-6 ~ 1,
                                                              TRUE ~ 0),
                                         cluster6 = case_when(low.level.wind >= 22 & atmos_stability1 < 0.0019 ~ 1,
                                                              TRUE ~ 0))

cluster0 = clim_wind2090 %>% filter(low.level.wind < 12)
cluster1 = clim_wind2090 %>% filter(cluster1 == 1)
cluster2 = clim_wind2090 %>% filter(cluster2 == 1)
cluster3 = clim_wind2090 %>% filter(cluster3 == 1)
cluster4 = clim_wind2090 %>% filter(cluster4 == 1)
cluster5 = clim_wind2090 %>% filter(cluster5 == 1)
cluster6 = clim_wind2090 %>% filter(cluster6 == 1)

prob = 0.95
t0 = quantile(cluster0$surface_wind, prob = prob)
t1 = quantile(cluster1$surface_wind, prob = prob)
t2 = quantile(cluster2$surface_wind, prob = prob)
t3 = quantile(cluster3$surface_wind, prob = prob)
t4 = quantile(cluster4$surface_wind, prob = prob)
t5 = quantile(cluster5$surface_wind, prob = prob)
t6 = quantile(cluster6$surface_wind, prob = prob)
x = c(1, 2, 3, 4, 5, 6, 7)
threshold = c(t0, t1, t2, t3, t4, t5, t6)
temp = data.frame(x, threshold)

ggplot(temp, aes(x = x, y = threshold)) +
  geom_point(color = "darkgreen", size = 3) + geom_line() +
  labs(title = "Threshold Selection for each Cluster", x = "Cluster Index", 
       y = "Threshold (95th Percentile Winds)") +
  scale_x_continuous(breaks = 1:7) + 
  theme_minimal()

fit5 = fevd(surface_wind, clim_wind2090, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit6 = fevd(surface_wind, clim_wind2090, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit7 = fevd(surface_wind, clim_wind2090, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            scale.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, use.phi = TRUE, type="PP")
fit8 = fevd(surface_wind, clim_wind2090, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
            shape.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
lr.test(fit5, fit6)
lr.test(fit6, fit7)
lr.test(fit6, fit8)
plot(fit5)
plot(fit6)
plot(fit7)
plot(fit8)

v = make.qcov(fit7, vals = list(threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0),
                                mu1 = c(0, 1, 0, 0, 0, 0, 0),
                                mu2 = c(0, 0, 1, 0, 0, 0, 0),
                                mu3 = c(0, 0, 0, 1, 0, 0, 0),
                                mu4 = c(0, 0, 0, 0, 1, 0, 0),
                                mu5 = c(0, 0, 0, 0, 0, 1, 0),
                                mu6 = c(0, 0, 0, 0, 0, 0, 1),
                                phi1 = c(0, 1, 0, 0, 0, 0, 0),
                                phi2 = c(0, 0, 1, 0, 0, 0, 0),
                                phi3 = c(0, 0, 0, 1, 0, 0, 0),
                                phi4 = c(0, 0, 0, 0, 1, 0, 0),
                                phi5 = c(0, 0, 0, 0, 0, 1, 0),
                                phi6 = c(0, 0, 0, 0, 0, 0, 1)))
return.level(fit7, return.period = c(2, 20, 50, 75, 100), qcov=v)

clusters = c(1, 2, 3, 4, 5, 6, 7)
prop_0 = 1 - mean(clim_wind2090$cluster1) - mean(clim_wind2090$cluster2) -
  mean(clim_wind2090$cluster3) - mean(clim_wind2090$cluster4) - 
  mean(clim_wind2090$cluster5) - mean(clim_wind2090$cluster6)
prop = c(prop_0, mean(clim_wind2090$cluster1), mean(clim_wind2090$cluster2),
         mean(clim_wind2090$cluster3), mean(clim_wind2090$cluster4),
         mean(clim_wind2090$cluster5), mean(clim_wind2090$cluster6))

cond_rl = function(z, k, fit) {
  params = fit$results$par
  if(k != 1) {
    mu = params["mu0"] + params[k]
    sigma = exp(params["phi0"] + params[7+k])
  } else {
    mu = params["mu0"]
    sigma = exp(params['phi0'])
  }
  xi = params["shape"]
  return(1 - pevd(z, loc = mu, scale = sigma, shape = xi, threshold = threshold[k], type = "PP"))
}

joint_rl = function(z, fit) {
  return(sum(prop * sapply(clusters, function(clusters) cond_rl(z, clusters, fit))))
}

temp = results2090
lower_ci = c(quantile(temp[1, ], prob = 0.025), quantile(temp[2, ], prob = 0.025), 
             quantile(temp[3, ], prob = 0.025), quantile(temp[4, ], prob = 0.025),
             quantile(temp[5, ], prob = 0.025))
upper_ci = c(quantile(temp[1, ], prob = 0.975), quantile(temp[2, ], prob = 0.975), 
             quantile(temp[3, ], prob = 0.975), quantile(temp[4, ], prob = 0.975),
             quantile(temp[5, ], prob = 0.975))
estimate2090 = c(uniroot(function(z) joint_rl(z, fit7) - 1/(2), lower = 0, upper = 100)$root,
                 uniroot(function(z) joint_rl(z, fit7) - 1/(20), lower = 0, upper = 100)$root,
                 uniroot(function(z) joint_rl(z, fit7) - 1/(50), lower = 0, upper = 100)$root,
                 uniroot(function(z) joint_rl(z, fit7) - 1/(75), lower = 0, upper = 100)$root,
                 uniroot(function(z) joint_rl(z, fit7) - 1/(100), lower = 0, upper = 100)$root)

overlay_df = data.frame(
  return_period = c(2, 20, 50, 75, 100),
  lower_ci = lower_ci, 
  upper_ci = upper_ci,
  estimate = estimate2090
)

# Plot joint return levels bootstrapped for both 2000 and 2090
# Did not plot legend, 2000 is lightblue, 2090 is red
ggplot(temp_df, aes(x = return_period)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = estimate, color = "2000"), color = "blue", size = 0.8) +
  geom_point(aes(y = estimate), color = "darkblue", size = 1.5) +
  geom_ribbon(data = overlay_df, aes(ymin = lower_ci, ymax = upper_ci), fill = "red", alpha = 0.1) +
  geom_line(data = overlay_df, aes(x = return_period, y = estimate2090, color = "2090"), 
            color="firebrick", size = 0.8) +
  geom_point(data = overlay_df, aes(x = return_period, y = estimate2090), color = "darkblue", size = 1.5) +
  labs(
    title = "Return Level Plot with 95% Bootstrap Confidence Intervals",
    x = "Return Period (years)",
    y = "Wind Speed Return Level (m/s)"
  ) +
  theme_minimal()