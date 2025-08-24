library(extRemes)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(MASS)
library(rattle)
library(tibble)
library(rpart)
library(rpart.plot)

wind_speed = read.csv("pearson_wind_data.csv")
wind_speed$valid_time = as.Date(wind_speed$valid_time, format = "%Y-%m-%d")
wind_speed$Year = year(wind_speed$valid_time)
wind_speed$Month = month(wind_speed$valid_time)

# Yearly Mean Climatology
wind_speed = wind_speed %>% mutate(day_of_year = yday(valid_time))
yearly_mean_climatology = wind_speed %>% group_by(day_of_year) %>% 
  summarise(mean_wind_speed = mean(surface_wind))
ggplot(yearly_mean_climatology, aes(x = day_of_year, y = mean_wind_speed)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(60, 151, 244, 335),  # MAM, JJA, SON, DJF
             linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = max(yearly_mean_climatology$mean_wind_speed), label = "MAM", angle = 90, vjust = -0.5) +
  annotate("text", x = 152, y = max(yearly_mean_climatology$mean_wind_speed), label = "JJA", angle = 90, vjust = -0.5) +
  annotate("text", x = 244, y = max(yearly_mean_climatology$mean_wind_speed), label = "SON", angle = 90, vjust = -0.5) +
  annotate("text", x = 335, y = max(yearly_mean_climatology$mean_wind_speed), label = "DJF", angle = 90, vjust = -0.5) +
  labs(title = "Yearly Mean Climatology of Wind Speeds in Toronto",
       x = "Day of Year",
       y = "Mean Wind Speed (m/s)") +
  theme_minimal()

# Loess Curve
ggplot(wind_speed, aes(x = day_of_year, y = surface_wind)) + 
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", span = 0.1, color = "blue", size = 1, fill = "lightblue", se = TRUE) +
  labs(title = "LOWESS Smoothing by Day of Year",
       x = "Day of Year",
       y = "Wind Speed") + 
  theme_minimal() + 
  theme(axis.text.x = element_blank())

# Yearly Maximum Climatology
yearly_max_climatology = wind_speed %>% group_by(day_of_year) %>% 
  summarise(max_wind_speed = max(surface_wind))
ggplot(yearly_max_climatology, aes(x = day_of_year, y = max_wind_speed)) +
  geom_line(color = "blue") +
  geom_vline(xintercept = c(60, 151, 244, 335),  # MAM, JJA, SON, DJF
             linetype = "dashed", color = "red") +
  annotate("text", x = 60, y = max(yearly_max_climatology$max_wind_speed) - 0.6, label = "Spring", angle = 90, vjust = -0.5) +
  annotate("text", x = 152, y = max(yearly_max_climatology$max_wind_speed) - 0.6, label = "Summer", angle = 90, vjust = -0.5) +
  annotate("text", x = 244, y = max(yearly_max_climatology$max_wind_speed) - 0.6, label = "Autumn", angle = 90, vjust = -0.5) +
  annotate("text", x = 335, y = max(yearly_max_climatology$max_wind_speed) - 0.6, label = "Winter", angle = 90, vjust = -0.5) +
  
  labs(title = "Yearly Maximum Climatology of Wind Speeds in Toronto",
       x = "Day of Year",
       y = "Maximum Wind Speed (m/s)") +
  theme_minimal()

# Fit baseline threshold model with 98th percentile threshold
threshold = quantile(wind_speed$surface_wind, probs = 0.98)
fit = fevd(surface_wind, wind_speed, threshold = threshold, type = "GP")
plot(fit)
plot(fit, "trace")
return.level(fit, return.period = c(2, 20, 50, 100, 500), do.ci=TRUE)
ci(fit, type="return.level", method="proflik", return.period = 500, xrange = c(15, 24), verbose = TRUE)

# Threshold Plots
threshrange.plot(wind_speed$surface_wind, r = c(3, 12), nint = 25)
mrlplot(wind_speed$surface_wind, xlim=c(3, 12))

# Boxplots and violin plots of wind speed at each month
extreme_winds = wind_speed %>% filter(surface_wind > 8.5)

extreme_winds$Month <- factor(month.abb[extreme_winds$Month], levels = month.abb)
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


# Plot boxplots
ggplot(extreme_winds, aes(x = factor(Month), y = surface_wind, fill = factor(Month))) + geom_boxplot() +
  labs(title = "Extreme Wind Speeds above 8.5 m/s in Toronto",
       x = "Month",
       y = "Wind Speed (m/s)") +
  scale_fill_manual(values = month_colors) + 
  theme_minimal()

# Plot violin plots
ggplot(extreme_winds, aes(x = factor(Month), y = surface_wind, fill = factor(Month))) + geom_violin() +
  labs(title = "Wind Speed Violin Plots",
       x = "Month",
       y = "Wind Speed (m/s)") +
  theme_minimal()

# Regression Tree
extreme_winds = wind_speed %>% filter(surface_wind > 8.5)
extreme_winds = extreme_winds %>% mutate(summer_index = case_when(Month %in% c(6, 7, 8) ~ 1,
                                                                  TRUE ~ 0))
extreme_winds = subset(extreme_winds, select = -c(pressure_level, latitude, longitude, Year, Month, valid_time))
regress_anova = rpart(surface_wind ~ ., extreme_winds, method = "anova", 
                      control = rpart.control(cp=0, minsplit=25, minbucket=15, maxcompete=0, maxsurrogate=0))
regress_anova_prune = prune(regress_anova, cp = 0.01)
rpart.plot(regress_anova_prune, branch = 0.3, compress = TRUE, extra = 101)

# Create cluster covariates
wind_speed = wind_speed %>% mutate(cluster1 = case_when(low.level_wind > 22 & low.level_wind <= 33 ~ 1,
                                                        TRUE ~ 0),
                                   cluster2 = case_when(low.level_wind >= 33 & low.level_wind < 39 & thermal_wind_y_grad < 6.3 ~ 1,
                                                        TRUE ~ 0),
                                   cluster3 = case_when(low.level_wind >= 33 & low.level_wind < 39 & thermal_wind_y_grad >= 6.3 &
                                                          thermal_wind_x_grad > -0.32 ~ 1,
                                                        TRUE ~ 0),
                                   cluster4 = case_when(low.level_wind >= 33 & low.level_wind < 39 & thermal_wind_y_grad >= 6.3 &
                                                          thermal_wind_x_grad <= -0.32 ~ 1,
                                                        TRUE ~ 0),
                                   cluster5 = case_when(low.level_wind >= 39 & low.level_wind < 43 ~ 1,
                                                        TRUE ~ 0),
                                   cluster6 = case_when(low.level_wind >= 43 ~ 1,
                                                        TRUE ~ 0))

# Choose 95th percentile cluster winds as threshold
cluster0 = wind_speed %>% filter(low.level_wind <= 22)
cluster1 = wind_speed %>% filter(cluster1 == 1)
cluster2 = wind_speed %>% filter(cluster2 == 1)
cluster3 = wind_speed %>% filter(cluster3 == 1)
cluster4 = wind_speed %>% filter(cluster4 == 1)
cluster5 = wind_speed %>% filter(cluster5 == 1)
cluster6 = wind_speed %>% filter(cluster6 == 1)

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

# Fit multiple models, choose model using likelihood ratio test
fit9 = fevd(surface_wind, wind_speed, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
            threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit10 = fevd(surface_wind, wind_speed, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
             threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
fit11 = fevd(surface_wind, wind_speed, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
             threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             scale.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, use.phi = TRUE, type="PP")
fit12 = fevd(surface_wind, wind_speed, threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0), 
             threshold.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, 
             shape.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
lr.test(fit9, fit10)
lr.test(fit10, fit11)
lr.test(fit10, fit12)
plot(fit10)
plot(fit11)
plot(fit12)

# Plot surface wind with threshold selection at each cluster
temp <- data.frame(
  time = wind_speed$valid_time,
  wind_speed = as.numeric(wind_speed$surface_wind),
  threshold = c(fit10$threshold, rep(NA, length(wind_speed$surface_wind) - length(fit10$threshold)))  # pad with NA if needed
)

temp_long = data.frame(
  time = rep(temp$time[1:365], 2),
  y = c(temp$wind_speed[1:365], temp$threshold[1:365]),
  group = rep(c("Surface Wind", "Threshold (95th percentile)"), each = 365)
)

ggplot(temp_long, aes(x = time, y = y, color = group)) +
  geom_line() +
  labs(title = "Wind Speeds Plotted with Threshold Selection (Year 1979)", color = "Legend", 
       x = "Time", y = "Surface Wind Speed") +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.85),  # x and y coordinates inside plot
    legend.background = element_rect(fill = "white", color = "black")  # optional styling
  )

# Conditional return levels
v = make.qcov(fit10, vals = list(threshold = c(t0, t1-t0, t2-t0, t3-t0, t4-t0, t5-t0, t6-t0),
                                 mu1 = c(0, 1, 0, 0, 0, 0, 0),
                                 mu2 = c(0, 0, 1, 0, 0, 0, 0),
                                 mu3 = c(0, 0, 0, 1, 0, 0, 0),
                                 mu4 = c(0, 0, 0, 0, 1, 0, 0),
                                 mu5 = c(0, 0, 0, 0, 0, 1, 0),
                                 mu6 = c(0, 0, 0, 0, 0, 0, 1)))
return.level(fit10, return.period = c(2, 20, 50, 75, 100), qcov=v)

# Bootstrap conditional return levels for confidence interval
set.seed(123)
bootstrap_function = function(data, return_period) {
  sample_data = data[sample(1:nrow(data), replace = TRUE), ]
  cluster0 = sample_data %>% filter(low.level_wind <= 22)
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
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
  return(return.level(fit, return.period = c(return_period), qcov=v))
}

results = replicate(1000, bootstrap_function(wind_speed, 2))[, ,]
results20 = replicate(1000, bootstrap_function(wind_speed, 20))[, ,]
results50 = replicate(1000, bootstrap_function(wind_speed, 50))[, ,]
results75 = replicate(1000, bootstrap_function(wind_speed, 75))[, ,]
results100 = replicate(1000, bootstrap_function(wind_speed, 100))[, ,]

# Define confidence intervals, I used temp variable for easier copy-pasting, too lazy to write for loops
temp = results
lower_ci2 = c(quantile(temp[1, ], 0.025), quantile(temp[2, ], 0.025), quantile(temp[3, ], 0.025),
              quantile(temp[4, ], 0.025), quantile(temp[5, ], 0.025), quantile(temp[6, ], 0.025), 
              quantile(temp[7, ], 0.025))
upper_ci2 = c(quantile(temp[1, ], 0.975), quantile(temp[2, ], 0.975), quantile(temp[3, ], 0.975),
              quantile(temp[4, ], 0.975), quantile(temp[5, ], 0.975), quantile(temp[6, ], 0.975), 
              quantile(temp[7, ], 0.975))

temp = results20
lower_ci20 = c(quantile(temp[1, ], 0.025), quantile(temp[2, ], 0.025), quantile(temp[3, ], 0.025),
               quantile(temp[4, ], 0.025), quantile(temp[5, ], 0.025), quantile(temp[6, ], 0.025), 
               quantile(temp[7, ], 0.025))
upper_ci20 = c(quantile(temp[1, ], 0.975), quantile(temp[2, ], 0.975), quantile(temp[3, ], 0.975),
               quantile(temp[4, ], 0.975), quantile(temp[5, ], 0.975), quantile(temp[6, ], 0.975), 
               quantile(temp[7, ], 0.975))

temp = results50
lower_ci50 = c(quantile(temp[1, ], 0.025), quantile(temp[2, ], 0.025), quantile(temp[3, ], 0.025),
               quantile(temp[4, ], 0.025), quantile(temp[5, ], 0.025), quantile(temp[6, ], 0.025), 
               quantile(temp[7, ], 0.025))
upper_ci50 = c(quantile(temp[1, ], 0.975), quantile(temp[2, ], 0.975), quantile(temp[3, ], 0.975),
               quantile(temp[4, ], 0.975), quantile(temp[5, ], 0.975), quantile(temp[6, ], 0.975), 
               quantile(temp[7, ], 0.975))

temp = results75
lower_ci75 = c(quantile(temp[1, ], 0.025), quantile(temp[2, ], 0.025), quantile(temp[3, ], 0.025),
               quantile(temp[4, ], 0.025), quantile(temp[5, ], 0.025), quantile(temp[6, ], 0.025), 
               quantile(temp[7, ], 0.025))
upper_ci75 = c(quantile(temp[1, ], 0.975), quantile(temp[2, ], 0.975), quantile(temp[3, ], 0.975),
               quantile(temp[4, ], 0.975), quantile(temp[5, ], 0.975), quantile(temp[6, ], 0.975), 
               quantile(temp[7, ], 0.975))

temp = results100
lower_ci100 = c(quantile(temp[1, ], 0.025), quantile(temp[2, ], 0.025), quantile(temp[3, ], 0.025),
                quantile(temp[4, ], 0.025), quantile(temp[5, ], 0.025), quantile(temp[6, ], 0.025), 
                quantile(temp[7, ], 0.025))
upper_ci100 = c(quantile(temp[1, ], 0.975), quantile(temp[2, ], 0.975), quantile(temp[3, ], 0.975),
                quantile(temp[4, ], 0.975), quantile(temp[5, ], 0.975), quantile(temp[6, ], 0.975), 
                quantile(temp[7, ], 0.975))

# Plot of bootstrapped conditional return levels
temp = return.level(fit10, return.period = c(2, 20, 50, 75, 100), qcov=v)
temp1 = data.frame(
  x = rep(1:7, 5),
  y = c(temp[, 1], temp[, 2], temp[, 3], temp[, 4], temp[, 5]),
  group = rep(c("2 year Return Period", "20 year Return Period", "50 year Return Period",
                "75 year Return Period", "100 year Return Period"), each = 7),
  lower_ci = c(lower_ci2, lower_ci20, lower_ci50, lower_ci75, lower_ci100),
  upper_ci = c(upper_ci2, upper_ci20, upper_ci50, upper_ci75, upper_ci100)
)

temp1$group = factor(temp1$group, levels = c("2 year Return Period", "20 year Return Period", 
                                             "50 year Return Period", "75 year Return Period", 
                                             "100 year Return Period"))

ggplot(temp1, aes(x = x, y = y, group = group)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = group), alpha = 0.1) +
  geom_line(aes(color = group), size = 0.7) +
  labs(title = "Bootstrapped Conditional Return Level Plot", x = "Cluster Index", y = "Wind Speed (m/s)", 
       fill = "Return Period", color = "Return Period") +
  scale_x_continuous(breaks = 1:7) +
  theme_minimal() + 
  theme(
    legend.position = c(0.82, 0.25),  # x and y coordinates inside plot
    legend.background = element_rect(fill = "white", color = "black")  # optional styling
  )

# Bootstrap joint return levels, use parallel computing for faster bootstrapping
library(doParallel)

cl <- makeCluster(4)
registerDoParallel(cl)

bootstrap_joint_rl = function(data, return_period) {
  sample_data = data[sample(1:nrow(data), replace = TRUE), ]
  prop_0 = 1 - mean(sample_data$cluster1) - mean(sample_data$cluster2) - mean(sample_data$cluster3) - 
    mean(sample_data$cluster4) - mean(sample_data$cluster5) - mean(sample_data$cluster6)
  prop = c(prop_0, mean(sample_data$cluster1), mean(sample_data$cluster2), mean(sample_data$cluster3),
           mean(sample_data$cluster4), mean(sample_data$cluster5), mean(sample_data$cluster6))
  
  cluster0 = sample_data %>% filter(low.level_wind <= 22)
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
             location.fun = ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, type="PP")
  
  clusters = 1:7
  return(uniroot(function(z) joint_rl(z, fit) - 1/return_period, lower = 0, upper = 100)$root)
}

results <- foreach(i = c(2, 20, 50, 75, 100), .combine = rbind, 
                   .packages = c("dplyr", "extRemes")) %dopar% {
                     replicate(1000, bootstrap_joint_rl(wind_speed, i))
                   }

stopCluster(cl)

