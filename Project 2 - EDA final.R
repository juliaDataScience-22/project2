library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(Amelia)


#----------------------
# 1. Summary Analysis
#----------------------

#load data & re-order columns
data_raw <- read_excel("StudentData.xlsx")
data_raw <- data_raw %>% 
  select(PH, everything())

summary(data_raw)

#----------
# 2. Missing Data
#----------

na_list <- sapply(data_raw, function(x) sum(is.na(x)))

na_df <- data.frame(
  variable = names(na_list),
  count_na = unlist(na_list)) %>% 
  arrange(desc(count_na))

na_df %>% 
  summarize(sum(count_na))

# top ten
na_topten <- head(na_df,10)
na_topten

# bar plot
na_plot2 <- na_df %>% 
  ggplot(aes(x = reorder(variable,count_na,decreasing = TRUE), 
             y = count_na)) +
  geom_bar (stat="identity", fill = "lightblue") +
  ggtitle ("Count of Missing Values by Variable") +
  xlab ("Variable") +
  ylab("Count of NA Values") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1))

na_plot2
ggsave("missing.png", plot = na_plot2, width = 8, height = 5)

# densityt plot from amelia package
missmap(data_raw, x.cex = 0.7, gap.xaxis = 0, 
        main = "Missing Data: Density Map by Variable")

#----------
# 3. Frequency  
#----------

# Categorical variable

brand_plot <-data_raw %>% 
  ggplot(aes(x=`Brand Code`)) + 
  geom_bar(fill = "cornsilk3") +
  ggtitle("Distribution by Brand Code") +
  scale_y_continuous(breaks = pretty_breaks(10))

brand_plot
ggsave("brand_histogram.png", plot = brand_plot, width = 8, height = 4)


# Numeric variables
data_melt <- melt(data_raw)

hist_plot <- data_melt %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(fill = "seagreen") + 
  facet_wrap(~variable, scales='free_x') +
  theme(
    title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size =12, face = "bold"),
    axis.title = element_text(size = 12)) +
  ggtitle("Frequency by Numeric Variable: All Brand Codes")

hist_plot
ggsave("histogram.png", plot = hist_plot, width = 16, height = 12)

# -------------
# 4. Correlation Analysis
# -------------

cor_vars <- select(data_raw, -`Brand Code`)
cor_matrix <- cor(cor_vars, use ="pairwise.complete.obs")
plot_matrix <- melt(cor_matrix)

# extract most highly correlated pairs (positive or negative)
highly_cor <- filter(plot_matrix, (value !=1 & (value >.6 | value < -.6))) %>%   arrange(desc(value)) %>% 
  distinct(value, .keep_all = TRUE)

highly_cor

# plot all 
plot_correlations <- plot_matrix %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1),
        axis.text.y = element_text(size = 14)) 

plot_correlations
ggsave("correlations.png", plot = plot_correlations, width = 12, height = 8)

# -------------
# 5. Boxplots for Outliers - not used
# -------------

# Box plots for detecting outliers in numerical variables
plot_outlier <- data_melt %>% 
  ggplot(aes(y = value)) + 
  geom_boxplot() + 
  facet_wrap(~variable, scales = 'free_y') +
  ggtitle("Box Plots for Outliers Detection")

plot_outlier
ggsave("outliers.png", plot = plot_outlier, width = 16, height = 12)

# -------------
# 6. Scatterplots with Predictor Outliers Highlighted
# -------------

# numeric predictors only
numeric_predictors <- names(data_raw)[sapply(data_raw, is.numeric) & 
                                        names(data_raw) != "PH"]

# outlier function
is_outlier <- function(x) {
  return(abs(x - mean(x, na.rm = TRUE)) > 2 * sd(x, na.rm = TRUE))
}

# loop
for (var2 in numeric_predictors) {
  var2_fixed <- paste0("`", var2, "`")  
  data_raw$outlier <- is_outlier(data_raw[[var2]])
  
  scatter1 <- ggplot(data_raw, aes_string(x = var2_fixed, y = "`PH`")) +
    geom_point() +
    geom_point(data = subset(data_raw, outlier == TRUE), aes_string(x = var2_fixed, y = "`PH`"), color = 'red') +
    labs(title = paste("Scatter Plot of", var2, "vs. PH with Outliers Highlighted"),
         x = var2,
         y = "PH") +
    theme(title = element_text(size = 16, face = "bold"),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14)) 
  print(scatter1)
  ggsave(paste0("scatter-",var2_fixed,".png"), plot = scatter1, width = 8, height = 6)
}
