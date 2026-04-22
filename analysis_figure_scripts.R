
##linear regression analysis and plot - Figure 4A##
library(ggplot2)

df <- read.csv("cv_manual_count_counter_block.csv")

fit <- lm(cv_count ~ Count, data = df)

r_squared <- summary(fit)$r.squared

plot <- ggplot(df, aes(x = Count, y = cv_count)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  labs(
    x = "Manual Counts",
    y = "Computer Vision Counts"
  ) +
  annotate(
    "text",
    x = max(df$Count, na.rm = TRUE) + 20,
    y = max(df$cv_count, na.rm = TRUE) - 30,
    label = paste("R² =", round(r_squared, 2)),
    hjust = 1.1, vjust = 1.5, size = 5
  ) +
  theme_minimal(base_size = 14)

print(plot)



##Bland-Altman analysis and plot - Figure 4B##
library(ggplot2)
library(dplyr)

df <- read.csv("cv_manual_count_counter_block.csv")

#customized Bland-Altman plot
make_bland_altman_plot <- function(data, x_col, y_col) {
  diff_xy <- data[[x_col]] - data[[y_col]]
  mean_xy <- (data[[x_col]] + data[[y_col]]) / 2
  
  bias   <- mean(diff_xy, na.rm = TRUE)
  sd_d   <- sd(diff_xy,   na.rm = TRUE)
  loa_lo <- bias - 1.96 * sd_d
  loa_hi <- bias + 1.96 * sd_d
  
  plot_df <- data.frame(mean_xy = mean_xy, diff_xy = diff_xy)
  
  p <- ggplot(plot_df, aes(x = mean_xy, y = diff_xy)) +
    geom_point(alpha = 0.2) +  # semi-transparent points
    geom_hline(yintercept = bias,    color = "Red", size = 1.2) + # bias line
    geom_hline(yintercept = loa_lo,  linetype = "dashed", color = "blue", size = 1.2) +
    geom_hline(yintercept = loa_hi,  linetype = "dashed", color = "blue", size = 1.2) +
    labs(
      x = "Average of Manual and CV count",
      y = "Manual − CV"
    ) +
    theme_minimal(base_size = 13)
  
  list(plot = p, stats = list(bias = bias, sd = sd_d, loa_low = loa_lo, loa_high = loa_hi))
}

res <- make_bland_altman_plot(df, "Count", "cv_count")
print(res$plot)
res$stats



##Counter bias analysis - Figure 5A##
library(tidyverse)
library(patchwork)
library(multcompView)

df <- read.csv("cv_manual_count_counter_block.csv")

unknown_vals <- c("", "unknown", "Unknown", "UNK", NA)

df_clean <- df %>%
  filter(!is.na(Counter), !(Counter %in% unknown_vals)) %>%
  mutate(
    diff        = Count - cv_count,         
    Counter     = factor(Counter),          
    Counter_num = as.integer(Counter),      
    Block       = as.integer(as.character(Block))
  )

pal <- c("1" = "#0072B2", "2" = "#E69F00", "3" = "#009E73")

#Tukey letters
aov_model <- aov(diff ~ Counter, data = df_clean)
tuk       <- TukeyHSD(aov_model, "Counter")
letters_l <- multcompLetters4(aov_model, tuk)

letters_df <- data.frame(
  Counter = names(letters_l$Counter$Letters),
  letters = letters_l$Counter$Letters,
  row.names = NULL,
  stringsAsFactors = FALSE
)

map_df <- df_clean %>% distinct(Counter, Counter_num)
letters_df <- letters_df %>% left_join(map_df, by = "Counter")

yrange <- range(df_clean$diff, na.rm = TRUE)
bump   <- 0.05 * diff(yrange)
label_pos <- df_clean %>%
  group_by(Counter_num) %>%
  summarise(y = max(diff, na.rm = TRUE) + bump, .groups = "drop") %>%
  left_join(letters_df, by = "Counter_num")

pA <- ggplot(df_clean, aes(x = factor(Counter_num), y = diff, fill = factor(Counter_num), color = factor(Counter_num))) +
  geom_boxplot(width = 0.8, outlier.shape = NA, alpha = 1) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 4) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D55E00", linewidth = 1) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_color_manual(values = pal, guide = "none") +
  labs(
    x = "Human Counter",
    y = "Manual - CV count"
  ) +
  # significance letters
  geom_text(data = label_pos, aes(x = factor(Counter_num), y = y, label = letters),y = 200, 
            inherit.aes = FALSE, vjust = 0, fontface = "bold", size = 6) +
  theme_minimal(base_size = 13)+
  theme(
    axis.text.x = element_text(size = 16),   # increase tick label size
    axis.title.x = element_text(size = 18),   # increase "Block" label size
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 14)
  )

print(pA)

##Counter bias across blocks - Figure 5B##

library(multcompView)

#Tukey letters per Block
letters_list <- df_clean %>%
  group_by(Block) %>%
  do({
    mod <- aov(diff ~ Counter, data = .)
    tuk <- TukeyHSD(mod, "Counter")
    lett <- multcompLetters4(mod, tuk)
    tibble(Counter = names(lett$Counter$Letters),
           letters = lett$Counter$Letters)
  })

letters_list <- letters_list %>%
  left_join(df_clean %>% distinct(Counter, Counter_num, Block), 
            by = c("Counter", "Block")) %>%
  group_by(Block, Counter_num) %>%
  summarise(letters = first(letters), .groups = "drop")

label_positions <- df_clean %>%
  group_by(Block, Counter_num) %>%
  summarise(y = max(diff, na.rm = TRUE) + 1, .groups = "drop") %>%
  left_join(letters_list, by = c("Block", "Counter_num"))

letters_list <- df_clean %>%
  group_by(Block) %>%
  do({
    mod <- aov(diff ~ Counter, data = .)
    tuk <- TukeyHSD(mod, "Counter")
    lett <- multcompLetters4(mod, tuk)
    tibble(Counter = names(lett$Counter$Letters),
           letters = lett$Counter$Letters)
  }) %>%
  ungroup()

letters_list %>%
  arrange(Block, Counter)

pal <- c("1" = "#0072B2", "2" = "#E69F00", "3" = "#009E73")

pB <- ggplot(df_clean, aes(x = Block, y = diff,
                           group = factor(Counter_num),
                           color = factor(Counter_num))) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.6),
              alpha = 0.2, size = 4) +
  stat_summary(fun = mean, geom = "line", size = 1,
               position = position_dodge(width = 0.6)) +
  stat_summary(fun = mean, geom = "point", size = 3, shape = 21, fill = "white",
               position = position_dodge(width = 0.6)) +
  scale_color_manual(values = pal, name = "Counter") +
  scale_x_continuous(breaks = sort(unique(df_clean$Block))) +
  labs(
    x = "Experimental Block",
    y = "Manual - CV count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x     = element_text(size = 16),
    axis.title.x    = element_text(size = 18),
    axis.title.y    = element_text(size = 18),
    axis.text.y     = element_text(size = 14),
    legend.text     = element_text(size = 14),
    legend.title    = element_text(size = 16),
    legend.key.size = unit(1.2, "cm")
  )

print(pB)


##Compared to ground truth analysis - Figure3##
library(tidyverse)
library(ggpubr)

df <- read.csv("heldout_manual_vs_cv_50test.csv")

df <- df %>%
  mutate(
    manual_diff = manual_count - gt_count,
    cv_diff     = cv_count_heldout     - gt_count
  )

df_long <- df %>%
  select(manual_diff, cv_diff) %>%
  pivot_longer(cols = everything(),
               names_to = "method",
               values_to = "diff") %>%
  mutate(method = recode(method,
                         manual_diff = "Manual − GT",
                         cv_diff     = "CV − GT"))

mae_values <- df_long %>%
  group_by(method) %>%
  summarise(MAE = mean(abs(diff), na.rm = TRUE))
mae_values <- df %>%
  summarise(
    MAE_manual_GT = mean(abs(manual_count - gt_count), na.rm = TRUE),
    MAE_CV_GT     = mean(abs(cv_count_heldout - gt_count), na.rm = TRUE)
  )

print(mae_values)

pal <- c("Manual − GT" = "#56B4E9",   
         "CV − GT"     = "#F0E442")   


p <- ggplot(df_long, aes(x = method, y = diff, fill = method)) +
  geom_boxplot(width = 0.55, outlier.shape = NA, alpha = 0.8) +
  geom_jitter(aes(color = method),   # <- map color to method
              width = 0.15, alpha = 0.9, size = 2.8,shape = 21, stroke = 0.6, color = "gray30") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#D55E00", linewidth = 1) +
  scale_fill_manual(values = pal, guide = "none") +
  scale_color_manual(values = pal, guide = "none") +
  labs(
    x = "",
    y = "Count difference"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    plot.caption     = element_text(size = 14, hjust = 0.5, margin = margin(t = 10)),
    axis.title.y     = element_text(size = 16),
    axis.text.x      = element_text(size = 16, face = "bold", margin = margin(t = 8)),
    axis.text.y      = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

print(p)





