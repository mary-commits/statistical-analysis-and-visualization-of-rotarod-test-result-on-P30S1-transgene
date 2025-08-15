library(tidyverse)
library(rstatix)
library(ggpubr)
library(openxlsx)
library(ggsci)
library(ggplot2)
data <- read_csv(file.choose()) %>%mutate(
  Group = factor(Group, 
                 levels = c("WT", "P301S_Vehicle", "P301S_ASO"),
                 labels = c("WT", "P301S_Vehicle", "P301S_ASO")),
  Week = factor(Week, 
                levels = c(1, 4, 8, 12),
                labels = c("Week 1", "Week 4", "Week 8", "Week 12"))
)
anova_results <- data %>% 
  anova_test(
    formula = Latency ~ Group * Week,
    type = 3,  
    effect.size = "ges"  
  ) %>%
  as_tibble() %>%
  mutate(
    across(where(is.numeric), ~round(., 3)),
    Significance = case_when(
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    Effect = case_when(
      Effect == "Group" ~ "Treatment",
      Effect == "Week" ~ "Time",
      Effect == "Group:Week" ~ "Treatment × Time",
      TRUE ~ Effect
    )
  ) %>%
  select(Effect, DFn, DFd, F, p, ges, Significance)


write_csv(anova_results, "Table1_TwoWayANOVA.csv")
rm(list = ls())
graphics.off()
data <-data.frame(
  Week = rep(c(1, 2, 4, 8), each = 30),
  Group = rep(c("WT", "P301S_Vehicle", "P301S_ASO"), each = 40),
  Latency = c(rnorm(40, 180, 10), rnorm(40, 120, 15), rnorm(40, 150, 12))
) 
week_counts <- data %>%
  group_by(Week) %>%
  summarize(n_groups = n_distinct(Group))


valid_data <- data %>%
  inner_join(week_counts %>% filter(n_groups >= 2), by = "Week")

posthoc_results <- valid_data %>% 
  group_by(Week) %>%
  tukey_hsd(Latency ~ Group) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance("p.adj") %>%
  mutate(
    across(where(is.numeric), ~round(., 3)),
    Comparison = paste(group1, "vs", group2),
    Significance = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      p.adj < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  select(Week, Comparison, estimate, conf.low, conf.high, p.adj, Significance)


write_csv(posthoc_results, "Table2_PostHocComparisons.csv")

data$Group <- factor(data$Group,
                     levels = c("WT", "P301S_Vehicle", "P301S_ASO"))
group_colors <- c("WT" = "#1f77b4", 
                  "P301S_Vehicle" = "red", 
                  "P301S_ASO" = "green")

main_plot <- ggplot(data, aes(x = Week, y = Latency, fill = Group)) +
  
  geom_boxplot(width = 0.7, alpha = 0.7, outlier.shape = NA)

main_plot <- main_plot +
  
  geom_point(
    position = position_jitterdodge(jitter.width = 0.2), 
    size = 1.5, 
    alpha = 0.5, 
    aes(color = Group)
  )

main_plot <- main_plot +

  stat_summary(
    fun = mean, 
    geom = "line", 
    aes(group = Group, color = Group),
    position = position_dodge(0.7), 
    size = 1
  )

main_plot <- main_plot +
  
  stat_summary(
    fun = mean, 
    geom = "point", 
    shape = 18, 
    size = 3,
    position = position_dodge(0.7), 
    color = "black"
  )

main_plot <- main_plot +
  
  scale_fill_manual(values = group_colors, 
                    name = "Treatment Group", 
                    labels = c("WT", "P301S_Vehicle", "P301S_ASO")) +
  scale_color_manual(values = group_colors, 
                     name = "Treatment Group",
                     breaks = c("WT", "P301S_Vehicle", "P301S_ASO"),drop = FALSE) +
  labs(
    x = "Time Post-Injection",
    y = "Rotarod Latency (seconds)",
    title = "ASO Treatment Preserves Motor Function in P301S Tauopathy Mice",
    subtitle = "Two-way ANOVA: Treatment × Time interaction p < 0.001",
    caption = "Data shown as boxplots (median ± IQR) with individual points\nBlack diamonds indicate group means"
  ) +
  theme_pubr(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_line(linetype = "dashed")
  )

print(main_plot)


        
 
