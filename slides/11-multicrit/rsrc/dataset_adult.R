# Used in: slides/11-multicrit/slides-multicrit-4-practical.tex
#
# Loads a compact Adult data snapshot and creates exploratory plots for the
# fairness example: income shares by race/education and age distributions by sex.

library(data.table)
library(ggplot2)
library(scales)

set.seed(1L)

output_dir = "../figure"
adult_cache_file = "dataset_adult_summary.rds"
adult_openml_csv = "https://www.openml.org/data/get_csv/1595261/adult.csv"

theme_set(theme_bw(base_size = 10))

save_plot = function(plot, filename, width, height) {
  ggsave(file.path(output_dir, filename), plot, width = width, height = height, dpi = 300)
}

load_adult_data = function() {
  if (file.exists(adult_cache_file)) {
    adult = readRDS(adult_cache_file)
    if ("education_num" %in% names(adult)) {
      adult
    } else {
      file.remove(adult_cache_file)
      load_adult_data()
    }
  } else {
    adult = fread(adult_openml_csv, select = c("age", "education", "education-num", "race", "sex", "class"))
    setnames(adult, c("education-num", "class"), c("education_num", "income"))
    saveRDS(adult, adult_cache_file, version = 2)
    adult
  }
}

plot_income_share = function(data, group_col, x_lab) {
  plot_data = data[, .N, by = c("sex", group_col, "income")]
  setnames(plot_data, group_col, "group")
  plot_data[, share := N / sum(N), by = .(sex, group)]
  plot_data[, label := fifelse(share >= 0.15, percent(share, accuracy = 1), "")]

  ggplot(plot_data, aes(x = group, y = share, fill = income)) +
    geom_col(width = 0.75, color = "white", linewidth = 0.15) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.6, color = "white") +
    facet_grid(sex ~ ., scales = "free_y", space = "free_y") +
    coord_flip() +
    scale_y_continuous(labels = label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = c("<=50K" = "#4C78A8", ">50K" = "#F58518")) +
    labs(x = x_lab, y = "Share", fill = "Income") +
    theme(legend.position = "bottom")
}

adult = load_adult_data()
setDT(adult)
adult[, education := factor(education, levels = unique(adult[order(education_num)]$education))]

race_plot = plot_income_share(adult, "race", "Race")
education_plot = plot_income_share(adult, "education", "Education")

age_plot = ggplot(adult, aes(x = age, fill = income)) +
  geom_histogram(binwidth = 1L, color = "black", linewidth = 0.1, alpha = 0.75, position = "identity") +
  facet_grid(. ~ sex) +
  scale_fill_manual(values = c("<=50K" = "#4C78A8", ">50K" = "#F58518")) +
  labs(x = "Age", y = "Count", fill = "Income") +
  theme(legend.position = "bottom")

if (interactive()) {
  print(race_plot)
  print(education_plot)
  print(age_plot)
}

save_plot(race_plot, "dataset_adult_race.png", width = 3, height = 6)
save_plot(education_plot, "dataset_adult_education.png", width = 5, height = 6)
save_plot(age_plot, "dataset_adult_age_sex.png", width = 5.5, height = 2.5)
