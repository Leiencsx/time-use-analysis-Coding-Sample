################################################################################
# Time Use Survey Analysis - ENUT 2021 (Coding Sample)
# Author: Shixuan Cao
# Date: 2025-02-21
# Description: Analysis of time allocation across different activities by 
#              demographic groups (age, sex, marital status)
################################################################################

# ==============================================================================
# 1. SETUP AND INITIALIZATION
# ==============================================================================

# 1.1 Install and load logging package
# ------------------------------------------------------------------------------
if (!require("logr")) install.packages("logr")
library(logr)

# Create log file for tracking analysis
log_open("time_use_analysis.log")

# 1.2 Load required packages
# ------------------------------------------------------------------------------
required_packages <- c("haven", "readxl", "dplyr", "tidyr", "ggplot2")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ==============================================================================
# 2. DATA LOADING AND PREPROCESSING
# ==============================================================================

# 2.1 Load raw data files
# ------------------------------------------------------------------------------
# Load diary data (individual time use records)
diario_data <- read_dta("enut2021_diario.dta")

# Load activity classification (activity codes and types)
cautal_data <- read_excel(
  "enut2021_cautal.xlsx",
  range = "A3:B60"
)

# Rename columns for clarity
colnames(cautal_data) <- c("actividad_type", "actividad_code")

# 2.2 Transform diary data to long format
# ------------------------------------------------------------------------------
# Purpose: Convert wide format (actividad_1, actividad_2, actividad_3) 
# to long format for easier analysis of multiple simultaneous activities

diario_data_long <- diario_data %>%
  pivot_longer(
    cols = starts_with("actividad_") & 
           !starts_with("actividad_hora") & 
           !starts_with("actividad_minuto"),
    names_to = "actividad_source",
    values_to = "actividad_code"
  ) %>%
  filter(!is.na(actividad_code))  # Remove rows with missing activity codes

# 2.3 Merge activity classifications
# ------------------------------------------------------------------------------
# Join activity type descriptions to activity codes
diario_data_long <- diario_data_long %>%
  left_join(cautal_data, by = "actividad_code")

# 2.4 Calculate time slot information
# ------------------------------------------------------------------------------
# Note: Each time slot represents 10 minutes of activity

cleaned_diario_data <- diario_data_long %>%
  mutate(
    # Start time
    start_hora = actividad_hora,
    start_minuto = actividad_minuto,
    
    # End time (10 minutes later)
    end_hora = ifelse(
      actividad_minuto + 10 >= 60,
      start_hora + 1,
      start_hora
    ),
    end_minuto = ifelse(
      start_minuto + 10 >= 60,
      (start_minuto + 10) %% 60,
      start_minuto + 10
    ),
    
    # Duration
    time_slot_duracion_minuto = 10
  )

# 2.5 Export cleaned data
# ------------------------------------------------------------------------------
write_dta(cleaned_diario_data, "cleaned_diario_data.dta")

cat("Data preprocessing completed.\n")

# ==============================================================================
# 3. LOAD BASE DATA FOR ANALYSIS
# ==============================================================================

base_data <- read_dta("enut2021_base.dta")

# ==============================================================================
# 4. CALCULATE OVERALL AVERAGE TIME SPENT ON ACTIVITIES
# ==============================================================================

# Note on average time calculations:
# - Average Social Time = Total time / Total population
#   (includes non-participants, shows societal average)
# - Average Per Participant = Total time / Number of participants
#   (shows average among only those who did the activity)

# 4.1 Paid Work
# ------------------------------------------------------------------------------
average_social_time_paid <- sum(
  base_data$tss_grangrupo_ocupacionyautocons * base_data$wper
) / sum(base_data$wper)

average_per_participant_paid <- sum(
  base_data$tss_grangrupo_ocupacionyautocons * base_data$wper
) / sum(base_data$tp_grupo_trabajoocupacion * base_data$wper)

cat(sprintf(
  "Paid Work:\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_paid,
  average_per_participant_paid
))

# 4.2 Domestic Work
# ------------------------------------------------------------------------------
average_social_time_domestic <- sum(
  base_data$tss_grupo_domestico * base_data$wper
) / sum(base_data$wper)

average_per_participant_domestic <- sum(
  base_data$tss_grupo_domestico * base_data$wper
) / sum(base_data$tp_grupo_domestico * base_data$wper)

cat(sprintf(
  "Domestic Work:\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_domestic,
  average_per_participant_domestic
))

# 4.3 Childcare (ages 0-13)
# ------------------------------------------------------------------------------
# Create composite childcare variable
base_data$tss_total_0a13 <- 
  base_data$tss_act_cuidado_personal_0a13 +
  base_data$tss_act_cuidado_traslados_0a13 +
  base_data$tss_act_cuidado_otros_0a13 +
  base_data$tss_act_cuidado_apoyo_0a13

# Create participation indicator
base_data$tp_act_cuidado_0A13 <- ifelse(
  base_data$tss_total_0a13 == 0, 
  0, 
  1
)

average_social_time_child <- sum(
  base_data$tss_total_0a13 * base_data$wper
) / sum(base_data$wper)

average_per_participant_child <- sum(
  base_data$tss_total_0a13 * base_data$wper
) / sum(base_data$tp_act_cuidado_0A13 * base_data$wper)

cat(sprintf(
  "Childcare (0-13):\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_child,
  average_per_participant_child
))

# 4.4 Caring for Household Members
# ------------------------------------------------------------------------------
average_social_time_carhousehold <- sum(
  base_data$tss_grupo_cuidado * base_data$wper
) / sum(base_data$wper)

average_per_participant_carhousehold <- sum(
  base_data$tss_grupo_cuidado * base_data$wper
) / sum(base_data$tp_grupo_cuidado * base_data$wper)

cat(sprintf(
  "Household Care:\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_carhousehold,
  average_per_participant_carhousehold
))

# 4.5 Personal Activities
# ------------------------------------------------------------------------------
average_social_time_peract <- sum(
  base_data$tss_grangrupo_personales * base_data$wper
) / sum(base_data$wper)

average_per_participant_peract <- sum(
  base_data$tss_grangrupo_personales * base_data$wper
) / sum(base_data$tp_grangrupo_personales * base_data$wper)

cat(sprintf(
  "Personal Activities:\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_peract,
  average_per_participant_peract
))

# 4.6 Unpaid Work
# ------------------------------------------------------------------------------
average_social_time_unpaid <- sum(
  base_data$tss_grangrupo_tnr * base_data$wper
) / sum(base_data$wper)

average_per_participant_unpaid <- sum(
  base_data$tss_grangrupo_tnr * base_data$wper
) / sum(base_data$tp_grangrupo_tnr * base_data$wper)

cat(sprintf(
  "Unpaid Work:\n  Social Average: %.2f min\n  Participant Average: %.2f min\n\n",
  average_social_time_unpaid,
  average_per_participant_unpaid
))

# ==============================================================================
# 5. CALCULATE GROUPED AVERAGES BY DEMOGRAPHICS
# ==============================================================================

# 5.1 Paid Work by Age, Sex, and Marital Status
# ------------------------------------------------------------------------------
average_paid_work <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_paid = sum(tss_grangrupo_ocupacionyautocons * wper) / 
                               sum(wper),
    average_per_participant_paid = sum(tss_grangrupo_ocupacionyautocons * wper) / 
                                   sum(tp_grangrupo_ocupacionyautoconsu * wper),
    .groups = "drop"
  )

# 5.2 Domestic Work
# ------------------------------------------------------------------------------
average_domestic_work <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_domestic_work = sum(tss_grupo_domestico * wper) / 
                                        sum(wper),
    average_per_participant_domestic_work = sum(tss_grupo_domestico * wper) / 
                                            sum(tp_grupo_domestico * wper),
    .groups = "drop"
  )

# 5.3 Childcare
# ------------------------------------------------------------------------------
average_childcare <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_childcare = sum(tss_total_0a13 * wper) / 
                                    sum(wper),
    average_per_participant_childcare = sum(tss_total_0a13 * wper) / 
                                        sum(tp_act_cuidado_0A13 * wper),
    .groups = "drop"
  )

# 5.4 Household Care
# ------------------------------------------------------------------------------
average_carhouse <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_carhouse = sum(tss_grupo_cuidado * wper) / 
                                   sum(wper),
    average_per_participant_carhouse = sum(tss_grupo_cuidado * wper) / 
                                       sum(tp_grupo_cuidado * wper),
    .groups = "drop"
  )

# 5.5 Personal Activities
# ------------------------------------------------------------------------------
average_peract <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_peract = sum(tss_grangrupo_personales * wper) / 
                                 sum(wper),
    average_per_participant_peract = sum(tss_grangrupo_personales * wper) / 
                                     sum(tp_grangrupo_personales * wper),
    .groups = "drop"
  )

# 5.6 Unpaid Work
# ------------------------------------------------------------------------------
average_unpaid <- base_data %>%
  group_by(grupo_edad_sel, sexo_sel, bhch07_sel) %>%
  summarize(
    average_social_time_unpaid = sum(tss_grangrupo_tnr * wper) / 
                                 sum(wper),
    average_per_participant_unpaid = sum(tss_grangrupo_tnr * wper) / 
                                     sum(tp_grangrupo_tnr * wper),
    .groups = "drop"
  )

# 5.7 Combine all grouped data
# ------------------------------------------------------------------------------
combined_average_time_table <- average_paid_work %>%
  left_join(average_domestic_work, 
            by = c("grupo_edad_sel", "sexo_sel", "bhch07_sel")) %>%
  left_join(average_childcare, 
            by = c("grupo_edad_sel", "sexo_sel", "bhch07_sel")) %>%
  left_join(average_carhouse, 
            by = c("grupo_edad_sel", "sexo_sel", "bhch07_sel")) %>%
  left_join(average_peract, 
            by = c("grupo_edad_sel", "sexo_sel", "bhch07_sel")) %>%
  left_join(average_unpaid, 
            by = c("grupo_edad_sel", "sexo_sel", "bhch07_sel"))

# ==============================================================================
# 6. HELPER FUNCTIONS FOR VISUALIZATION
# ==============================================================================

# 6.1 Create age group labels
# ------------------------------------------------------------------------------
get_age_labels <- function() {
  c(
    "2" = "14-29 years",
    "3" = "30-64 years",
    "4" = "65+ years"
  )
}

# 6.2 Create marital status labels
# ------------------------------------------------------------------------------
get_marital_labels <- function() {
  c(
    "1" = "In relationship",
    "2" = "Married",
    "3" = "Separated",
    "4" = "Divorced",
    "5" = "Widowed",
    "6" = "Single",
    "99" = "No answer"
  )
}

# 6.3 Common theme for all plots
# ------------------------------------------------------------------------------
get_plot_theme <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

# 6.4 Sex color scheme
# ------------------------------------------------------------------------------
get_sex_colors <- function() {
  c("1" = "#E41A1C", "2" = "#377EB8")  # Red for Female, Blue for Male
}

get_sex_labels <- function() {
  c("1" = "Female", "2" = "Male")
}

# ==============================================================================
# 7. VISUALIZATIONS: TIME BY AGE AND SEX
# ==============================================================================

# 7.1 Paid Work - Social Average
# ------------------------------------------------------------------------------
plot1 <- ggplot(
  average_paid_work, 
  aes(x = factor(grupo_edad_sel), 
      y = average_social_time_paid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Paid Work by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.2 Paid Work - Participant Average
# ------------------------------------------------------------------------------
plot2 <- ggplot(
  average_paid_work, 
  aes(x = factor(grupo_edad_sel), 
      y = average_per_participant_paid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Paid Work by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.3 Childcare - Social Average
# ------------------------------------------------------------------------------
plot3 <- ggplot(
  average_childcare, 
  aes(x = factor(grupo_edad_sel), 
      y = average_social_time_childcare,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Childcare by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.4 Childcare - Participant Average
# ------------------------------------------------------------------------------
plot4 <- ggplot(
  average_childcare, 
  aes(x = factor(grupo_edad_sel), 
      y = average_per_participant_childcare,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Childcare by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.5 Household Care - Social Average
# ------------------------------------------------------------------------------
plot5 <- ggplot(
  average_carhouse, 
  aes(x = factor(grupo_edad_sel), 
      y = average_social_time_carhouse,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Household Care by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.6 Household Care - Participant Average
# ------------------------------------------------------------------------------
plot6 <- ggplot(
  average_carhouse, 
  aes(x = factor(grupo_edad_sel), 
      y = average_per_participant_carhouse,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Household Care by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.7 Personal Activities - Social Average
# ------------------------------------------------------------------------------
plot7 <- ggplot(
  average_peract, 
  aes(x = factor(grupo_edad_sel), 
      y = average_social_time_peract,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Personal Activities by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.8 Personal Activities - Participant Average
# ------------------------------------------------------------------------------
plot8 <- ggplot(
  average_peract, 
  aes(x = factor(grupo_edad_sel), 
      y = average_per_participant_peract,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Personal Activities by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.9 Unpaid Work - Social Average
# ------------------------------------------------------------------------------
plot9 <- ggplot(
  average_unpaid, 
  aes(x = factor(grupo_edad_sel), 
      y = average_social_time_unpaid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Unpaid Work by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# 7.10 Unpaid Work - Participant Average
# ------------------------------------------------------------------------------
plot10 <- ggplot(
  average_unpaid, 
  aes(x = factor(grupo_edad_sel), 
      y = average_per_participant_unpaid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Unpaid Work by Age and Sex",
    x = "Age Group",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_age_labels()) +
  get_plot_theme()

# ==============================================================================
# 8. VISUALIZATIONS: TIME BY MARITAL STATUS AND SEX
# ==============================================================================

# 8.1 Paid Work - Social Average
# ------------------------------------------------------------------------------
plot11 <- ggplot(
  average_paid_work, 
  aes(x = factor(bhch07_sel), 
      y = average_social_time_paid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Paid Work by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.2 Paid Work - Participant Average
# ------------------------------------------------------------------------------
plot12 <- ggplot(
  average_paid_work, 
  aes(x = factor(bhch07_sel), 
      y = average_per_participant_paid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Paid Work by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.3 Childcare - Social Average
# ------------------------------------------------------------------------------
plot13 <- ggplot(
  average_childcare, 
  aes(x = factor(bhch07_sel), 
      y = average_social_time_childcare,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Childcare by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.4 Childcare - Participant Average
# ------------------------------------------------------------------------------
plot14 <- ggplot(
  average_childcare, 
  aes(x = factor(bhch07_sel), 
      y = average_per_participant_childcare,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Childcare by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.5 Household Care - Social Average
# ------------------------------------------------------------------------------
plot15 <- ggplot(
  average_carhouse, 
  aes(x = factor(bhch07_sel), 
      y = average_social_time_carhouse,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Household Care by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.6 Household Care - Participant Average
# ------------------------------------------------------------------------------
plot16 <- ggplot(
  average_carhouse, 
  aes(x = factor(bhch07_sel), 
      y = average_per_participant_carhouse,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Household Care by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.7 Personal Activities - Social Average
# ------------------------------------------------------------------------------
plot17 <- ggplot(
  average_peract, 
  aes(x = factor(bhch07_sel), 
      y = average_social_time_peract,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Personal Activities by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.8 Personal Activities - Participant Average
# ------------------------------------------------------------------------------
plot18 <- ggplot(
  average_peract, 
  aes(x = factor(bhch07_sel), 
      y = average_per_participant_peract,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Personal Activities by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.9 Unpaid Work - Social Average
# ------------------------------------------------------------------------------
plot19 <- ggplot(
  average_unpaid, 
  aes(x = factor(bhch07_sel), 
      y = average_social_time_unpaid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Social Time in Unpaid Work by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8.10 Unpaid Work - Participant Average
# ------------------------------------------------------------------------------
plot20 <- ggplot(
  average_unpaid, 
  aes(x = factor(bhch07_sel), 
      y = average_per_participant_unpaid,
      fill = factor(sexo_sel))
) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Time Per Participant in Unpaid Work by Marital Status and Sex",
    x = "Marital Status",
    y = "Average Time (minutes)",
    fill = "Sex"
  ) +
  scale_fill_manual(values = get_sex_colors(), labels = get_sex_labels()) +
  scale_x_discrete(labels = get_marital_labels()) +
  get_plot_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ==============================================================================
# 9. STACKED BAR CHART: TIME ALLOCATION BY SEX
# ==============================================================================

# Prepare data for stacked visualization
average_social_time_long <- combined_average_time_table %>%
  pivot_longer(
    cols = c(
      average_social_time_paid, 
      average_social_time_peract, 
      average_social_time_unpaid
    ),
    names_to = "activity",
    values_to = "time"
  )

# Create stacked bar chart
plot21 <- ggplot(
  average_social_time_long, 
  aes(x = factor(sexo_sel), y = time, fill = activity)
) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Total Time Allocation by Sex",
    x = "Sex",
    y = "Total Time (minutes)",
    fill = "Activity Type"
  ) +
  scale_fill_manual(
    values = c(
      "average_social_time_paid" = "#1f77b4",
      "average_social_time_unpaid" = "#ff7f0e",
      "average_social_time_peract" = "#2ca02c"
    ),
    labels = c(
      "average_social_time_paid" = "Paid Work",
      "average_social_time_unpaid" = "Unpaid Work",
      "average_social_time_peract" = "Personal Activities"
    )
  ) +
  scale_x_discrete(labels = get_sex_labels()) +
  get_plot_theme()

# ==============================================================================
# 10. EXPORT VISUALIZATIONS
# ==============================================================================

# Create list of all plots
plot_list <- list(
  plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
  plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18, plot19, plot20,
  plot21
)

# Save all plots
cat("Saving visualizations...\n")
for (i in seq_along(plot_list)) {
  filename <- sprintf("plot_%02d.png", i)
  ggsave(
    filename, 
    plot = plot_list[[i]], 
    width = 10, 
    height = 6, 
    dpi = 300
  )
  cat(sprintf("  Saved: %s\n", filename))
}

cat("All visualizations saved successfully.\n")

# ==============================================================================
# 11. CLEANUP AND CLOSE LOG
# ==============================================================================

log_close()

cat("\n=== Analysis Complete ===\n")
cat("Log file: time_use_analysis.log\n")
cat("Output plots: plot_01.png to plot_21.png\n")
cat("Combined data table: combined_average_time_table\n")