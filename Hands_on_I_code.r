# Assignment 1
setwd("/home/diegovicente/dmi/assignment-1-diegovicente01-cmd")

# Import the necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(knitr)
library(kableExtra)
library(patchwork)
library(pheatmap)
library(scales)
# -------------------------------------------------------------------------
# EXERCISE 1.1
# -------------------------------------------------------------------------


# --- PREPARE DATA ---
# Load data from excel and treat "/" and empty strings as missing values
df <- read_excel("table_s1.xlsx", sheet = 2, na = c("", "/"))

# Remove the spaces from the column names
names(df) <- gsub(" ", "_", names(df))
names(df) <- gsub("\\(", "", names(df))
names(df) <- gsub("\\)", "", names(df))

# Convert date format and calculate required time differences
df <- df %>%
  mutate(
    # Convert columns to Date format 
    Onset_date_f = as.Date(Onset_date_f),
    Admission_date = as.Date(Admission_date),
    Date_of_progression_to_severe_state = as.Date(Date_of_progression_to_severe_state),
    
    # Calculate time differences in days
    time_onset_to_admission = as.numeric(Admission_date - Onset_date_f + 1),
    time_admission_to_severe = as.numeric(Date_of_progression_to_severe_state - Admission_date + 1)
  )

# Create group labels
df <- df %>%
  mutate(
    Group = case_when(
      Group_d == 0 ~ "healthy",
      Group_d == 1 ~ "non_covid",
      Group_d == 2 ~ "non_severe",
      Group_d == 3 ~ "severe"
    )
  )

# Create sex labels
df <- df %>%
  mutate(Sex_g = factor(Sex_g, 
                        levels = c(0, 1),            
                        labels = c("Female", "Male"))) 

# Create an expanded dataset, in order to include the groups "Total Covid" 
# and "Global" when calculating and plotting statistics later. 
# Create "Total Covid" subset 
df_covid_total <- df %>%
  filter(Group %in% c("non_severe", "severe")) %>%
  mutate(Group = "total_covid")
# Create "Global" subset (All of the rows)
df_global <- df %>%
  mutate(Group = "Global")
# Bind everything together into expanded dataset and set groups differentiation.
df_expanded <- bind_rows(df, df_covid_total, df_global)
df_expanded$Group <- factor(df_expanded$Group, 
                            levels = c("healthy", "non_covid", "non_severe", "severe", "total_covid", "Global"))


# --- CALCULATE STATISTICS ---
# AGE & BMI — Statistics by group 
age_bmi_stats <- df_expanded %>%
  group_by(Group) %>%
  summarise(
    # Age
    mean_age = mean(Age_year, na.rm = TRUE),
    sd_age   = sd(Age_year, na.rm = TRUE),
    median_age = median(Age_year, na.rm = TRUE),
    Q1_age     = quantile(Age_year, 0.25, na.rm = TRUE), 
    Q3_age     = quantile(Age_year, 0.75, na.rm = TRUE), 
    iqr_age    = IQR(Age_year, na.rm = TRUE),
    min_age    = min(Age_year, na.rm = TRUE),
    max_age    = max(Age_year, na.rm = TRUE),
    # BMI
    mean_bmi = mean(BMI_h, na.rm = TRUE),
    sd_bmi   = sd(BMI_h, na.rm = TRUE),
    median_bmi = median(BMI_h, na.rm = TRUE),
    Q1_bmi     = quantile(BMI_h, 0.25, na.rm = TRUE),    
    Q3_bmi     = quantile(BMI_h, 0.75, na.rm = TRUE),    
    iqr_bmi    = IQR(BMI_h, na.rm = TRUE),
    min_bmi    = min(BMI_h, na.rm = TRUE),
    max_bmi    = max(BMI_h, na.rm = TRUE),
    .groups = "drop"
  )

# Time - Statistics by group
time_stats <- df_expanded %>%
  group_by(Group) %>%
  summarise(
    # Time: Onset to Admission
    mean_onset_adm   = mean(time_onset_to_admission, na.rm = TRUE),
    sd_onset_adm     = sd(time_onset_to_admission, na.rm = TRUE),
    median_onset_adm = median(time_onset_to_admission, na.rm = TRUE),
    Q1_onset_adm     = quantile(time_onset_to_admission, 0.25, na.rm = TRUE),
    Q3_onset_adm     = quantile(time_onset_to_admission, 0.75, na.rm = TRUE),
    iqr_onset_adm    = IQR(time_onset_to_admission, na.rm = TRUE),
    min_onset_adm    = if(all(is.na(time_onset_to_admission))) NA else min(time_onset_to_admission, na.rm = TRUE),
    max_onset_adm    = if(all(is.na(time_onset_to_admission))) NA else max(time_onset_to_admission, na.rm = TRUE),
    # Time: Admission to Severe
    mean_adm_sev     = mean(time_admission_to_severe, na.rm = TRUE),
    sd_adm_sev       = sd(time_admission_to_severe, na.rm = TRUE),
    median_adm_sev   = median(time_admission_to_severe, na.rm = TRUE),
    Q1_adm_sev       = quantile(time_admission_to_severe, 0.25, na.rm = TRUE),
    Q3_adm_sev       = quantile(time_admission_to_severe, 0.75, na.rm = TRUE),
    iqr_adm_sev      = IQR(time_admission_to_severe, na.rm = TRUE),
    min_adm_sev      = if(all(is.na(time_admission_to_severe))) NA else min(time_admission_to_severe, na.rm = TRUE),
    max_adm_sev      = if(all(is.na(time_admission_to_severe))) NA else max(time_admission_to_severe, na.rm = TRUE),
    .groups = "drop"
  )

# Sex - Statistics by group 
sex_stats <- df_expanded %>%
  group_by(Group, Sex_g) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()


# --- OUTPUT TABLE ---
# Count the patients in each group and create group lables for the table
group_counts <- table(df_expanded$Group)
lbl_healthy    <- paste0("Healthy (N=", group_counts["healthy"], ")")
lbl_non_covid  <- paste0("Non-COVID-19 (N=", group_counts["non_covid"], ")")
lbl_total      <- paste0("Total COVID-19 (N=", group_counts["total_covid"], ")")
lbl_non_severe <- paste0("COVID-19 Non-severe (N=", group_counts["non_severe"], ")")
lbl_severe     <- paste0("COVID-19 Severe (N=", group_counts["severe"], ")")

# Define the Column Order 
col_selection <- c(
  "Variable", 
  "Statistic", 
  "healthy", 
  "non_covid", 
  "total_covid", 
  "non_severe", 
  "severe"
)

# Sex
sex_rows <- sex_stats %>%
  filter(Group %in% c("healthy", "non_covid", "total_covid", "non_severe", "severe")) %>%
  mutate(val = sprintf("%d (%.1f%%)", count, proportion * 100)) %>%
  select(Group, Sex_g, val) %>%
  pivot_wider(names_from = Group, values_from = val) %>%
  mutate(
    # Force labels to be Male/Female text
    Statistic = case_when(
      as.character(Sex_g) %in% c("1", "Male") ~ "Male",
      as.character(Sex_g) %in% c("0", "Female") ~ "Female",
    ),
    Variable = "Sex - no. (%)" 
  ) %>%
  arrange(factor(Statistic, levels = c("Male", "Female"))) 

# Age 
age_rows <- age_bmi_stats %>%
  filter(Group %in% c("healthy", "non_covid", "total_covid", "non_severe", "severe")) %>%
  mutate(
    `Mean ± SD`    = sprintf("%.1f ± %.1f", mean_age, sd_age),
    `Median (IQR)` = sprintf("%.1f (%.1f-%.1f)", median_age, Q1_age, Q3_age),
    `Range`        = sprintf("%.1f-%.1f", min_age, max_age)
  ) %>%
  select(Group, `Mean ± SD`, `Median (IQR)`, `Range`) %>%
  pivot_longer(cols = -Group, names_to = "Statistic", values_to = "val") %>%
  pivot_wider(names_from = Group, values_from = val) %>%
  mutate(Variable = "Age - year")

# BMI 
bmi_rows <- age_bmi_stats %>%
  filter(Group %in% c("healthy", "non_covid", "total_covid", "non_severe", "severe")) %>%
  mutate(
    `Mean ± SD`    = sprintf("%.1f ± %.1f", mean_bmi, sd_bmi),
    `Median (IQR)` = sprintf("%.1f (%.1f-%.1f)", median_bmi, Q1_bmi, Q3_bmi),
    `Range`        = sprintf("%.1f-%.1f", min_bmi, max_bmi)
  ) %>%
  select(Group, `Mean ± SD`, `Median (IQR)`, `Range`) %>%
  pivot_longer(cols = -Group, names_to = "Statistic", values_to = "val") %>%
  pivot_wider(names_from = Group, values_from = val) %>%
  mutate(Variable = "BMI - kg/m2")

# Time 1: Onset to Admission 
time1_rows <- time_stats %>%
  filter(Group %in% c("healthy", "non_covid", "total_covid", "non_severe", "severe")) %>%
  mutate(
    `Mean ± SD`    = ifelse(is.na(mean_onset_adm), "", sprintf("%.1f ± %.1f", mean_onset_adm, sd_onset_adm)),
    `Median (IQR)` = ifelse(is.na(median_onset_adm), "", sprintf("%.1f (%.1f-%.1f)", median_onset_adm, Q1_onset_adm, Q3_onset_adm)),
    `Range`        = ifelse(is.na(min_onset_adm), "", sprintf("%.1f-%.1f", min_onset_adm, max_onset_adm))
  ) %>%
  select(Group, `Mean ± SD`, `Median (IQR)`, `Range`) %>%
  pivot_longer(cols = -Group, names_to = "Statistic", values_to = "val") %>%
  pivot_wider(names_from = Group, values_from = val) %>%
  mutate(Variable = "Time from Onset to Admission - Days")

# Time 2: Admission to Severe 
time2_rows <- time_stats %>%
  filter(Group %in% c("healthy", "non_covid", "total_covid", "non_severe", "severe")) %>%
  mutate(
    `Mean ± SD`    = ifelse(is.na(mean_adm_sev), "", sprintf("%.1f ± %.1f", mean_adm_sev, sd_adm_sev)),
    `Median (IQR)` = ifelse(is.na(median_adm_sev), "", sprintf("%.1f (%.1f-%.1f)", median_adm_sev, Q1_adm_sev, Q3_adm_sev)),
    `Range`        = ifelse(is.na(min_adm_sev), "", sprintf("%.1f-%.1f", min_adm_sev, max_adm_sev))
  ) %>%
  select(Group, `Mean ± SD`, `Median (IQR)`, `Range`) %>%
  pivot_longer(cols = -Group, names_to = "Statistic", values_to = "val") %>%
  pivot_wider(names_from = Group, values_from = val) %>%
  mutate(Variable = "Time from Admission to Severe - Days")

# Bind together and print 
final_table <- bind_rows(sex_rows, age_rows, bmi_rows, time1_rows, time2_rows) %>%
  select(all_of(col_selection)) %>%
  rename(
    !!lbl_healthy := healthy,
    !!lbl_non_covid := non_covid,
    !!lbl_total := total_covid,
    !!lbl_non_severe := non_severe,
    !!lbl_severe := severe
  )

kbl(final_table, caption = "Table 1: Demographical and Baseline Characteristics") %>%
  kable_classic(full_width = F, html_font = "Arial") %>%
  row_spec(0, bold = TRUE) %>% 
  column_spec(1, bold = TRUE) %>%
  # Add the separation lines
  row_spec(2, extra_css = "border-bottom: 1px solid #ddd;") %>%
  row_spec(5, extra_css = "border-bottom: 1px solid #ddd;") %>%
  row_spec(8, extra_css = "border-bottom: 1px solid #ddd;") %>%
  row_spec(11, extra_css = "border-bottom: 1px solid #ddd;") %>%
  collapse_rows(columns = 1, valign = "top") 

# -------------------------------------------------------------------------
# EXERCISE 1.2
# -------------------------------------------------------------------------

# Convert all time-based columns to Date
df <- df %>%
  mutate(across(where(~ inherits(.x, "POSIXt")), as.Date))

# Provide a summary of the data with "skimr" package
library(skimr)
skim(df)

# Remove column with almost only missing data
df <- df %>% select(-`MSRep_ID_c`)

# --- PLOTS ---
# Set the order of the groups for the plots
df_expanded$Group <- factor(df_expanded$Group, 
                            levels = c("Global", "healthy", "non_covid", "total_covid", "non_severe", "severe"))

# Plot 1: Sex by groups
ggplot(df_expanded, aes(x = Group, fill = Sex_g)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Proportion of Sex by Group", y = "Proportion", x = "", fill = "Sex") +
  scale_fill_manual(values = c("Female" = "#E69F00", "Male" = "#56B4E9")) +
  scale_x_discrete(labels = c(
    "Global"      = "Global",
    "healthy"     = "Healthy",
    "non_covid"   = "Non-COVID-19",
    "total_covid" = "Total COVID-19",
    "non_severe"  = "COVID-19 Non-severe",
    "severe"      = "COVID-19 Severe"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: BMI by Groups 
ggplot(df_expanded, aes(x = Group, y = BMI_h, fill = Group)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Distribution of BMI by Patient Group",
    x = "",
    y = "BMI"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c(
    "Global"      = "Global",
    "healthy"     = "Healthy",
    "non_covid"   = "Non-COVID-19",
    "total_covid" = "Total COVID-19",
    "non_severe"  = "COVID-19 Non-severe",
    "severe"      = "COVID-19 Severe"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Age by Groups 
ggplot(df_expanded, aes(x = Group, y = Age_year, fill = Group)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Distribution of Age by Patient Group",
    x = "",
    y = "Age"
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = c(
    "Global"      = "Global",
    "healthy"     = "Healthy",
    "non_covid"   = "Non-COVID-19",
    "total_covid" = "Total COVID-19",
    "non_severe"  = "COVID-19 Non-severe",
    "severe"      = "COVID-19 Severe"
  )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 4: BMI by Sex 
ggplot(df, aes(x = Sex_g, y = BMI_h, fill = Sex_g)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of BMI by Sex",
    x = "Sex",
    y = "BMI",
    fill = ""
  ) +
  scale_fill_manual(values = c("Female" = "#E69F00", "Male" = "#56B4E9"))

# Plot 5: Age by Sex 
ggplot(df, aes(x = Sex_g, y = Age_year, fill = Sex_g)) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Distribution of Age by Sex",
    x = "Sex",
    y = "Age",
    fill = ""
  ) +
  scale_fill_manual(values = c("Female" = "#E69F00", "Male" = "#56B4E9"))

# Plot 6: Correlation Plot between Metabolomics columns
# Metabolomic columns indices 
target_cols_corr <- c(18, 19, 20, 21, 23, 24, 25, 26, 27, 28, 29, 30)
# Ensure columns are numeric; handle errors if columns contain non-numeric data, clean the variable names
df_corr <- df[, target_cols_corr]
current_names <- names(df_corr)
clean_names <- gsub(",.*", "", current_names)
clean_names <- gsub("_[a-zA-Z]$", "", clean_names)
clean_names <- gsub("_", " ", clean_names)
names(df_corr) <- clean_names
df_corr[] <- lapply(df_corr, function(x) as.numeric(as.character(x)))
# Calculate the correlation matrix
res <- cor(df_corr, use = "complete.obs")
# Generate the plot
corrplot(res, 
         type = "upper",           
         order = "hclust",        
         tl.col = "black",        
         tl.srt = 45,             
         tl.cex = 0.7,            
         diag = FALSE,
         title = "Correlation of Clinical and Metabolomic Variables",
         mar = c(0, 0, 2, 0)
)
# -------------------------------------------------------------------------
# EXERCISE 2.1
# -------------------------------------------------------------------------

# Define indexes and names
target_cols_plots <- c(19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31)
target_colnames <- colnames(df_expanded)[target_cols_plots]

# Handle labels for plots
clean_titles <- c(
  "White blood cell (WBC)",
  "Lymphocyte",
  "Monocyte",
  "Platelet",
  "C-reactive protein (CRP)",
  "Alanine aminotransferase (ALT)",
  "Aspartate aminotransferase (AST)",
  "Glutamyltransferase (GGT)",
  "Total bilirubin (TBIL)",
  "Direct bilirubin (DBIL)",
  "Creatinine",
  "Glucose"
)

clean_units <- list(
  expression(Count ~ (10^9/L)),         # WBC (Implicit multiplication) or use (times ~ 10^9/L)
  expression(Count ~ (10^9/L)),         # Lymphocyte
  expression(Count ~ (10^9/L)),         # Monocyte
  expression(Count ~ (10^9/L)),         # Platelet
  "mg/L",                               # CRP
  "U/L",                                # ALT
  "U/L",                                # AST
  "U/L",                                # GGT
  expression(mu * mol/L),               # TBIL (µmol/L)
  expression(mu * mol/L),               # DBIL
  expression(mu * mol/L),               # Creatinine
  "mmol/L"                              # Glucose
)

# Filter data frame and order groups
df_filtered <- df_expanded %>%
  filter(Group %in% c("non_covid", "non_severe", "severe")) %>%
  mutate(
    Group = factor(Group, levels = c("non_covid", "non_severe", "severe")),
    `CRP_i,_mg/L` = as.numeric(gsub("[^0-9.]", "", as.character(`CRP_i,_mg/L`)))
  )

# Create plots list
plot_list <- list()

for (i in seq_along(target_cols_plots)) {
  
  col_idx <- target_cols_plots[i]
  colname <- colnames(df_expanded)[col_idx]
  
  my_title <- clean_titles[i]
  my_unit  <- clean_units[[i]]
  
  p <- ggplot(df_filtered, aes(x = Group, y = .data[[colname]], fill = Group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
    theme_minimal() +
    labs(
      title = my_title,
      x = NULL,
      y = my_unit
    ) +
    scale_fill_manual(values = c("non_covid"  = "#66c2a5", 
                                 "non_severe" = "#fc8d62", 
                                 "severe"     = "#8da0cb")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  plot_list[[colname]] <- p
}

# Organize and show the 12 plots
wrap_plots(plot_list, ncol = 3, nrow = 4) + 
  plot_annotation(
    title = "Comparison of Clinical and Metabolomic Variables by Severity",
    subtitle = "Groups: Non-COVID, Non-Severe and Severe"
  )



# -------------------------------------------------------------------------
# EXERCISE 2.2
# -------------------------------------------------------------------------
# Method 1: hide extreme values
df_filtered <- df_expanded %>%
  filter(Group %in% c("non_covid", "non_severe", "severe")) %>%
  mutate(Group = factor(Group, levels = c("non_covid", "non_severe", "severe"))) %>%
  # Force specific columns to be numeric
  mutate(across(all_of(colnames(df_expanded)[target_cols_plots]), ~ as.numeric(as.character(.x))))

# Generate plots list
plot_list <- list()

for (i in seq_along(target_cols_plots)) {
  
  col_idx <- target_cols_plots[i]
  colname <- colnames(df_expanded)[col_idx]
  
  my_title <- clean_titles[i]
  my_unit  <- clean_units[[i]]
  
  # Calculate the 95th percentile for the Upper Limit
  # We use this to "zoom in" and hide the top 5% extreme values
  upper_limit <- quantile(df_filtered[[colname]], 0.95, na.rm = TRUE)
  
  p <- ggplot(df_filtered, aes(x = Group, y = .data[[colname]], fill = Group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
    
    coord_cartesian(ylim = c(0, upper_limit)) +
    
    theme_minimal() +
    labs(
      title = my_title,
      x = NULL,
      y = my_unit
    ) +
    scale_fill_manual(values = c("non_covid"  = "#66c2a5", 
                                 "non_severe" = "#fc8d62", 
                                 "severe"     = "#8da0cb")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  plot_list[[colname]] <- p
}

# Organize and show the 12 plots
wrap_plots(plot_list, ncol = 3, nrow = 4) + 
  plot_annotation(
    title = "Comparison of Metabolites (Extreme Values Clipped)",
    subtitle = "View zoomed to 95th percentile; top 5% outliers hidden"
  )


# Method 2: Hide outliers
# Create plots list
plot_list <- list()

for (i in seq_along(target_cols_plots)) {
  
  col_idx <- target_cols_plots[i]
  colname <- colnames(df_expanded)[col_idx]
  
  my_title <- clean_titles[i]
  my_unit  <- clean_units[[i]]
  
  # Calculate Whiskers  per group
  outlier_bounds <- df_filtered %>%
    group_by(Group) %>%
    summarise(
      Q1 = quantile(.data[[colname]], 0.25, na.rm = TRUE),
      Q3 = quantile(.data[[colname]], 0.75, na.rm = TRUE),
      IQR_val = Q3 - Q1,
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      .groups = "drop"
    )
  
  # Filter Points for Jitter
  # Join the bounds to the data and keep only points within the bounds
  df_points_filtered <- df_filtered %>%
    left_join(outlier_bounds, by = "Group") %>%
    filter(.data[[colname]] >= lower_bound & .data[[colname]] <= upper_bound)
  
  plot_min <- min(df_points_filtered[[colname]], na.rm = TRUE)
  plot_max <- max(df_points_filtered[[colname]], na.rm = TRUE)
  plot_min <- max(0, plot_min * 0.95) 
  plot_max <- plot_max * 1.05
  
  
  p <- ggplot(df_filtered, aes(x = Group, y = .data[[colname]], fill = Group)) +
    
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    
    geom_jitter(data = df_points_filtered, 
                width = 0.2, alpha = 0.4, size = 1) +
    
    coord_cartesian(ylim = c(plot_min, plot_max)) +
    
    theme_minimal() +
    labs(
      title = my_title,
      x = NULL,
      y = my_unit
    ) +
    scale_fill_manual(values = c("non_covid"  = "#66c2a5", 
                                 "non_severe" = "#fc8d62", 
                                 "severe"     = "#8da0cb")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  plot_list[[colname]] <- p
}

wrap_plots(plot_list, ncol = 3, nrow = 4) + 
  plot_annotation(
    title = "Comparison of Metabolites (Outliers Removed)",
    subtitle = "View restricted to statistical whiskers (Group-specific IQR)"
  )


# Method 3: log-transformation
# Create plots list
plot_list <- list()

for (i in seq_along(target_cols_plots)) {
  
  col_idx <- target_cols_plots[i]
  colname <- colnames(df_expanded)[col_idx]
  
  my_title <- clean_titles[i]
  my_unit  <- clean_units[[i]]
  
  p <- ggplot(df_filtered, aes(x = Group, y = .data[[colname]], fill = Group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
    
    # Use pseudo_log to handle any potential 0s safely without error
    scale_y_continuous(trans = "pseudo_log") + 
    
    theme_minimal() +
    labs(
      title = my_title,
      x = NULL,
      y = my_unit
    ) +
    scale_fill_manual(values = c("non_covid"  = "#66c2a5", 
                                 "non_severe" = "#fc8d62", 
                                 "severe"     = "#8da0cb")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold")
    )
  
  plot_list[[colname]] <- p
}

wrap_plots(plot_list, ncol = 3, nrow = 4) + 
  plot_annotation(
    title = "Comparison of Metabolites (Log-Transformed)",
    subtitle = "Outliers handled via Pseudo-Log Transformation"
  )


# Method 4: log-transformation + hiding extreme values
plot_list <- list()

for (i in seq_along(target_cols_plots)) {
  
  col_idx <- target_cols_plots[i]
  colname <- colnames(df_expanded)[col_idx]
  
  my_title <- clean_titles[i]
  my_unit  <- clean_units[[i]]
  
  lower_limit <- quantile(df_filtered[[colname]], 0.02, na.rm = TRUE)
  upper_limit <- quantile(df_filtered[[colname]], 0.98, na.rm = TRUE)
  
  if (lower_limit < 0) lower_limit <- 0
  
  p <- ggplot(df_filtered, aes(x = Group, y = .data[[colname]], fill = Group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) + 
    geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
    
    scale_y_continuous(trans = "pseudo_log", n.breaks = 4) + 
    
    coord_cartesian(ylim = c(lower_limit * 0.9, upper_limit * 1.1)) +
    
    theme_minimal() +
    labs(
      title = my_title,
      x = NULL,
      y = my_unit
    ) +
    scale_fill_manual(values = c("non_covid"  = "#66c2a5", 
                                 "non_severe" = "#fc8d62", 
                                 "severe"     = "#8da0cb")) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none",
      plot.title = element_text(size = 10, face = "bold"),
      
      axis.text.y = element_text(size = 7) 
    )
  
  plot_list[[colname]] <- p
}

wrap_plots(plot_list, ncol = 3, nrow = 4) + 
  plot_annotation(
    title = "Comparison of Metabolites (Log-Transformed + Clipped)",
    subtitle = "Zoomed to 2nd-98th percentiles"
  )


# -------------------------------------------------------------------------
# EXERCISE 3
# -------------------------------------------------------------------------
# Prepare Data Matrix
# Select only the 12 target numeric columns
heatmap_data <- df_filtered %>%
  select(all_of(target_colnames)) %>%
  as.matrix() 

# Rename Columns (Variables) to Clean Titles
# We use the 'clean_titles' vector we defined in Exercise 2.2
colnames(heatmap_data) <- clean_titles

# Scale and Transpose
heatmap_matrix <- t(scale(heatmap_data))

# Create a data frame for the column annotations
annotation_info <- df_filtered %>%
  select(Group, Sex_g) %>%
  as.data.frame()

# Rename the columns for the Legend
colnames(annotation_info) <- c("Group", "Sex")

# Rename the Factor Levels for "Group" to be descriptive
annotation_info$Group <- factor(annotation_info$Group, 
                                levels = c("non_covid", "non_severe", "severe"),
                                labels = c("Non-COVID-19", "COVID-19 Non-severe", "COVID-19 Severe"))

# Ensure row names of annotation match column names of the matrix
rownames(annotation_info) <- paste0("P", 1:ncol(heatmap_matrix))
colnames(heatmap_matrix)  <- paste0("P", 1:ncol(heatmap_matrix))

ann_colors <- list(
  Group = c("Non-COVID-19"        = "#66c2a5", 
            "COVID-19 Non-severe" = "#fc8d62", 
            "COVID-19 Severe"     = "#8da0cb"),
  Sex   = c("Female"              = "#E69F00", 
            "Male"                = "#56B4E9")
)

# Draw the Heatmap 
library(pheatmap)

pheatmap(heatmap_matrix,
         cluster_rows = TRUE,         # Cluster metabolites
         cluster_cols = TRUE,         # Cluster patients
         annotation_col = annotation_info, 
         annotation_colors = ann_colors,
         show_colnames = FALSE,       # Hide messy patient IDs
         show_rownames = TRUE,        # Show the clean metabolite names
         fontsize_row = 10,           # Adjust font size if needed
         color = colorRampPalette(c("navy", "white", "firebrick3"))(100),
         main = "Biomarker Heatmap: Metabolites by gender and severity group"
)


