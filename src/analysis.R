# =============================================================================
# Lab 05: Comparing Groups with t-tests, ANOVA and MANOVA
# =============================================================================
#
# In this tutorial, we will spend time to analyze (synthetic) student study-habit 
# data to gain some experience in comparing group averages. Every statistical 
# test in this lab is paired with a plot. As a general rule, please always spend
# time to explore the data and create plots to get familiar with it. Once you 
# are familiar with your data, you will know what kinds of comparisons 
# (statistical tests) ought to be used to address research questions.

# In this lab, we will cover the following statistical tests: 
# - Welch two-sample t-test      t.test(y ~ group, data)
# - Paired t-test                t.test(after, before, paired = TRUE)
# - One-way ANOVA + Tukey HSD    aov(y ~ group, data), TukeyHSD()
# - One-way MANOVA               manova(cbind(y1, y2, y3) ~ group, data)
#
# Note: you may have to look up function documentation to complete some tasks.
# Try ?t.test, ?aov, ?TukeyHSD and ?manova in the R console.
#
# Dataset: data/student_wellness.csv
# =============================================================================


# NAME: Add Your Name Here
# DATE: Add Date Here


rm(list = ls()) # clear out the variables from memory to make a clean execution of the code.

# If you want to remove all previous plots and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console

# Load required libraries, install them as necessary.
if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

# Every plot is displayed AND saved to graphics/output/ so you can include them in your submission if you want.
# (You do not need to edit this helper.)
dir.create(file.path("graphics", "output"), showWarnings = FALSE, recursive = TRUE)
show_plot <- function(p, name) {
  print(p)
  ggsave(file.path("graphics", "output", paste0(name, ".png")), p, width = 7, height = 5, dpi = 120)
}

alpha <- 0.05 # significance level used for every test in this lab

#################################################

# Load the data: data/student_wellness.csv
# Make sure your working directory is the lab folder (Session > Set Working Directory in RStudio).
# Equally, you can use the file.choose() function to browse for the CSV file.

# TODO 1: Load data/student_wellness.csv into a dataframe called 'students'
# Hint: Use read_csv(PATH/FILE), or read_csv(file.choose()) to browse for it.

students <- NULL # YOUR CODE HERE

# Make the grouping columns factors so that plots and tests treat them as groups.
students <- students %>%
  mutate(
    study_method = factor(study_method, levels = c("Passive Review", "Flashcards", "Practice Tests")),
    device = factor(device),
    campus_zone = factor(campus_zone)
  )

# Display basic information about the dataset
str(students)
head(students)

# =============================================================================
# TASK 1: Know Your Groups
# Question: How many students are in each group? (Unbalanced groups matter!)
# =============================================================================

students %>% count(study_method)
students %>% count(device)
students %>% count(campus_zone)

# =============================================================================
# TASK 2: Two-Sample t-test (Welch)
# Question: Do students who take notes on paper score differently on the exam
#           than students who take notes on a laptop?
# =============================================================================

# Step 1: LOOK at the data before testing.
# TODO 2: Add a boxplot layer to the plot below.
# Hint: geom_boxplot(alpha = 0.4, outlier.shape = NA)
p_device <- ggplot(students, aes(x = device, y = exam_score, fill = device)) +
  # YOUR CODE HERE (remember the + at the end of the line)
  geom_jitter(width = 0.15, alpha = 0.35) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  labs(title = "Exam Score by Note-Taking Device",
       subtitle = "White diamond = group mean",
       x = "Device", y = "Exam Score") +
  theme_minimal() +
  theme(legend.position = "none")
show_plot(p_device, "01_device_boxplot")

# Step 2: Check what the plot suggests. Do the boxes overlap a lot? Are the means far apart?
students %>%
  group_by(device) %>%
  summarize(n = n(), mean_exam = mean(exam_score), sd_exam = sd(exam_score))

# Step 3: Run the test.
# TODO 3: This line is broken! The formula must be  outcome ~ group.
# Hint: The outcome (numeric) goes on the LEFT of the ~ and the grouping variable goes on the RIGHT.
device_test <- t.test(device ~ exam_score, data = students)
print(device_test)

# Step 4: A p-value only tells you IF there is a difference. Effect size tells you HOW BIG it is.
# TODO 4: Calculate Cohen's d = (difference in means) / (pooled standard deviation).
# Hint: pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)  (this simple form is fine when group sizes are similar)
# Hint: device_summary has one row per device. Use device_summary$sd_exam and diff(device_summary$mean_exam).
device_summary <- students %>%
  group_by(device) %>%
  summarize(mean_exam = mean(exam_score), sd_exam = sd(exam_score))
mean_diff <- NULL # YOUR CODE HERE (use abs() so the difference is positive)
pooled_sd <- NULL # YOUR CODE HERE
cohens_d <- mean_diff / pooled_sd
cat("Cohen's d for device:", round(cohens_d, 2), "\n") # ~0.2 small, ~0.5 medium, ~0.8 large

# =============================================================================
# TASK 3: Paired t-test
# Question: Did students improve from the pre_test to the post_test?
# (The SAME student took both tests, so the scores are paired.)
# =============================================================================

# Points above the dashed line are students who improved.
p_paired <- ggplot(students, aes(x = pre_test, y = post_test)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Post-test vs Pre-test Score",
       subtitle = "Dashed line = no change",
       x = "Pre-test", y = "Post-test") +
  theme_minimal()
show_plot(p_paired, "02_paired_scatter")

students <- students %>% mutate(improvement = post_test - pre_test)

p_improve <- ggplot(students, aes(x = improvement)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Distribution of Improvement (Post - Pre)",
       x = "Improvement (points)", y = "Count") +
  theme_minimal()
show_plot(p_improve, "03_improvement_hist")

# TODO 5: This paired test is broken! It is treating the scores as two independent groups.
# Hint: A paired t-test needs  paired = TRUE.
paired_test <- t.test(students$post_test, students$pre_test, paired = FALSE)
print(paired_test)

# =============================================================================
# TASK 4: One-Way ANOVA (3+ groups)
# Question: Does exam score differ across the three study methods?
# =============================================================================

# With three groups we should NOT run three t-tests (that inflates the false-positive rate).
# ANOVA asks one question: "Are ALL the group means equal?"
p_method <- ggplot(students, aes(x = study_method, y = exam_score, fill = study_method)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  labs(title = "Exam Score by Study Method",
       x = "Study Method", y = "Exam Score") +
  theme_minimal() +
  theme(legend.position = "none")
show_plot(p_method, "04_method_boxplot")

# TODO 6: Fit a one-way ANOVA of exam_score by study_method and save it as 'method_aov'.
# Hint: aov(outcome ~ group, data = students)
method_aov <- NULL # YOUR CODE HERE
summary(method_aov)

# ANOVA only says "at least one mean is different". Which ones? Use a post-hoc test.
# TODO 7: Run Tukey's HSD on 'method_aov' and save it as 'method_tukey'.
# Hint: TukeyHSD(method_aov). Confidence intervals that do NOT contain 0 are significant differences.
method_tukey <- NULL # YOUR CODE HERE
print(method_tukey)
plot(method_tukey, las = 1, col = "steelblue") # intervals crossing 0 are NOT significant

# Assumption check: residuals should look roughly normal (points near the line).
qqnorm(residuals(method_aov), main = "Normal Q-Q Plot of ANOVA Residuals")
qqline(residuals(method_aov), col = "red")

# =============================================================================
# TASK 5: An ANOVA You Probably Don't Need
# Question: Does exam score differ by campus zone (North/South/East/West)?
# =============================================================================

p_zone <- ggplot(students, aes(x = campus_zone, y = exam_score, fill = campus_zone)) +
  geom_boxplot(alpha = 0.4, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
  labs(title = "Exam Score by Campus Zone",
       x = "Campus Zone", y = "Exam Score") +
  theme_minimal() +
  theme(legend.position = "none")
show_plot(p_zone, "05_zone_boxplot")

zone_aov <- aov(exam_score ~ campus_zone, data = students)
summary(zone_aov)

# TODO 8: Pull the p-value out of the ANOVA table and save it as 'zone_p'.
# Hint: summary(zone_aov) is a list. The first element is the ANOVA table, and its "Pr(>F)" column holds the p-values.
zone_p <- NULL # YOUR CODE HERE

# TODO 9: This decision rule is backwards! Fix it.
# Hint: We REJECT the null hypothesis (significant) when the p-value is LESS than alpha.
zone_decision <- ifelse(zone_p > alpha,
                        "Significant: at least one zone mean differs",
                        "Not significant: no evidence that zones differ")
cat("Campus zone ANOVA p-value:", round(zone_p, 4), "->", zone_decision, "\n")

# =============================================================================
# TASK 6: One-Way MANOVA (several outcomes at once)
# Question: Do the study methods differ on exam score, project score and stress
#           level TOGETHER?
# =============================================================================

# Why MANOVA? The outcomes are correlated. This heatmap shows how strongly.
outcomes <- c("exam_score", "project_score", "stress_level")
cor_long <- as.data.frame(as.table(cor(students[, outcomes]))) %>%
  rename(var1 = Var1, var2 = Var2, correlation = Freq)

p_cor <- ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2))) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  labs(title = "Correlation Between Outcome Variables", x = NULL, y = NULL) +
  theme_minimal()
show_plot(p_cor, "06_outcome_correlation")

# TODO 10: Fit the MANOVA and save it as 'method_manova'.
# Hint: manova(cbind(y1, y2, y3) ~ group, data = students). cbind() glues the outcome columns together.
method_manova <- NULL # YOUR CODE HERE
summary(method_manova, test = "Pillai") # the overall (multivariate) test

# If the overall MANOVA is significant, follow up with one ANOVA per outcome.
summary.aov(method_manova)

# Heatmap of group means. Each outcome is standardized (z-score of every student) BEFORE averaging so
# that exam points and stress ratings can share one color scale. A value of 0.5 means
# "the group average is half a standard deviation above the overall average".
group_means <- students %>%
  mutate(across(all_of(outcomes), ~ as.numeric(scale(.x)))) %>%
  group_by(study_method) %>%
  summarize(across(all_of(outcomes), mean)) %>%
  pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "z_mean")

# TODO 11: Add the heatmap tiles. Map fill to the standardized mean.
# Hint: geom_tile(aes(fill = z_mean), color = "white")
p_heat <- ggplot(group_means, aes(x = study_method, y = outcome)) +
  # YOUR CODE HERE (remember the + at the end of the line)
  geom_text(aes(label = round(z_mean, 2))) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue") +
  labs(title = "Standardized Group Means by Study Method",
       subtitle = "Blue = above the overall average, red = below (in standard deviations)",
       x = "Study Method", y = NULL, fill = "z of mean") +
  theme_minimal()
show_plot(p_heat, "07_group_mean_heatmap")

# =============================================================================
# TASK 7: Summary of Findings
# Question: Which tests found statistically significant differences?
# =============================================================================

manova_p <- summary(method_manova, test = "Pillai")$stats[1, "Pr(>F)"]

results <- tibble(
  test = c("Welch t-test (device)", "Paired t-test (post vs pre)",
           "ANOVA (study method)", "ANOVA (campus zone)", "MANOVA (study method)"),
  p_value = c(device_test$p.value, paired_test$p.value,
              summary(method_aov)[[1]][["Pr(>F)"]][1], zone_p, manova_p)
) %>%
  mutate(significant = p_value < alpha,
         p_value = signif(p_value, 3))
print(results)

# Add your own comments here describing what you learned from the plots and the tests.
# Remember: significant results should match what you SAW in the plots.

(Did you remember to add your name and date at the top of this file?)
