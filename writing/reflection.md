## Reflection Questions

Name: Add Your Name Here
Date: Add Date Here

---

Reflection Questions: Comparing Groups with t-tests, ANOVA and MANOVA

## Overview

After completing your analysis of the student study-habits dataset and exploring the Shiny app, take time to reflect on what the tests, plots and p-values actually told you. The aim of this reflection is not just to repeat the output of R, but to explain the results and conclusions in your own words using clear and meaningful language. Please refer to specific numbers and plots from your work in your discussion.

---

## Part 1. Your R Analysis (8 Questions)

No one-liner answers. Please respond to your questions using clear and meaningful language (about 100 words each).

### Section A. t-tests

Considering your analysis of exam scores by note-taking device and of pre-test versus post-test scores, please address the following questions.

Q1. Describe what the device boxplot showed before you ran the t-test, and then explain what the t-test's p-value and confidence interval tell you in plain language. Why did the code use a Welch t-test?

TODO

Q2. A p-value says whether a difference is detectable; Cohen's d says how large it is. What was your Cohen's d for device, and is the difference statistically significant, practically important, or both? Would you advise a student to switch to paper notes based on this result?

TODO


Q3. Why is the pre-test/post-test comparison a paired t-test rather than an independent t-test? What did the scatterplot and the histogram of improvements show, and what goes wrong in the analysis if `paired = FALSE`?

TODO

### Section B. ANOVA

Considering your analysis of exam scores by study method and by campus zone, please address the following questions.

Q4. Why not run three separate t-tests to compare Passive Review, Flashcards and Practice Tests? State the null hypothesis of the ANOVA, and explain what the Tukey HSD results added that the ANOVA alone could not.

TODO

Q5. Compare the study-method plot and ANOVA with the campus-zone plot and ANOVA. How did the plots predict the p-values? What does a non-significant result for campus zone mean, and what does it NOT mean?

TODO

### Section C. MANOVA

Considering your analysis of exam score, project score and stress level together, please address the following questions.

Q6. Why might you use one MANOVA instead of three separate ANOVAs? Use the outcome correlation heatmap in your explanation.

TODO

Q7. Describe the group-means heatmap. Which study method looks best and worst on each outcome, and did the follow-up ANOVAs agree with what you saw?

TODO

Q8. What assumptions do these tests rely on (for example normality, equal variances, independent observations), and what limitations should you keep in mind? Consider that the data are synthetic, that the study-method groups were not necessarily randomly assigned, and that you ran several tests in one analysis.

TODO

---

## Part 2. The Shiny App (3 Questions)

Please base these answers on your experiments with the Shiny app. Mention which dataset and settings you used.

Q9. Using the Simulator with the true effect set to 0, click "New random sample" at least 20 times and record how often the p-value falls below alpha. What did you observe, and what does this tell you about what alpha = 0.05 means?

TODO

Q10. In the Simulator, set a small true effect (about 0.3) and increase "Students per group" from 10 to 200. How did the plot and the p-value change? What does this tell you about the difference between statistical significance and practical importance?

TODO

Q11. Choose one dataset in the app where the plot made a test look unnecessary (or obvious) and one where the plot could not settle the question. Did running the test anyway change your thinking? When is a test truly necessary?

TODO

---

(Did you remember to add your name to the top of this document?)
