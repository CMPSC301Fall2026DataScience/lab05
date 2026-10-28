# =============================================================================
# CMPSC 301: Data Science -- Lab 05 Companion Shiny App
# "Look first, then test": explore t-tests, ANOVA and MANOVA interactively.
#
# Run from RStudio (Run App button) or the console:
#   shiny::runApp("shiny")
# Packages needed: shiny, ggplot2 (both come with the tidyverse install).
# =============================================================================

library(shiny)
library(ggplot2)

# ---- Data sources -----------------------------------------------------------

lab_paths <- c("../data/student_wellness.csv", "data/student_wellness.csv")
lab_file <- lab_paths[file.exists(lab_paths)][1]

dataset_choices <- c(
  if (!is.na(lab_file)) "Lab 05: Student Wellness",
  "iris (flowers)", "ToothGrowth (guinea pigs)", "PlantGrowth (plants)",
  "chickwts (chicken feed)", "mtcars (cars)", "Simulator (you control the truth)"
)

# Which grouping variable / responses to preselect for each dataset.
dataset_defaults <- list(
  "Lab 05: Student Wellness"           = list(group = "study_method", resp = c("exam_score", "project_score", "stress_level")),
  "iris (flowers)"                     = list(group = "Species", resp = c("Sepal.Length", "Petal.Length", "Petal.Width")),
  "ToothGrowth (guinea pigs)"          = list(group = "dose", resp = "len"),
  "PlantGrowth (plants)"               = list(group = "group", resp = "weight"),
  "chickwts (chicken feed)"            = list(group = "feed", resp = "weight"),
  "mtcars (cars)"                      = list(group = "cyl", resp = c("mpg", "hp", "wt")),
  "Simulator (you control the truth)"  = list(group = "group", resp = c("y1", "y2", "y3"))
)

# Turn "group-like" columns into factors and drop ID-like columns.
prep_data <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.character(x) && length(unique(x)) > 8) {
      df[[nm]] <- NULL
    } else if (is.character(x) || (is.numeric(x) && length(unique(x)) <= 5)) {
      df[[nm]] <- factor(x)
    }
  }
  df
}

simulate_data <- function(k, n, effect, sd, seed) {
  set.seed(seed)
  g <- factor(rep(LETTERS[seq_len(k)], each = n))
  idx <- as.numeric(g) - 1
  z0 <- rnorm(k * n) # shared noise makes the three outcomes correlated
  noise <- function() sd * (sqrt(0.5) * z0 + sqrt(0.5) * rnorm(k * n))
  data.frame(
    group = g,
    y1 = 50 + effect * sd * idx + noise(),
    y2 = 50 + 0.7 * effect * sd * idx + noise(),
    y3 = 50 - 0.5 * effect * sd * idx + noise()
  )
}

# ---- Plot helpers -----------------------------------------------------------

box_plot <- function(d, g, y) {
  ggplot(d, aes(x = .data[[g]], y = .data[[y]], fill = .data[[g]])) +
    geom_boxplot(alpha = 0.35, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.35) +
    stat_summary(fun.data = mean_cl_normal_safe, geom = "errorbar", width = 0.25, linewidth = 1) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +
    labs(title = paste(y, "by", g),
         subtitle = "White diamond = mean, black bars = 95% CI of the mean",
         x = g, y = y) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
}

mean_cl_normal_safe <- function(x) {
  m <- mean(x); se <- sd(x) / sqrt(length(x))
  h <- qt(0.975, max(length(x) - 1, 1)) * se
  data.frame(y = m, ymin = m - h, ymax = m + h)
}

# Eyeball guide: how far apart are the group means compared with the spread of the data?
eyeball <- function(d, g, y) {
  means <- tapply(d[[y]], d[[g]], mean)
  gap <- diff(range(means)) / sd(d[[y]])
  verdict <- if (gap < 0.2) {
    list("🟥", "Groups look almost identical. Expect NOT significant; a test is unlikely to find anything.")
  } else if (gap < 0.8) {
    list("🟨", "Some separation, but the boxes overlap. You CAN'T decide by eye, so a test is NECESSARY.")
  } else {
    list("🟩", "The groups are clearly separated. A test will almost surely confirm what you can already see.")
  }
  list(gap = gap, icon = verdict[[1]], text = verdict[[2]])
}

# Shade the p-value area under the null distribution.
null_plot <- function(type, stat, df1, df2, p, alpha) {
  if (type == "t") {
    x <- seq(-max(4.5, abs(stat) * 1.3), max(4.5, abs(stat) * 1.3), length.out = 600)
    dens <- dt(x, df1)
    crit <- qt(1 - alpha / 2, df1)
    d <- data.frame(x, dens, tail = abs(x) >= abs(stat))
    gg <- ggplot(d, aes(x, dens)) +
      geom_area(data = subset(d, tail), fill = "firebrick", alpha = 0.6) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = c(-crit, crit), linetype = "dotted") +
      geom_vline(xintercept = stat, color = "firebrick", linewidth = 1) +
      labs(title = "If there were truly NO difference, how surprising is your t?",
           subtitle = sprintf("Red line = observed t (%.2f). Red area = p-value (%.4f). Dotted = cutoffs for alpha = %.2f.", stat, p, alpha),
           x = "t statistic", y = "Density")
  } else {
    xmax <- max(qf(0.999, df1, df2), stat * 1.3)
    x <- seq(0.001, xmax, length.out = 600)
    dens <- df(x, df1, df2)
    crit <- qf(1 - alpha, df1, df2)
    d <- data.frame(x, dens, tail = x >= stat)
    gg <- ggplot(d, aes(x, dens)) +
      geom_area(data = subset(d, tail), fill = "firebrick", alpha = 0.6) +
      geom_line(linewidth = 1) +
      geom_vline(xintercept = crit, linetype = "dotted") +
      geom_vline(xintercept = stat, color = "firebrick", linewidth = 1) +
      labs(title = "If there were truly NO difference, how surprising is your F?",
           subtitle = sprintf("Red line = observed F (%.2f). Red area = p-value (%.4f). Dotted = cutoff for alpha = %.2f.", stat, p, alpha),
           x = "F statistic", y = "Density")
  }
  gg + theme_minimal(base_size = 14)
}

# ---- UI ---------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Lab 05: Look First, Then Test (t-test, ANOVA, MANOVA)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("dataset", "1. Choose data", dataset_choices),
      conditionalPanel(
        "input.dataset == 'Simulator (you control the truth)'",
        sliderInput("sim_k", "Number of groups", 2, 4, 3, step = 1),
        sliderInput("sim_n", "Students per group", 10, 200, 40, step = 5),
        sliderInput("sim_effect", "True effect (gap between groups, in SDs)", 0, 1.5, 0.5, step = 0.05),
        sliderInput("sim_sd", "Noise (SD)", 1, 20, 10),
        numericInput("sim_seed", "Random seed", 1, min = 1),
        actionButton("resample", "New random sample")
      ),
      radioButtons("test", "2. Choose a test",
                   c("t-test (2 groups, 1 outcome)" = "t",
                     "ANOVA (2+ groups, 1 outcome)" = "anova",
                     "MANOVA (2+ groups, 2+ outcomes)" = "manova")),
      helpText("MANOVA needs 2+ outcomes: use Lab 05, iris, mtcars or the Simulator."),
      uiOutput("group_ui"),
      uiOutput("levels_ui"),
      uiOutput("resp_ui"),
      conditionalPanel("input.test == 't'", checkboxInput("eqvar", "Assume equal variances (Student instead of Welch)", FALSE)),
      conditionalPanel("input.test == 'manova'",
                       selectInput("mtest", "MANOVA statistic", c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"))),
      sliderInput("alpha", "Significance level (alpha)", 0.01, 0.10, 0.05, step = 0.01)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel("1. Look at the plot",
                 br(), actionButton("code_plot", "Show R code for this step", icon = icon("code")), br(), br(),
                 uiOutput("eyeball_ui"), br(),
                 uiOutput("plot_area")),
        tabPanel("2. Run the test",
                 br(),
                 p("Done looking? Run the test even if the plot told you it isn't needed, then compare the result with what you saw."),
                 actionButton("run", "Run the test", class = "btn-primary"),
                 actionButton("code_test", "Show R code for this step", icon = icon("code")),
                 br(), br(),
                 uiOutput("result_ui")),
        tabPanel("3. Check assumptions",
                 br(), actionButton("code_assump", "Show R code for this step", icon = icon("code")), br(), br(),
                 plotOutput("qq_plot", height = 380), br(),
                 tableOutput("assump_table"),
                 p(em("Rule of thumb: points near the line mean the residuals are roughly normal. A Bartlett p-value below 0.05 suggests unequal spreads (use Welch for t-tests)."))),
        tabPanel("Try this",
                 br(),
                 h4("Guided experiments"),
                 tags$ol(
                   tags$li("Lab data, ANOVA, study_method vs exam_score. Look first, then run the test. Repeat with campus_zone. What differs in the plot AND in the p-value?"),
                   tags$li("iris, MANOVA, Species with three outcomes. Which outcome shows the strongest separation in the heatmap?"),
                   tags$li("Simulator, set the true effect to 0 and click 'New random sample' 20 times. How often is p < alpha? What does alpha mean?"),
                   tags$li("Simulator, set the effect to 0.3 and move 'Students per group' from 10 to 200. What changes in the plot? In the p-value?"),
                   tags$li("Simulator, 4 groups, try the t-test. Pick two groups and compare their p-value with ANOVA across all four."),
                   tags$li("ToothGrowth, ANOVA of len by supp vs by dose. Which grouping variable matters more?")
                 ))
      )
    )
  )
)

# ---- Server -----------------------------------------------------------------

server <- function(input, output, session) {

  raw <- reactive({
    nm <- input$dataset
    if (nm == "Simulator (you control the truth)") {
      input$resample
      seed <- isolate(input$sim_seed)
      simulate_data(input$sim_k, input$sim_n, input$sim_effect, input$sim_sd,
                    seed + (input$resample %||% 0))
    } else {
      prep_data(switch(nm,
        "Lab 05: Student Wellness" = read.csv(lab_file),
        "iris (flowers)" = iris,
        "ToothGrowth (guinea pigs)" = ToothGrowth,
        "PlantGrowth (plants)" = PlantGrowth,
        "chickwts (chicken feed)" = chickwts,
        "mtcars (cars)" = mtcars))
    }
  })

  fac_cols <- reactive(names(raw())[vapply(raw(), is.factor, logical(1))])
  num_cols <- reactive(names(raw())[vapply(raw(), is.numeric, logical(1))])
  defaults <- reactive(dataset_defaults[[input$dataset]])

  output$group_ui <- renderUI({
    sel <- defaults()$group
    selectInput("group", "3. Grouping variable", fac_cols(), selected = if (sel %in% fac_cols()) sel else fac_cols()[1])
  })

  output$levels_ui <- renderUI({
    req(input$group, input$test == "t")
    lv <- levels(raw()[[input$group]])
    if (length(lv) <= 2) return(NULL)
    tagList(
      selectInput("lvl1", "Compare group", lv, selected = lv[1]),
      selectInput("lvl2", "with group", lv, selected = lv[2])
    )
  })

  output$resp_ui <- renderUI({
    req(input$group)
    choices <- setdiff(num_cols(), input$group)
    def <- intersect(defaults()$resp, choices)
    if (input$test == "manova") {
      selectInput("resp_multi", "4. Outcomes (pick 2 or more)", choices,
                  selected = if (length(def) >= 2) def else choices[1:2], multiple = TRUE)
    } else {
      selectInput("resp", "4. Outcome", choices, selected = if (length(def)) def[1] else choices[1])
    }
  })

  responses <- reactive({
    if (input$test == "manova") req(input$resp_multi) else req(input$resp)
  })

  work <- reactive({
    req(input$group, responses())
    d <- raw()
    g <- input$group
    if (input$test == "t") {
      lv <- levels(d[[g]])
      keep <- if (length(lv) > 2) {
        req(input$lvl1, input$lvl2)
        validate(need(input$lvl1 != input$lvl2, "Pick two DIFFERENT groups to compare."))
        c(input$lvl1, input$lvl2)
      } else lv
      d <- d[d[[g]] %in% keep, ]
    }
    d <- d[complete.cases(d[, c(g, responses())]), c(g, responses())]
    d[[g]] <- droplevels(d[[g]])
    validate(need(nlevels(d[[g]]) >= 2, "Need at least 2 groups."),
             need(all(table(d[[g]]) >= 3), "Each group needs at least 3 observations."))
    if (input$test == "manova") validate(need(length(responses()) >= 2, "MANOVA needs at least 2 outcomes."))
    d
  })

  # Results are hidden whenever the inputs change, so students must re-run (and re-think).
  ran <- reactiveVal(FALSE)
  observeEvent(input$run, ran(TRUE))
  observeEvent(list(input$dataset, input$test, input$group, input$lvl1, input$lvl2, input$resp,
                    input$resp_multi, input$eqvar, input$mtest, input$sim_k, input$sim_n,
                    input$sim_effect, input$sim_sd, input$sim_seed, input$resample),
               ran(FALSE), ignoreInit = TRUE)

  # ---- Tab 1: plots ----
  output$eyeball_ui <- renderUI({
    d <- work(); g <- input$group
    items <- lapply(responses(), function(y) {
      e <- eyeball(d, g, y)
      tags$li(tags$b(y), sprintf(" (largest gap between means = %.2f SD): ", e$gap), e$icon, " ", e$text)
    })
    wellPanel(h4("Eyeball guide (this is NOT a test)"), tags$ul(items))
  })

  output$plot_area <- renderUI({
    d <- work()
    if (input$test == "manova") {
      tagList(
        plotOutput("heat_plot", height = 320),
        plotOutput("scatter_plot", height = 420)
      )
    } else {
      plotOutput("box_plot", height = 480)
    }
  })

  output$box_plot <- renderPlot({
    req(input$test != "manova")
    box_plot(work(), input$group, responses())
  })

  heat_data <- reactive({
    d <- work(); g <- input$group; ys <- responses()
    z <- d
    z[ys] <- lapply(z[ys], function(v) as.numeric(scale(v)))
    m <- aggregate(z[ys], list(group = z[[g]]), mean)
    out <- do.call(rbind, lapply(ys, function(y) data.frame(group = m$group, outcome = y, z_mean = m[[y]])))
    out
  })

  output$heat_plot <- renderPlot({
    ggplot(heat_data(), aes(group, outcome, fill = z_mean)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(z_mean, 2)), size = 5) +
      scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue") +
      labs(title = "Standardized group means (heatmap)",
           subtitle = "Blue = above overall average, red = below (in SDs). Pale everywhere = groups look the same.",
           x = input$group, y = NULL, fill = "SDs") +
      theme_minimal(base_size = 14)
  })

  output$scatter_plot <- renderPlot({
    d <- work(); ys <- responses()
    ggplot(d, aes(.data[[ys[1]]], .data[[ys[2]]], color = .data[[input$group]])) +
      geom_point(alpha = 0.5) +
      stat_ellipse(linewidth = 1) +
      labs(title = paste("Two of your outcomes together:", ys[1], "vs", ys[2]),
           subtitle = "Ellipses = where most of each group sits. Separated ellipses = a multivariate difference.",
           color = input$group) +
      theme_minimal(base_size = 14)
  })

  # ---- Tab 2: test ----
  test_res <- reactive({
    d <- work(); g <- input$group; ys <- responses()
    if (input$test == "t") {
      fit <- t.test(d[[ys]] ~ d[[g]], var.equal = input$eqvar)
      list(type = "t", print = fit, stat = unname(fit$statistic), df1 = unname(fit$parameter), df2 = NA,
           p = fit$p.value)
    } else if (input$test == "anova") {
      fit <- aov(reformulate(g, ys), data = d)
      tab <- summary(fit)[[1]]
      list(type = "F", fit = fit, print = summary(fit), stat = tab[["F value"]][1], df1 = tab[["Df"]][1],
           df2 = tab[["Df"]][2], p = tab[["Pr(>F)"]][1])
    } else {
      f <- as.formula(paste("cbind(", paste0("`", ys, "`", collapse = ", "), ") ~ `", g, "`", sep = ""))
      fit <- tryCatch(manova(f, data = d), error = function(e) NULL)
      validate(need(!is.null(fit), "MANOVA could not be fit (are the outcomes nearly identical?)."))
      s <- summary(fit, test = input$mtest)
      st <- s$stats[1, ]
      list(type = "F", fit = fit, print = s, follow = summary.aov(fit), stat = unname(st["approx F"]),
           df1 = unname(st["num Df"]), df2 = unname(st["den Df"]), p = unname(st["Pr(>F)"]))
    }
  })

  output$result_ui <- renderUI({
    req(ran())
    r <- test_res()
    sig <- r$p < input$alpha
    e_txt <- {
      gaps <- vapply(responses(), function(y) eyeball(work(), input$group, y)$gap, numeric(1))
      if (all(gaps < 0.2)) "The plot showed almost identical groups"
      else if (any(gaps >= 0.8)) "The plot showed clearly separated groups"
      else "The plot showed some overlap"
    }
    tagList(
      wellPanel(
        h4(sprintf("p-value = %s", format.pval(r$p, digits = 3, eps = 1e-16))),
        if (sig) tags$b(style = "color:#b22222;", sprintf("Statistically significant at alpha = %.2f: reject the null (the means are not all equal).", input$alpha))
        else tags$b(style = "color:#555;", sprintf("NOT significant at alpha = %.2f: not enough evidence that the means differ.", input$alpha)),
        p(em(paste0(e_txt, ". Does the test agree with your eyes?")))
      ),
      plotOutput("null_plot", height = 330),
      if (input$test == "anova") tagList(h4("Which groups differ? (Tukey HSD)"), plotOutput("tukey_plot", height = 300)),
      h4("R output"),
      verbatimTextOutput("raw_print"),
      if (input$test == "manova") tagList(h4("Follow-up: one ANOVA per outcome"), verbatimTextOutput("follow_print"))
    )
  })

  output$null_plot <- renderPlot({
    req(ran()); r <- test_res()
    null_plot(r$type, r$stat, r$df1, r$df2, r$p, input$alpha)
  })

  output$raw_print <- renderPrint({ req(ran()); test_res()$print })
  output$follow_print <- renderPrint({ req(ran(), input$test == "manova"); test_res()$follow })

  output$tukey_plot <- renderPlot({
    req(ran(), input$test == "anova")
    tk <- as.data.frame(TukeyHSD(test_res()$fit)[[1]])
    tk$pair <- rownames(tk)
    ggplot(tk, aes(x = diff, y = pair)) +
      geom_pointrange(aes(xmin = lwr, xmax = upr, color = `p adj` < input$alpha), linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      scale_color_manual(values = c("TRUE" = "firebrick", "FALSE" = "grey40"), name = "Significant") +
      labs(title = "Pairwise differences in means (95% family-wise CI)",
           subtitle = "Intervals that cross the dashed zero line are NOT significant", x = "Difference in means", y = NULL) +
      theme_minimal(base_size = 14)
  })

  # ---- Tab 3: assumptions ----
  output$qq_plot <- renderPlot({
    d <- work(); g <- input$group; ys <- responses()
    res <- do.call(rbind, lapply(ys, function(y) {
      data.frame(outcome = y, resid = residuals(lm(reformulate(g, y), data = d)))
    }))
    ggplot(res, aes(sample = resid)) +
      stat_qq(alpha = 0.5) + stat_qq_line(color = "red") +
      facet_wrap(~ outcome, scales = "free") +
      labs(title = "Normal Q-Q plot of residuals", x = "Theoretical quantiles", y = "Sample quantiles") +
      theme_minimal(base_size = 14)
  })

  output$assump_table <- renderTable({
    d <- work(); g <- input$group
    do.call(rbind, lapply(responses(), function(y) {
      data.frame(Outcome = y,
                 `Smallest group n` = min(table(d[[g]])),
                 `Largest SD / smallest SD` = round(max(tapply(d[[y]], d[[g]], sd)) / min(tapply(d[[y]], d[[g]], sd)), 2),
                 `Bartlett p` = signif(bartlett.test(d[[y]], d[[g]])$p.value, 3),
                 check.names = FALSE)
    }))
  })

  # ---- "Show R code" buttons ----
  bt <- function(x) ifelse(make.names(x) == x, x, paste0("`", x, "`"))
  qv <- function(x) paste0('"', x, '"')
  cv <- function(x) paste0("c(", paste(qv(x), collapse = ", "), ")")

  # Code that rebuilds the data frame 'd' exactly as the app prepared it.
  code_data <- reactive({
    nm <- input$dataset; g <- input$group; ys <- responses()
    load <- switch(nm,
      "Lab 05: Student Wellness" = 'd <- read.csv("data/student_wellness.csv")',
      "iris (flowers)" = "d <- iris",
      "ToothGrowth (guinea pigs)" = "d <- ToothGrowth",
      "PlantGrowth (plants)" = "d <- PlantGrowth",
      "chickwts (chicken feed)" = "d <- chickwts",
      "mtcars (cars)" = "d <- mtcars",
      c("# Simulated data: groups share some noise, so the outcomes are correlated",
        sprintf("set.seed(%d)", isolate(input$sim_seed) + (input$resample %||% 0)),
        sprintf("k <- %d; n <- %d; effect <- %s; sd <- %d", input$sim_k, input$sim_n, input$sim_effect, input$sim_sd),
        "group <- factor(rep(LETTERS[1:k], each = n))",
        "idx <- as.numeric(group) - 1",
        "z0 <- rnorm(k * n)",
        "noise <- function() sd * (sqrt(0.5) * z0 + sqrt(0.5) * rnorm(k * n))",
        "d <- data.frame(group,",
        "                y1 = 50 + effect * sd * idx + noise(),",
        "                y2 = 50 + 0.7 * effect * sd * idx + noise(),",
        "                y3 = 50 - 0.5 * effect * sd * idx + noise())"))
    lines <- c(load,
               sprintf('d[["%s"]] <- factor(d[["%s"]])  # groups must be factors', g, g))
    if (input$test == "t") {
      keep <- levels(work()[[g]])
      lines <- c(lines,
                 sprintf('d <- d[d[["%s"]] %%in%% %s, ]  # keep the two groups being compared', g, cv(keep)),
                 sprintf('d[["%s"]] <- droplevels(d[["%s"]])', g, g))
    }
    c(lines, sprintf("d <- d[complete.cases(d[, %s]), ]  # drop rows with missing values", cv(c(g, ys))))
  })

  code_plot_text <- reactive({
    g <- input$group; ys <- responses()
    if (input$test == "manova") {
      zs <- sapply(ys, function(y) sprintf('z[["%s"]] <- as.numeric(scale(z[["%s"]]))', y, y))
      paste(c("library(ggplot2)", "", code_data(), "",
        "# Heatmap: standardize each outcome, then average within each group",
        "z <- d", zs,
        sprintf("m <- aggregate(z[, %s], list(group = z[[\"%s\"]]), mean)", cv(ys), g),
        "heat <- do.call(rbind, lapply(", sprintf("  %s, function(y) data.frame(group = m$group, outcome = y, z_mean = m[[y]])))", cv(ys)),
        "",
        "ggplot(heat, aes(group, outcome, fill = z_mean)) +",
        '  geom_tile(color = "white") +',
        "  geom_text(aes(label = round(z_mean, 2))) +",
        '  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue") +',
        sprintf('  labs(x = "%s", y = NULL, fill = "SDs") +', g),
        "  theme_minimal(base_size = 14)", "",
        "# Scatterplot of the first two outcomes with a group ellipse",
        sprintf("ggplot(d, aes(%s, %s, color = %s)) +", bt(ys[1]), bt(ys[2]), bt(g)),
        "  geom_point(alpha = 0.5) +", "  stat_ellipse(linewidth = 1) +", "  theme_minimal(base_size = 14)"),
        collapse = "\n")
    } else {
      y <- ys
      paste(c("library(ggplot2)", "", code_data(), "",
        "# Boxplot + individual points + mean (diamond) with a 95% CI of the mean",
        sprintf("ggplot(d, aes(x = %s, y = %s, fill = %s)) +", bt(g), bt(y), bt(g)),
        "  geom_boxplot(alpha = 0.35, outlier.shape = NA) +",
        "  geom_jitter(width = 0.15, alpha = 0.35) +",
        "  stat_summary(fun.data = function(x) {",
        "    m <- mean(x); h <- qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))",
        "    data.frame(y = m, ymin = m - h, ymax = m + h)",
        '  }, geom = "errorbar", width = 0.25, linewidth = 1) +',
        '  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white") +',
        sprintf('  labs(title = "%s by %s", x = "%s", y = "%s") +', y, g, g, y),
        "  theme_minimal(base_size = 14) +", '  theme(legend.position = "none")'),
        collapse = "\n")
    }
  })

  code_test_text <- reactive({
    g <- input$group; ys <- responses()
    body <- if (input$test == "t") {
      c(sprintf("# %s t-test: outcome ~ group",
                if (isTRUE(input$eqvar)) "Student (equal variances)" else "Welch (does not assume equal variances)"),
        sprintf("res <- t.test(%s ~ %s, data = d%s)", bt(ys), bt(g), if (isTRUE(input$eqvar)) ", var.equal = TRUE" else ""),
        "res", "res$p.value  # compare with alpha")
    } else if (input$test == "anova") {
      c("# One-way ANOVA: are ALL group means equal?",
        sprintf("fit <- aov(%s ~ %s, data = d)", bt(ys), bt(g)),
        "summary(fit)", "",
        "# Post-hoc: which pairs differ? Intervals that cross 0 are not significant",
        "tk <- TukeyHSD(fit)", "tk", "plot(tk, las = 1)")
    } else {
      c("# One-way MANOVA: do the groups differ on the outcomes TOGETHER?",
        sprintf("fit <- manova(cbind(%s) ~ %s, data = d)", paste(bt(ys), collapse = ", "), bt(g)),
        sprintf('summary(fit, test = "%s")', input$mtest), "",
        "# Follow-up: one ANOVA per outcome", "summary.aov(fit)")
    }
    paste(c(code_data(), "", body, "", sprintf("alpha <- %s  # significant if p < alpha", input$alpha)), collapse = "\n")
  })

  code_assump_text <- reactive({
    g <- input$group; ys <- responses()
    paste(c("library(ggplot2)", "", code_data(), "",
      "# Normal Q-Q plot of the residuals (points near the line = roughly normal)",
      "res <- do.call(rbind, lapply(", sprintf("  %s, function(y) {", cv(ys)),
      sprintf('    data.frame(outcome = y, resid = residuals(lm(reformulate("%s", y), data = d)))', g),
      "  }))",
      "ggplot(res, aes(sample = resid)) +",
      '  stat_qq(alpha = 0.5) + stat_qq_line(color = "red") +',
      '  facet_wrap(~ outcome, scales = "free") +', "  theme_minimal(base_size = 14)", "",
      "# Equal spreads? Bartlett p < 0.05 suggests unequal variances",
      sprintf("for (y in %s) {", cv(ys)),
      sprintf('  cat(y, "Bartlett p =", bartlett.test(d[[y]], d[["%s"]])$p.value, "\\n")', g),
      sprintf('  print(tapply(d[[y]], d[["%s"]], sd))  # SD in each group', g),
      "}"), collapse = "\n")
  })

  show_code <- function(title, txt) {
    showModal(modalDialog(
      title = title, size = "l", easyClose = TRUE,
      p("Paste this into a fresh R script (or the console). The lab data version expects your working directory to be the lab folder."),
      tags$pre(style = "max-height:60vh; overflow:auto;", txt),
      footer = modalButton("Close")
    ))
  }
  observeEvent(input$code_plot,   show_code("R code: the plot", code_plot_text()))
  observeEvent(input$code_test,   show_code("R code: the test", code_test_text()))
  observeEvent(input$code_assump, show_code("R code: assumption checks", code_assump_text()))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
