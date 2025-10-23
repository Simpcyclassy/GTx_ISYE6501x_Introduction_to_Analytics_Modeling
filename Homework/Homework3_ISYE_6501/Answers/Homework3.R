# Outlier testing for the Crime column in uscrime.txt using Grubbs' test
# Requirements: outliers package (install once with install.packages("outliers"))

library(outliers)

# -----------------------------
# 1) Load data and select target
# -----------------------------
file_path <- "uscrime.txt"                  # local file
us <- read.table(file_path, header = TRUE)
crime <- us[[ncol(us)]]                     # last column = number of crimes per 100,000
col_target <- names(us)[ncol(us)]
n <- length(crime)
alpha <- 0.05

# --------------------------------------------
# 2) Distribution check (assumption of Grubbs)
# --------------------------------------------
sw <- shapiro.test(crime)                   # Grubbs assumes approximate normality


# -----------------------------------
# 3) Grubbs' tests (single outlier)
# -----------------------------------
# Two-sided test (tests whether either the min or max is an outlier)
g_two <- grubbs.test(crime)

# One-sided tests to identify which tail (if any) is implicated
g_max <- grubbs.test(crime, two.sided = FALSE)                   # tests the maximum
g_min <- grubbs.test(crime, two.sided = FALSE, opposite = TRUE)  # tests the minimum

# Extremes and their indices
i_max <- which.max(crime); v_max <- crime[i_max]
i_min <- which.min(crime); v_min <- crime[i_min]

# -----------------------------
# 4) Report primary conclusion
# -----------------------------
cat("\n=== Grubbs' Outlier Test on", col_target, "===\n")
cat(sprintf("Sample size (n): %d\n", n))
cat(sprintf("Shapiro–Wilk normality p-value: %.5f\n", sw$p.value))
cat(sprintf("Two-sided Grubbs:   G = %.4f, p = %.5f\n", g_two$statistic, g_two$p.value))
cat(sprintf("One-sided (max):    G = %.4f, p = %.5f (max=%g at row %d)\n",
            g_max$statistic, g_max$p.value, v_max, i_max))
cat(sprintf("One-sided (min):    G = %.4f, p = %.5f (min=%g at row %d)\n",
            g_min$statistic, g_min$p.value, v_min, i_min))

if (g_two$p.value < alpha) {
  which_tail <- ifelse(g_max$p.value < alpha, "maximum", "minimum")
  cat(sprintf("\nConclusion (alpha=%.2f): Evidence of a single outlier (%s).\n",
              alpha, which_tail))
} else {
  cat(sprintf("\nConclusion (alpha=%.2f): No evidence of a single outlier.\n", alpha))
}

# -----------------------------------------------------------------
# 5) OPTIONAL: iterative Grubbs to probe for multiple outliers
#    (Grubbs is a single-outlier test; this use is conservative.)
# -----------------------------------------------------------------
run_iterative <- FALSE
if (run_iterative) {
  x <- crime; idx <- seq_along(crime); flagged <- integer(0)
  repeat {
    r <- grubbs.test(x)
    if (r$p.value < alpha) {
      i_ext <- which.max(abs(x - mean(x)))
      flagged <- c(flagged, idx[i_ext])
      x   <- x[-i_ext]
      idx <- idx[-i_ext]
    } else break
  }
  if (length(flagged)) {
    cat("\nIterative Grubbs (exploratory): flagged rows/values\n")
    print(data.frame(row = flagged, Crime = us$Crime[flagged]))
  } else {
    cat("\nIterative Grubbs (exploratory): no additional outliers flagged.\n")
  }
}


# Packages
#install.packages("tidyverse")
#install.packages("qcc")
#install.packages("lubridate")


library(tidyverse)
library(qcc)       # for cusum
library(lubridate) # for date handling

# 1. Read the data
temps <- read.table("temps.txt", header = TRUE, sep = "\t", check.names = FALSE)

# 2. Reshape to long format
temps_long <- temps %>%
  pivot_longer(-Date, names_to = "Year", values_to = "Temp") %>%
  mutate(
    Year = as.integer(Year),
    Date = dmy(paste(Date, Year)) # combine day-month with year
  )

# 3. Filter for July–October
summer_data <- temps_long %>%
  filter(month(Date) >= 7 & month(Date) <= 10)

# ---- PART 1: Detect unofficial summer end each year ----
summer_end_points <- summer_data %>%
  group_by(Year) %>%
  group_modify(~ {
    # Get daily temps for this year
    temps_vec <- .x$Temp
    
    # CUSUM: target mean = mean July temp (start of summer)
    target <- mean(temps_vec[month(.x$Date) == 7])
    
    # Apply CUSUM for downward shift detection
    csum <- cusum(temps_vec, decision.interval = 5, se.shift = 1, center = target)
    
    # Find first day where CUSUM signals a drop
    signal_day <- which(csum$decision == 1)[1]
    
    tibble(
      summer_end_date = if (!is.na(signal_day)) .x$Date[signal_day] else NA
    )
  })

print(summer_end_points)

# ---- PART 2: Detect long-term warming trend ----
# We'll take July–August average each year and run CUSUM for upward shift
summer_avg <- summer_data %>%
  filter(month(Date) %in% c(7, 8)) %>%
  group_by(Year) %>%
  summarise(mean_temp = mean(Temp), .groups = "drop")

# Target = mean of first 5 years
target_longterm <- mean(summer_avg$mean_temp[summer_avg$Year <= 2000])

# Apply CUSUM for upward shift detection
csum_longterm <- cusum(summer_avg$mean_temp,
                       decision.interval = 5,
                       se.shift = 0.5,
                       center = target_longterm)

# Plot to visualize
plot(csum_longterm, main = "CUSUM for Summer Avg Temp (July–Aug)")

# Identify year of first sustained upward shift
warming_year <- summer_avg$Year[which(csum_longterm$decision == 1)[1]]
cat("Possible warming shift detected around:", warming_year, "\n")
