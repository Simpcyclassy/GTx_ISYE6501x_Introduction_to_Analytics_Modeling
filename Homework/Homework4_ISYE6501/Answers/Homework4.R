
# install.packages("smooth"); install.packages("Kendall")
library(tidyverse)
library(lubridate)
library(smooth)   # es()
library(Kendall)

# ---- load & reshape ----
raw <- read_table("temps.txt", col_types = cols(.default = "c"))
years <- setdiff(names(raw), "DAY")

dat <- raw |>
  pivot_longer(all_of(years), names_to = "year", values_to = "temp") |>
  mutate(
    year = as.integer(year),
    temp = as.numeric(temp),
    day  = parse_date_time(paste(DAY, year), orders = "d-b-Y")
  ) |>
  arrange(year, day) |>
  filter(month(day) %in% 7:10)

# ---- helper (pass year explicitly) ----
end_of_summer_one_year <- function(df_year, yr, delta = 2, persist_k = 5, window_n = 7) {
  # July baseline
  july_base <- median(df_year$temp[month(df_year$day) == 7], na.rm = TRUE)
  if (is.na(july_base)) return(NA)
  
  # Simple ES with smooth::es  (use ANN, not "A")
  fit <- es(df_year$temp, model = "ANN", silent = TRUE)   # level-only, alpha chosen by SSE
  S   <- as.numeric(fit$fitted)
  
  # Start search at Aug 1
  idx_start <- which(df_year$day >= as.Date(sprintf("%d-08-01", yr)))[1]
  if (is.na(idx_start)) return(NA)
  
  # Persistence rule
  below <- S < (july_base - delta)
  n <- length(S)
  for (t in idx_start:(n - window_n + 1)) {
    if (sum(below[t:(t + window_n - 1)], na.rm = TRUE) >= persist_k) {
      return(df_year$day[t])
    }
  }
  return(NA)
}

# ---- compute end-of-summer per year (use .y$year) ----
ends <- dat |>
  group_by(year) |>
  group_modify(~ tibble(end_date = end_of_summer_one_year(.x, .y$year))) |>
  ungroup() |>
  mutate(doy = yday(end_date))

print(ends)

# ---- trend tests (will work once there are non-NA dates) ----
trend_df <- ends |> filter(!is.na(doy))

if (nrow(trend_df) > 1) {
  lm_fit <- lm(doy ~ year, data = trend_df)
  print(summary(lm_fit))
  
  kend <- Kendall(trend_df$year, trend_df$doy)
  print(kend)
  
  # Plot
  ggplot(trend_df, aes(year, doy)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Unofficial End-of-Summer Date vs Year",
         y = "Day of Year", x = "Year")
} else {
  message("Not enough non-NA end dates yet to run trend tests.")
}

