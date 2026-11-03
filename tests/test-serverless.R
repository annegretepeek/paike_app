# Run from the repository root; requires only shiny and base R.
app_env <- new.env(parent = globalenv())
invisible(source("serverless/app.R", local = app_env, chdir = TRUE,
                 encoding = "UTF-8"))
data <- readRDS("serverless/paike.rds")
old_env <- new.env(parent = baseenv())
load("original/paike.RData", envir = old_env)
old <- old_env$paike
stopifnot(nrow(data) == 4380L, length(app_env$by_city) == 12L,
          identical(data$city, enc2utf8(old$Asukoht)),
          identical(data$date, as.Date(old$Kuupaev, tz = "Europe/Tallinn")))

# Every scenario must match the clock times in the original saved data.
fields <- c(current_rise = "tava1", current_set = "tava2",
            summer_rise = "suvi1", summer_set = "suvi2",
            winter_rise = "talv1", winter_set = "talv2")
for (field in names(fields)) {
  times <- format(old[[fields[[field]]]], "%H:%M", tz = "EET")
  expected <- vapply(times, app_env$clock_minutes, integer(1), USE.NAMES = FALSE)
  stopifnot(identical(data[[field]], expected))
}

# Independent minute-by-minute reference, including overnight and equal times.
minute <- 0:1439
schedules <- rbind(c(420, 1380), c(1380, 420), c(0, 1439), c(600, 600),
                   c(1380, 1200), c(60, 0), c(900, 901), c(0, 0))
for (city in names(app_env$by_city)) {
  d <- app_env$by_city[[city]]
  for (i in seq_len(nrow(schedules))) {
    wake <- schedules[i, 1]
    bed <- schedules[i, 2]
    awake <- if (wake == bed) rep(TRUE, 1440) else if (wake < bed) {
      minute >= wake & minute < bed
    } else minute >= wake | minute < bed
    actual <- app_env$daylight_minutes(d$rise, d$set, wake, bed)
    expected <- matrix(vapply(seq_along(d$rise), function(j) {
      sum(awake & minute >= d$rise[j] & minute < d$set[j])
    }, integer(1)), nrow = nrow(d$rise))
    stopifnot(all(actual == expected))
  }
}
stopifnot(app_env$clock_minutes("23:59") == 1439L,
          is.na(app_env$clock_minutes("24:00")),
          is.na(app_env$clock_minutes("")))

# Exercise reactive inputs, scenario changes, overnight schedules and validation.
shiny::testServer(app_env$server, {
  session$setInputs(linn = "Tallinn", scenario = "current")
  default <- totals()
  stopifnot(length(default) == 3L, all(is.finite(default)))
  stopifnot(nzchar(output$tulemus$html), length(output$daylight$src) == 1L)
  session$setInputs(aratus = "23:00", uni = "07:00", scenario = "winter")
  d <- city_data()
  stopifnot(all(totals() == colSums(app_env$daylight_minutes(
    d$rise, d$set, 1380, 420)) / 60), length(output$daylight$src) == 1L)
  session$setInputs(linn = "Tartu", scenario = "summer")
  stopifnot(length(output$daylight$src) == 1L)
  session$setInputs(aratus = "07:00", uni = "07:00")
  stopifnot(all(totals() == colSums(city_data()$set - city_data()$rise) / 60))
  session$setInputs(aratus = "")
  error <- tryCatch({ schedule(); NULL }, error = identity)
  stopifnot(inherits(error, "shiny.silent.error"))
})
cat("Passed: all 4,380 rows match the saved scenarios; eight schedules across\n",
    "12 cities and three scenarios match minute-level reference calculations;\n",
    "reactive outputs, all chart tabs and input validation work.\n", sep = "")
