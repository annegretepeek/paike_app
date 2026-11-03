# Run from the repository root. No contributed packages are needed.
source_data <- new.env(parent = baseenv())
load("original/paike.RData", envir = source_data)
original <- source_data$paike

clock_minutes <- function(x) {
  pieces <- strsplit(as.character(x), ":", fixed = TRUE)
  vapply(pieces, function(p) as.integer(p[1]) * 60L + as.integer(p[2]), integer(1))
}

dates <- as.Date(original$Kuupaev, tz = "Europe/Tallinn")
rise <- clock_minutes(original$Tous)
set <- clock_minutes(original$Langus)
summer <- dates >= as.Date("2018-03-25") & dates <= as.Date("2018-10-27")
data <- data.frame(
  date = dates, city = enc2utf8(original$Asukoht),
  current_rise = rise, current_set = set,
  summer_rise = rise + ifelse(summer, 0L, 60L),
  summer_set = set + ifelse(summer, 0L, 60L),
  winter_rise = rise - ifelse(summer, 60L, 0L),
  winter_set = set - ifelse(summer, 60L, 0L)
)
stopifnot(!anyNA(data), all(rise < set), all(data$winter_rise >= 0),
          all(data$summer_set <= 1440), length(unique(data$city)) == 12L)
dir.create("serverless", showWarnings = FALSE)
saveRDS(data, "serverless/paike.rds", compress = "xz", version = 2)
cat(nrow(data), "rows saved to serverless/paike.rds\n")
