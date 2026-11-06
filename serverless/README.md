# Browser-ready daylight calculator

This version needs only **shiny**. It uses base R plots and 24-hour HH:MM text
inputs instead of ggplot2 and shinyTime. Numeric minutes replace POSIXct clock
times, so results do not depend on today's date or the visitor's timezone.
City data is grouped once at startup; all three totals use one vectorized
calculation, and only the selected chart is rendered.
Time fields always display values such as `07:00` and `23:00`, regardless of
the browser's locale. Enter two-digit hours and minutes from `00:00` to `23:59`.

The UI remains in Estonian. Charts use the same daylight/sleep shading and red
wake/sleep lines, with a simpler base R appearance. Equal wake and sleep times
mean 24 hours awake, following the original app. Differences are calculated
before rounding (the original rounded each total first).
Overnight overlap is clipped to the sunrise/sunset interval, fixing cases in
the original formula that counted more daylight than was available that day.

`paike.rds` contains the original 4,380 city/date observations, with dates and
integer minutes after midnight for all three scenarios. It replaces the larger
RData workspace and its data.table attributes. The 2018 summer-time interval
(March 25 through October 27) is deliberately retained. This app does not
calculate sunrise or sunset for other years.

## Run locally

From the repository root:

```r
install.packages("shiny")
shiny::runApp("serverless")
```

## Export for static hosting

The folder contains Shiny source, not a prebuilt static website. Export it with
[Shinylive](https://posit-dev.github.io/r-shinylive/), then publish the resulting
`site/` folder to static hosting:

```r
install.packages(c("shinylive", "httpuv"))
shinylive::export("serverless", "site")
httpuv::runStaticServer("site")
```

No hosted R process is needed after exporting. The browser downloads R/Shiny
and runs the app locally. Test the exported app in your target browsers before
publishing. No deployment or GitHub Pages configuration is changed here.

## Rebuild data / run checks

From the repository root:

```r
source("tools/prepare-serverless-data.R", encoding = "UTF-8")
source("tests/test-serverless.R", encoding = "UTF-8")
```

The checks compare all saved scenario times against the original data and
compare eight schedules against a minute-by-minute reference across every
city, day and scenario. They also exercise Shiny's reactive outputs, all three
plot selections, equal times and invalid input. These are local R checks;
Shinylive export and browser interaction still need separate verification.
