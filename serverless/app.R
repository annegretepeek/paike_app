# Shiny for R and Shinylive: only the shiny package is required.
library(shiny)

paike <- readRDS("paike.rds")
scenarios <- c("current", "summer", "winter")
cities <- unique(paike$city)

# Parse and group data once, rather than inside each session or reactive.
by_city <- lapply(split(paike, paike$city), function(d) {
  d <- d[order(d$date), ]
  list(
    date = d$date,
    rise = as.matrix(d[paste0(scenarios, "_rise")]),
    set = as.matrix(d[paste0(scenarios, "_set")])
  )
})
rm(paike)

clock_minutes <- function(value) {
  if (length(value) != 1L || is.na(value) ||
      !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", value)) return(NA_integer_)
  as.integer(substr(value, 1, 2)) * 60L + as.integer(substr(value, 4, 5))
}

daylight_minutes <- function(rise, set, wake, bed) {
  overlap <- function(start, end) {
    pmax(pmin(set, end) - pmax(rise, start), 0)
  }
  # Preserve the original convention: equal times mean 24 hours awake.
  if (wake == bed) return(set - rise)
  if (wake < bed) return(overlap(wake, bed))
  overlap(wake, 1440) + overlap(0, bed)
}

time_input <- function(id, label, value) {
  # Native type="time" may display AM/PM according to the browser locale.
  # Shiny's text binding preserves the explicit 24-hour HH:MM display.
  textInput(id, paste0(label, " (HH:MM, 24 h)"), value = value,
            placeholder = "HH:MM")
}

draw_daylight <- function(d, scenario, wake, bed) {
  x <- as.numeric(d$date)
  rise <- d$rise[, scenario]
  set <- d$set[, scenario]
  par(mar = c(3, 4, 1, 1), bg = "transparent", las = 1)
  plot(range(x), c(0, 1440), type = "n", xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i", bty = "n")
  rect(min(x), 0, max(x), 1440, col = "grey85", border = NA)
  polygon(c(x, rev(x)), c(rise, rev(set)), col = "#FFF59D", border = NA)
  ticks <- seq(0, 1440, by = 240)
  abline(h = ticks, col = "white", lwd = 0.7)
  months <- seq(as.Date("2018-01-01"), as.Date("2018-12-01"), by = "month")
  abline(v = as.numeric(months), col = "white", lwd = 0.7)
  lines(x, rise)
  lines(x, set)
  # Shade sleeping hours, including schedules that cross midnight.
  if (wake < bed) {
    rect(min(x), 0, max(x), wake, col = "#00000080", border = NA)
    rect(min(x), bed, max(x), 1440, col = "#00000080", border = NA)
  } else if (wake > bed) {
    rect(min(x), bed, max(x), wake, col = "#00000080", border = NA)
  }
  abline(h = c(wake, bed), col = "#D32F2F", lwd = 1.5)
  axis(1, at = as.numeric(months),
       labels = c("jaan", "veebr", "märts", "apr", "mai", "juuni",
                  "juuli", "aug", "sept", "okt", "nov", "dets"),
       tick = FALSE)
  axis(2, at = ticks, labels = sprintf("%02d:00", ticks %/% 60), tick = FALSE)
}

ui <- fluidPage(
  titlePanel(NULL, windowTitle = "Päikse kalkulaator"),
  fluidRow(
    column(4, selectInput("linn", "Lähim linn:", cities, selected = "Tallinn")),
    column(4, time_input("aratus", "Ärkamisaeg:", "07:00")),
    column(4, time_input("uni", "Magamaminek:", "23:00"))
  ),
  tags$div(style = "margin-bottom: 15px;", htmlOutput("tulemus")),
  tabsetPanel(
    id = "scenario", selected = "current",
    tabPanel("Kellakeeramine", value = "current"),
    tabPanel("Suveaeg", value = "summer"),
    tabPanel("Talveaeg", value = "winter")
  ),
  plotOutput("daylight", height = "500px"),
  tags$p("Arvutusteks kasutatakse 2018. aasta andmeid. Valge aeg on aeg ",
         "päikesetõusust päikeseloojanguni. Võrdne ärkamis- ja magamaminekuaeg ",
         "tähendab 24 tundi ärkvelolekut.")
)

server <- function(input, output, session) {
  schedule <- reactive({
    wake <- clock_minutes(if (is.null(input$aratus)) "07:00" else input$aratus)
    bed <- clock_minutes(if (is.null(input$uni)) "23:00" else input$uni)
    validate(need(!is.na(wake) && !is.na(bed),
                  "Sisesta kellaajad 24 tunni kujul HH:MM (00:00–23:59)."))
    list(wake = wake, bed = bed)
  })
  city_data <- reactive({
    req(input$linn, input$linn %in% names(by_city))
    by_city[[input$linn]]
  })
  totals <- reactive({
    d <- city_data()
    s <- schedule()
    colSums(daylight_minutes(d$rise, d$set, s$wake, s$bed)) / 60
  })
  output$tulemus <- renderUI({
    hours <- totals()
    difference <- function(index, label) {
      delta <- hours[index] - hours[1]
      paste0("Kui Eesti jääks pidevale ", label, ", sul oleks <b>",
             round(abs(delta)), " tundi ",
             if (delta >= 0) "rohkem" else "vähem", "</b> valget aega.")
    }
    HTML(paste(
      paste0("Praegu on sul aastast umbes <b>", round(hours[1]),
             " tundi valget aega</b>."),
      difference(2, "suveajale"), difference(3, "talveajale"), sep = "<br/>"
    ))
  })
  # One renderer: only the selected scenario is drawn.
  output$daylight <- renderPlot({
    scenario <- match(input$scenario, scenarios)
    req(length(scenario) == 1L, !is.na(scenario))
    s <- schedule()
    draw_daylight(city_data(), scenario, s$wake, s$bed)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
