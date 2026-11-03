#app.R
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(shiny)
library(shinyTime)

#setwd("../Desktop/Paike/")
#Sys.setenv(TZ = "EET")
# paike <- fread("paike_2018.csv", encoding = "UTF-8") %>%
#   mutate(
#     Kuupaev = as.POSIXct(Kuupaev, format = "%d.%m.%Y"),
#     tava1 = as_datetime(as.POSIXct(format(Tous, format = "%H:%M"), format = "%H:%M"), tz = "EET"),
#     tava2 = as_datetime(as.POSIXct(format(Langus, format = "%H:%M"), format = "%H:%M"), tz = "EET"),
#     suvi1 = as_datetime(ifelse(Kuupaev < as.Date("2018-03-25") | Kuupaev > as.Date("2018-10-27"), tava1 + 3600, tava1), tz = "EET"),
#     talv1 = as_datetime(ifelse(Kuupaev >= as.Date("2018-03-25") & Kuupaev <= as.Date("2018-10-27"), tava1 - 3600, tava1), tz = "EET"),
#     suvi2 = as_datetime(ifelse(Kuupaev < as.Date("2018-03-25") | Kuupaev > as.Date("2018-10-27"), tava2 + 3600, tava2), tz = "EET"),
#     talv2 = as_datetime(ifelse(Kuupaev >= as.Date("2018-03-25") & Kuupaev <= as.Date("2018-10-27"), tava2 - 3600, tava2), tz = "EET")
#   )
# save(paike, file = "paike.RData")

load("paike.RData")


ui <- fluidPage(
  column(width = 4, selectInput("linn", "Lähim linn:", unique(paike$Asukoht), selected = "Tallinn")),
  column(width = 4, timeInput("aratus", "Ärkamisaeg:", seconds = FALSE, value = strptime("7:00:00", "%T"))),
  column(width = 4, timeInput("uni", "Magamaminek:", seconds = FALSE, value = strptime("23:00:00", "%T"))),
  column(width = 12, textOutput("tulemus")),
  tabsetPanel(type = "tabs",
              tabPanel("Kellakeeramine",
                       plotOutput("praegune", height = 500)),
              tabPanel("Suveaeg",
                       #textOutput("tulemus2"),
                       plotOutput("suvine", height = 500)),
              tabPanel("Talveaeg", 
                       #textOutput("tulemus3"),
                       plotOutput("talvine", height = 500))
  )
)


# Define the server code
server <- function(input, output) {
  aratus <- reactive({as_datetime(as.POSIXct(format(input$aratus, format = "%H:%M"), format = "%H:%M"), tz = "EET")})
  uni <- reactive({as_datetime(as.POSIXct(format(input$uni, format = "%H:%M"), format = "%H:%M"), tz = "EET")})
  algus <- as_datetime(as.POSIXct(format("00:00", format = "%H:%M"), format = "%H:%M"), tz = "EET")
  lopp <- as_datetime(as.POSIXct(format("23:59", format = "%H:%M"), format = "%H:%M"), tz = "EET")
  
  andmed <- reactive({
    paike %>% filter(Asukoht == input$linn)
  })
  
  tunnid <- reactive({
    test <- andmed() %>% 
      mutate(algus = aratus(),
             lopp = uni()) 
    if (aratus() < uni()) {
      test <- test %>% 
        mutate(minutid_tava = pmax(0, difftime(pmin(tava2, lopp), pmax(tava1, algus), units = "mins")),
               minutid_suvi = pmax(0, difftime(pmin(suvi2, lopp), pmax(suvi1, algus), units = "mins")),
               minutid_talv = pmax(0, difftime(pmin(talv2, lopp), pmax(talv1, algus), units = "mins")))
    } else {
      test$minutid_tava <- ifelse(test$lopp < test$tava1,  pmax(0, difftime(test$tava2, pmax(test$tava1, test$algus), units = "mins")), 
                                  pmax(0, difftime(test$lopp, test$tava1, units = "mins")) + pmax(0,  difftime(test$tava2, test$algus, units = "mins")))
      test$minutid_suvi <- ifelse(test$lopp < test$suvi1, pmax(0, difftime(test$suvi2, pmax(test$suvi1, test$algus), units = "mins")), 
                                  pmax(0, difftime(test$lopp, test$suvi1, units = "mins")) + pmax(0,  difftime(test$suvi2, test$algus, units = "mins")))
      test$minutid_talv <- ifelse(test$lopp < test$talv1,  pmax(0,difftime(test$talv2, pmax(test$talv1, test$algus), units = "mins")), 
                                  pmax(0, difftime(test$lopp, test$talv1, units = "mins")) + pmax(0,  difftime(test$talv2, test$algus, units = "mins")))
      
    }
    return(c(round(sum(test$minutid_tava)/60), round(sum(test$minutid_suvi)/60), round(sum(test$minutid_talv)/60)))
  })
  
  output$tulemus <- renderText({
    if (tunnid()[1] < tunnid()[2]) {
      if (tunnid()[1] < tunnid()[3]) {
        paste0("Praegu on sul aastast umbes ", tunnid()[1], " tundi valget aega. Kui Eesti jääks pidevale suveajale, sul oleks ", tunnid()[2] - tunnid()[1],
               " tundi rohkem valget aega. Kui Eesti jääks pidevale talveajale, sul oleks ", tunnid()[3] - tunnid()[1], " tundi rohkem valget aega.")
      } else {
        paste0("Praegu on sul aastast umbes ", tunnid()[1], " tundi valget aega. Kui Eesti jääks pidevale suveajale, sul oleks ", tunnid()[2] - tunnid()[1],
               " tundi rohkem valget aega. Kui Eesti jääks pidevale talveajale, sul oleks ", tunnid()[1] - tunnid()[3], " tundi vähem valget aega.")
      }
    } else {
      if (tunnid()[1] < tunnid()[3]) {
        paste0("Praegu on sul aastast umbes ", tunnid()[1], " tundi valget aega. Kui Eesti jääks pidevale suveajale, sul oleks ", tunnid()[1] - tunnid()[2],
               " tundi vähem valget aega. Kui Eesti jääks pidevale talveajale,  sul oleks ", tunnid()[3] - tunnid()[1], " tundi rohkem valget aega.")
      }
      else {
        paste0("Praegu on sul aastast umbes ", tunnid()[1], " tundi valget aega. Kui Eesti jääks pidevale suveajale, sul oleks ", tunnid()[1] - tunnid()[2],
               " tundi vähem valget aega. Kui Eesti jääks pidevale talveajale, sul oleks ", tunnid()[1] - tunnid()[3], " tundi vähem valget aega.")
      }
    }
  })
  
  output$praegune <- renderPlot({
    str(andmed())
    p <- ggplot(andmed(), aes(x = Kuupaev)) + 
      geom_line(aes(y = as.POSIXct(tava1))) + 
      geom_line(aes(y = as.POSIXct(tava2))) + 
      geom_ribbon(aes(ymin = tava1, ymax = tava2), fill = "yellow", alpha = 0.5) +
      geom_ribbon(aes(ymin = tava2, ymax = lopp), fill = "black", alpha = 0.3) +
      geom_ribbon(aes(ymin = algus, ymax = tava1), fill = "black", alpha = 0.3) +
      theme_minimal(base_size = 15) +
      scale_x_datetime("", date_labels = "%b", limits = c(as.POSIXct("2018-01-01"), as.POSIXct("2018-12-31"))) + 
      scale_y_datetime("", date_labels = "%H:%M", limits = c(algus, lopp)) +
      geom_line(aes(y = aratus()), color = "red") + 
      geom_line(aes(y = uni()), color = "red") +
      theme(plot.background = element_rect(fill = "transparent",colour = NA)) + 
      coord_fixed(ratio = 300)
    
    if (uni() < aratus()) {
      p + geom_ribbon(aes(ymin = uni(), ymax = aratus()), fill = "black", alpha = 0.7) 
    } else {
      p +  geom_ribbon(aes(ymin = algus, ymax = aratus()), fill = "black", alpha = 0.7) +
        geom_ribbon(aes(ymin = uni(), ymax = lopp), fill = "black", alpha = 0.7) 
    }
  })
  output$suvine <- renderPlot({
    p <- ggplot(andmed(), aes(x = Kuupaev)) + 
      geom_line(aes(y = suvi1)) + 
      geom_line(aes(y = suvi2)) + 
      geom_ribbon(aes(ymin = suvi1, ymax = suvi2), fill = "yellow", alpha = 0.5) +
      geom_ribbon(aes(ymin = suvi2, ymax = lopp), fill = "black", alpha = 0.3) +
      geom_ribbon(aes(ymin = algus, ymax = suvi1), fill = "black", alpha = 0.3) +
      theme_minimal(base_size = 15) +
      scale_x_datetime("", date_labels = "%b", limits = c(as.POSIXct("2018-01-01"), as.POSIXct("2018-12-31"))) + 
      scale_y_datetime("", date_labels = "%H:%M", limits = c(algus, lopp)) +
      geom_line(aes(y = aratus()), color = "red") + 
      geom_line(aes(y = uni()), color = "red") +
      theme(plot.background = element_rect(fill = "transparent",colour = NA)) + 
      coord_fixed(ratio = 300)
    
    if (uni() < aratus()) {
      p + geom_ribbon(aes(ymin = uni(), ymax = aratus()), fill = "black", alpha = 0.7) 
    } else {
      p +  geom_ribbon(aes(ymin = algus, ymax = aratus()), fill = "black", alpha = 0.7) +
        geom_ribbon(aes(ymin = uni(), ymax = lopp), fill = "black", alpha = 0.7) 
    }
  })
  output$talvine <- renderPlot({
    p <- ggplot(andmed(), aes(x = Kuupaev)) + 
      geom_line(aes(y = talv1)) + 
      geom_line(aes(y = talv2)) + 
      geom_ribbon(aes(ymin = talv1, ymax = talv2), fill = "yellow", alpha = 0.5) +
      geom_ribbon(aes(ymin = talv2, ymax = lopp), fill = "black", alpha = 0.3) +
      geom_ribbon(aes(ymin = algus, ymax = talv1), fill = "black", alpha = 0.3) +
      theme_minimal(base_size = 15) +
      scale_x_datetime("", date_labels = "%b", limits = c(as.POSIXct("2018-01-01"), as.POSIXct("2018-12-31"))) + 
      scale_y_datetime("", date_labels = "%H:%M", limits = c(algus, lopp)) +
      geom_line(aes(y = aratus()), color = "red") + 
      geom_line(aes(y = uni()), color = "red") +
      theme(plot.background = element_rect(fill = "transparent",colour = NA)) + 
      coord_fixed(ratio = 300)
    
    if (uni() < aratus()) {
      p + geom_ribbon(aes(ymin = uni(), ymax = aratus()), fill = "black", alpha = 0.7) 
    } else {
      p +  geom_ribbon(aes(ymin = algus, ymax = aratus()), fill = "black", alpha = 0.7) +
        geom_ribbon(aes(ymin = uni(), ymax = lopp), fill = "black", alpha = 0.7) 
    }
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)