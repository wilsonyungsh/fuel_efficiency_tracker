library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# ===== DB =====
dir.create("data", showWarnings = FALSE)
con <- dbConnect(SQLite(), "data/fuel.sqlite")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS fuel (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  date TEXT,
  odometer REAL,
  litres REAL
);
")

# ===== UI =====
ui <- fluidPage(
  titlePanel("🚗 Fuel Consumption Tracker"),

  sidebarLayout(
    sidebarPanel(
      dateInput("date", "加油日期", value = Sys.Date()),
      numericInput("odo", "里程數 (km)", 0, min = 0),
      numericInput("litres", "加油量 (L)", 0, min = 0, step = 0.1),
      actionButton("save", "新增紀錄", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("fuel_plot"),
      tableOutput("summary"),
      tableOutput("table")
    )
  )
)

# ===== Server =====
server <- function(input, output, session) {

  fuel <- reactive({
    dbReadTable(con, "fuel") %>%
      mutate(date = as.Date(date)) %>%
      arrange(date)
  })

  observeEvent(input$save, {
    dbExecute(con,
      "INSERT INTO fuel (date, odometer, litres) VALUES (?,?,?)",
      params = list(
        as.character(input$date),
        input$odo,
        input$litres
      )
    )
  })

  output$summary <- renderTable({
    df <- fuel()

    df %>%
      mutate(
        distance = odometer - lag(odometer),
        l_per_100km = litres / distance * 100
      ) %>%
      summarise(
        平均油耗 = round(mean(l_per_100km, na.rm = TRUE), 2)
      )
  })

  output$fuel_plot <- renderPlot({
    df <- fuel() %>%
      mutate(
        distance = odometer - lag(odometer),
        l_per_100km = litres / distance * 100
      )

    ggplot(df, aes(date, l_per_100km)) +
      geom_line() +
      geom_point() +
      labs(y = "L / 100km", x = NULL) +
      theme_minimal()
  })

  output$table <- renderTable({
    fuel()
  })
}

shinyApp(ui, server)
