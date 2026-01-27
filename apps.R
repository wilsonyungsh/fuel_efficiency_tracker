library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)

# =====================
# Database
# =====================
con <- dbConnect(SQLite(), "data/car_log.sqlite")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS fuel (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  date TEXT,
  odometer REAL,
  litres REAL,
  fuel_type TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS service (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  service_date TEXT,
  odometer REAL,
  item TEXT,
  cost REAL,
  remarks TEXT
);
")

# =====================
# UI
# =====================
ui <- fluidPage(
  titlePanel("🚗 Fuel & Service Dashboard"),

  tabsetPanel(
    tabPanel(
      "Fuel",
      sidebarLayout(
        sidebarPanel(
          h4("⛽ Fuel"),
          dateInput("fuel_date", "Date", Sys.Date()),
          numericInput("fuel_odo", "Odometer", 0),
          numericInput("fuel_litres", "Litres", 0),
          selectInput("fuel_type", "Fuel type",
            c("98", "95", "91", "E10")),
          actionButton("save_fuel", "Save / Update", class = "btn-primary"),
          actionButton("delete_fuel", "Delete", class = "btn-danger")
        ),
        mainPanel(
          h4("📊 Fuel Consumption"),

          radioButtons(
            "fuel_unit",
            NULL,
            choices = c(
              "L / 100km" = "l100",
              "km / L" = "kml"
            ),
            inline = TRUE
          ),

          plotOutput("fuel_plot", height = 300),

          hr(),
          h4("🧾 Latest 5 Fuel Records"),
          DTOutput("fuel_table")
        )
      )
    ),

    tabPanel(
      "Service",
      sidebarLayout(
        sidebarPanel(
          h4("🔧 Service"),
          dateInput("svc_date", "Service date", Sys.Date()),
          numericInput("svc_odo", "Odometer", 0),
          textInput("svc_item", "Item"),
          numericInput("svc_cost", "Cost ($)", 0),
          textAreaInput("svc_remarks", "Remarks", rows = 2),
          actionButton("save_service", "Save / Update", class = "btn-warning"),
          actionButton("delete_service", "Delete", class = "btn-danger")
        ),
        mainPanel(
          h4("🛠 Latest 5 Service Records"),
          DTOutput("service_table")
        )
      )
    )
  )
)

# =====================
# Server
# =====================
server <- function(input, output, session) {

  selected_fuel_id    <- reactiveVal(NULL)
  selected_service_id <- reactiveVal(NULL)

  # ===== triggers =====
  fuel_trigger <- reactiveVal(0)
  service_trigger <- reactiveVal(0)

  # ---------- Fuel ----------
  fuel_data <- reactive({
    fuel_trigger()
    dbReadTable(con, "fuel") %>%
      mutate(date = as.Date(date)) %>%
      arrange(desc(date))
  })

  fuel_calc <- reactive({
    fuel_data() %>%
      arrange(date) %>%
      mutate(
        lag_odo = lag(odometer),
        km = odometer - lag_odo,
        l_per_100 = litres / km * 100,
        km_per_l  = km / litres
      ) %>%
      filter(!is.na(km), km > 0)
  })

  output$fuel_plot <- renderPlot({
    df <- fuel_calc()
    req(nrow(df) > 0)

    if (input$fuel_unit == "l100") {
      ggplot(df, aes(date, l_per_100)) +
        geom_line(color = "#C62828") +
        geom_point(color = "#C62828") +
        geom_smooth() +
        expand_limits(y = 0) +
        labs(y = "L / 100km", x = NULL) +
        theme_minimal(base_size = 14)
    } else {
      ggplot(df, aes(date, km_per_l)) +
        geom_line(color = "#2E7D32") +
        geom_point(color = "#2E7D32") +
        expand_limits(y = 0) +
        geom_smooth() +
        labs(y = "km / L", x = NULL) +
        theme_minimal(base_size = 14)
    }
  })

  output$fuel_table <- renderDT({
    datatable(
      fuel_data() %>% head(5),
      selection = "single",
      options = list(dom = "t", ordering = FALSE)
    )
  })

  observeEvent(input$fuel_table_rows_selected, {
    r <- fuel_data()[input$fuel_table_rows_selected, ]
    selected_fuel_id(r$id)

    updateDateInput(session, "fuel_date", value = r$date)
    updateNumericInput(session, "fuel_odo", value = r$odometer)
    updateNumericInput(session, "fuel_litres", value = r$litres)
    updateSelectInput(session, "fuel_type", selected = r$fuel_type)
  })

  observeEvent(input$save_fuel, {
    if (is.null(selected_fuel_id())) {
      dbExecute(con,
        "INSERT INTO fuel (date, odometer, litres, fuel_type)
         VALUES (?,?,?,?)",
        params = list(
          as.character(input$fuel_date),
          input$fuel_odo,
          input$fuel_litres,
          input$fuel_type
        )
      )
    } else {
      dbExecute(con,
        "UPDATE fuel SET date=?, odometer=?, litres=?, fuel_type=?
         WHERE id=?",
        params = list(
          as.character(input$fuel_date),
          input$fuel_odo,
          input$fuel_litres,
          input$fuel_type,
          selected_fuel_id()
        )
      )
    }
    selected_fuel_id(NULL)
    fuel_trigger(fuel_trigger() + 1)
  })

  observeEvent(input$delete_fuel, {
    req(selected_fuel_id())
    dbExecute(con, "DELETE FROM fuel WHERE id=?",
      params = list(selected_fuel_id()))
    selected_fuel_id(NULL)
    fuel_trigger(fuel_trigger() + 1)
  })

  # ---------- Service ----------
  service_data <- reactive({
    service_trigger()
    dbReadTable(con, "service") %>%
      mutate(service_date = as.Date(service_date)) %>%
      arrange(desc(service_date))
  })

  output$service_table <- renderDT({
    datatable(
      service_data() %>% head(5),
      selection = "single",
      options = list(dom = "t", ordering = FALSE)
    )
  })

  observeEvent(input$service_table_rows_selected, {
    r <- service_data()[input$service_table_rows_selected, ]
    selected_service_id(r$id)

    updateDateInput(session, "svc_date", value = r$service_date)
    updateNumericInput(session, "svc_odo", value = r$odometer)
    updateTextInput(session, "svc_item", value = r$item)
    updateNumericInput(session, "svc_cost", value = r$cost)
    updateTextAreaInput(session, "svc_remarks", value = r$remarks)
  })

  observeEvent(input$save_service, {
    if (is.null(selected_service_id())) {
      dbExecute(con,
        "INSERT INTO service (service_date, odometer, item, cost, remarks)
         VALUES (?,?,?,?,?)",
        params = list(
          as.character(input$svc_date),
          input$svc_odo,
          input$svc_item,
          input$svc_cost,
          input$svc_remarks
        )
      )
    } else {
      dbExecute(con,
        "UPDATE service
         SET service_date=?, odometer=?, item=?, cost=?, remarks=?
         WHERE id=?",
        params = list(
          as.character(input$svc_date),
          input$svc_odo,
          input$svc_item,
          input$svc_cost,
          input$svc_remarks,
          selected_service_id()
        )
      )
    }
    selected_service_id(NULL)
    service_trigger(service_trigger() + 1)
  })

  observeEvent(input$delete_service, {
    req(selected_service_id())
    dbExecute(con, "DELETE FROM service WHERE id=?",
      params = list(selected_service_id()))
    selected_service_id(NULL)
    service_trigger(service_trigger() + 1)
  })
}

shinyApp(ui, server)
