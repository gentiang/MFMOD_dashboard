library(shiny)
library(shinyWidgets)
library(bslib)
library(htmltools)
library(readxl)
library(tidyverse)

## Temporary data
df <- read_xlsx("untitled.xlsx") |> 
  mutate(dateid01 = as.double(str_sub(dateid01, 1,4)))

glimpse(df)

## UI Elements
input_ui <- list(fileInput(inputId = "file_input", label = "Input MFMOD solve (*.xlsx)", accept = ".xlsx", width = "100%"),
              numericInput(inputId = "history_end", label = "History end", value = "", step = 1, width = "100%"),
              p(),
              actionButton(inputId = "import", label = "Import", width = "100%"))
gdp_ui <- radioButtons(inputId = "gdp_type", label = "Market or factor prices", choices = c("MKTP", "FCT"), selected = "MKTP", inline = T)
nia_vol_ui <- list(
  sliderInput(inputId = "nia_vol_years", label = NULL, min = 1980, max = 2050, value = c(2000, 2030), step = 1, sep = "", ticks = T, width = "100%"),
  awesomeRadio(inputId = "nia_radio", label = "Buttons", choices = c("Choice 1", "Choice 2", "Choice 3"), selected = "Choice 2", inline = T, width = "100%"),
  awesomeCheckboxGroup(inputId = "nia_checkbox", label = "Items", choices = c("Item 1", "Item 2", "Item 3", "Item 4"), selected = c("Item 1", "Item 3"), inline = T, width = "100%")
)

## Sidebars
input_sidebar <- sidebar(title = "Data Input", input_ui)
nia_vol_sidebar <- sidebar(title = "Filters", nia_vol_ui)

## Cards
input_content <- list(p("Please import an MFMOD solution that has been exported to Excel (*.xlsx). Input the year when history ends, and press", tags$em("Import."), "Then move on to the next tabs."),
                      card(card_header("Dataset"), card_body(DT::DTOutput("dataset"))))
summary_cards <- list(navset_card_underline(title = "Real GDP Growth", nav_panel("Market Prices", "MKTP plot"), nav_panel("Factor Prices", "FCT plot")),
                      card(card_header("Inflation"), card_body("Text")),
                      card(card_header("Fiscal"), card_body("Text")),
                      card(card_header("Balance of Payments"), card_body("Text")),
                      card(card_header("Emissions"), card_body("Text")))
info_content <- list(p("This navigation tab will contain general information about the app."),
                     p("Gentian Gashi, 2014"))

ui <- page_navbar(
  title = "MFMOD Solve Dashboard",
  nav_panel(title = "Input", layout_sidebar(sidebar = input_sidebar, input_content)),
  nav_panel(title = "Summary", 
            layout_columns(summary_cards[[1]], summary_cards[[2]], summary_cards[[3]], summary_cards[[4]], summary_cards[[5]], col_widths = c(8,4, 4, 4, 4), row_heights = c(1.5,1))),
  nav_menu(title = "Analysis", nav_panel(title = "NIA-VOL", layout_sidebar(sidebar = nia_vol_sidebar, p("In-depth analysis of indicators in NIA-VOL."))),
           nav_panel(title = "NIA-VAL", p("NIA-VAL")),
           nav_panel(title = "NIA-P", p("NIA-P")),
           nav_panel(title = "Fical", p("Fiscal")),
           nav_panel(title = "BoP", p("Balance of Payments"))),
  nav_spacer(),
  nav_panel(title = "Info", info_content),
  fillable_mobile = T
)

server <- function(input, output, session) {
  
  df_reactive <- reactive({
    df
  })
  
  output$dataset <- DT::renderDataTable({
    DT::datatable(df_reactive(),
                  options = list(
                    columnDefs = list(list(visible=FALSE, targets=c(13))),
                    searching = FALSE,
                    ordering = FALSE
                  ),
                  rownames = FALSE)
  })
  
}

shinyApp(ui, server)