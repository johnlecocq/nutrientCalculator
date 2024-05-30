#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import purrr
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @noRd

food_choices <- read.csv("./data/sr_legacy/food.csv") %>%
  select(1,3,4) %>%
  arrange(description)
nutrient <- read.csv("./data/sr_legacy/nutrient_new.csv")
food_nutrient <- read.csv("./data/sr_legacy/food_nutrient.csv") %>%
  select(2,3,4)
food_portion <- read.csv("./data/sr_legacy/food_portion.csv") %>%
  select(2,4,7,8)
req_data_raw <- read.csv("./data/rdi_data.csv", check.names=FALSE) %>%
  pivot_longer(cols = 2:last_col(),
               names_to = "name",
               values_to = "amount")

app_server <- function(input, output, session) {

food_names <- reactive(paste0("food", seq_len(input$num_inputs)))
portion_sizes <- reactive(paste0("potrion_size", seq_len(input$num_inputs)))

req_data <- eventReactive(input$lifestage,{
  req_data_raw %>%
    filter(`Life Stage Group` == input$lifestage)
})

output$foods <- renderUI({
  map(food_names(),
      ~virtualSelectInput(
        selected = isolate(input[[.x]]),
        inputId = .x,
        label = NULL,
        choices = food_choices$description,
        search = TRUE,
        width = "100%",
        markSearchResults = TRUE,
        dropboxWrapper = "body"
      ))
  })

portion_types_list <- reactive({
  filtered_food_list <- map(seq_along(food_names()),
                       ~filter(food_choices, description == input[[paste0("food", .x)]]))
  portion_list <- map(seq_along(filtered_food_list),
                      ~filter(food_portion, fdc_id == filtered_food_list[[.x]]$fdc_id) %>%
                        unique()
  )
  portion_list
})

output$portion_types <- renderUI({
  map(seq_along(food_names()),
      ~virtualSelectInput(
        showSelectedOptionsFirst = F,
        inputId = paste0("portion", .x),
        label = NULL,
        choices = portion_types_list()[[.x]]$modifier,
        search = TRUE,
        width = "100%",
        markSearchResults = TRUE,
        dropboxWrapper = "body"
      )
        )
})

output$portion_sizes <- renderUI({
  map(portion_sizes(),
      ~numericInput(inputId = .x,
                    label = NULL,
                    width = "100%",
                    value = isolate(input[[.x]]),
                    min = 0)
        )
})


}
