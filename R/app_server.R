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
#' @import ggplot2
#' @noRd

### Load data sets
food_choices <- read.csv("./data/sr_legacy/food.csv") %>%
  select(1,3,4) %>%
  arrange(description)
nutrient <- read.csv("./data/sr_legacy/nutrient_2.csv") %>%
  mutate("unit" =  relevel(as.factor(`unit`), "KCAL"))
food_nutrient <- read.csv("./data/sr_legacy/food_nutrient.csv") %>%
  select(2,3,4)
food_portion <- read.csv("./data/sr_legacy/food_portion.csv") %>%
  select(2,4,7,8) %>%
  mutate("serving_size" = paste0(amount, " ", modifier, " (", gram_weight, "g)"))
req_data_raw <- read.csv("./data/rdi_data.csv", check.names=FALSE) %>%
  pivot_longer(cols = 2:last_col(),
               names_to = "name",
               values_to = "amount")

### Start server code
app_server <- function(input, output, session) {

### Create vectors containing names for each food and portion input
food_names <- reactive({
  req(input$num_inputs > 0)
  paste0("food", seq_len(input$num_inputs))
  })
portion_sizes <- reactive({
  req(input$num_inputs > 0)
  paste0("portion_size", seq_len(input$num_inputs))
  })

### Reactively filter the food choices data based on user selections and create list where each entry is a selection
food_choices_list <- reactive({
  req(input[[paste0("food", length(food_names()))]])
  filtered_food_list <- map(seq_along(food_names()),
                            ~filter(food_choices, description == input[[paste0("food", .x)]]))
  filtered_food_list
})
### Reactively filter portions data to create serving size choices based on food inputs
portion_types_list <- reactive({
  # check that every possibly number of food inputs has a value
  req(food_choices_list())
  portion_list <- map(seq_along(food_choices_list()),
                      ~filter(food_portion, fdc_id == food_choices_list()[[.x]]$fdc_id) %>%
                        unique()
  )
  portion_list
})

req_data <- eventReactive(input$lifestage,{
  filter(req_data_raw, lifestage_group == input$lifestage)
})

### Reactively filter food nutrient data by foods selected to obtain nutrient ids and amounts
food_nutrients <- reactive({
  req(portion_types_list())
  walk(seq_along(portion_sizes()), ~req(input[[paste0("portion_size", .x)]]>0))
  # check that every possibly number of serving number inputs has a value
  nutrient_content_list <- map(seq_along(food_choices_list()),
      ~filter(food_nutrient, fdc_id == food_choices_list()[[.x]]$fdc_id) %>%
        filter(amount > 0))
  filtered_portion_types <- map(seq_along(food_choices_list()),
                                ~filter(portion_types_list()[[.x]], serving_size == input[[paste0("portion", .x)]]))
  scaled_nutrient_content <- map(seq_along(food_choices_list()),
   ~full_join(nutrient_content_list[[.x]], filtered_portion_types[[.x]], by = join_by(fdc_id)) %>%
     mutate("amount" = amount.x*gram_weight*input[[paste0("portion_size", .x)]]/100))
  scaled_units_nutrient_content <- map_df(seq_along(food_choices_list()),
           ~left_join(scaled_nutrient_content[[.x]], nutrient, by = join_by(nutrient_id))) %>%
             group_by(name) %>%
             reframe("amount" = sum(amount),
                       "unit" = unit) %>%
             unique() %>%
    full_join(req_data(),by = join_by("name")) %>%
    drop_na()
  scaled_units_nutrient_content
})

### Switch between tabs (check inputs and send error signals)
observeEvent(input$switchtoresults,{
  req(input$num_inputs > 0)
  req(input$lifestage)
  nav_select("selections-results", "results-tab")
})

observeEvent(input$switchtofoods,{
  nav_select("selections-results", "foods-tab")
})


req_data <- eventReactive(input$lifestage,{
  req_data_raw %>%
    filter(`Life Stage Group` == input$lifestage)
})

### Render Outputs

output$foods <- renderUI({
  map(food_names(),
      ~virtualSelectInput(
        optionsCount = 10,
        showValueAsTags = T,
        selected = isolate(input[[.x]]),
        inputId = .x,
        label = NULL,
        choices = food_choices$description,
        search = TRUE,
        width = "650px",
        markSearchResults = TRUE,
        dropboxWrapper = "body"
      ))
})

output$portion_types <- renderUI({
  map(seq_along(food_names()),
      ~virtualSelectInput(
        optionsCount = 5,
        inline = T,
        showSelectedOptionsFirst = T,
        inputId = paste0("portion", .x),
        label = input[[paste0("food", .x)]],
        choices = portion_types_list()[[.x]]$serving_size,
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

output$testF <- renderTable({
  food_nutrients()
})

output$testT <- renderPlot({
  ggplot(food_nutrients(), aes(name, amount.x))+
    geom_text(aes(y = 5*amount.y+7*amount.x, label = round(100*amount.x/amount.y)), vjust = 0.5, hjust=0.5, colour = "black", size = 12 / .pt,
              check_overlap = T)+
    geom_col(alpha = 0.4, fill='green', color="black",width = 0.5, position = "dodge")+
    geom_point(aes(name, amount.y),
               shape = 10,
               color="red",
               size = 5,
               stroke = 1.175)+
    facet_wrap(vars(unit),
               scales="free",
               strip.position = "top",
               ncol = 1
               )+
    labs(y=NULL,
         x=NULL)+
    theme(
      text = element_text(family = 'Arial',
                              color = "black",
                              size = 18),
          axis.text.x = element_text(angle = 30,
                                     hjust = 1),
          plot.background = element_rect(fill = 'ghostwhite'),
          panel.background = element_rect(color = "black",
                                          fill = 'ghostwhite'),
          panel.grid = element_line(color = 'darkgrey',
                                    linewidth = 0.33),
          panel.grid.minor = element_line(color = 'lightgrey',
                                          linewidth = 0.4),
          panel.grid.major.x = element_line(color = 'ghostwhite'),
          strip.background = element_blank(),
          strip.text = element_text(size = 20, face = 'bold'),
          strip.placement = "outside")+
    scale_y_log10()
    })

}
