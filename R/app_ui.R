#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fluid(
      theme = bs_theme(version = 5,
                       bg = "white",
                       fg = "black",
                       primary = "royalblue",
                       secondary = "goldenrod",
                       danger = "red",
                       heading_font = font_google("Bebas Neue"),
                       base_font = font_google("Karla")),
card(
  card_header(
    fluidRow(column(12,align='center', h1("LIST WHAT YOU ATE TODAY"))),
fluidRow(
  column(9,
         align="center",
      layout_column_wrap(
        shinyWidgets::virtualSelectInput(
          inputId = "food-1",
          label = "FOOD",
          choices = c("USDA foods list simplified"),
          search = T
        ),
        shinyWidgets::numericInputIcon(
          inputId = "amount-1",
          label = "AMOUNT",
          help_text = "Enter how much you ate (a number greater than zero)",
          icon = icon('weight-scale'),
          value = NULL,
          min=0
        ))
      ),
      column(3,
             align='center',

             fluidRow(column(8,align='left',
                             layout_column_wrap(
        shinyWidgets::virtualSelectInput(
          multiple = F,
          inputId = "unit-1",
          label = "UNITS",
          choices = c("lbs", "oz", "kg", "g", "fl oz"),
          search = T
        ))
        ),
        column(4, align='right',
        shinyWidgets::actionBttn(inputId = "remove-food",
                                 label = NULL,
                                 color = "danger",
                                 size = "sm",
                                 style = "material-circle",
                                 no_outline = T,
                                 icon = icon("remove"))
        ))
      ))
      ),

layout_column_wrap(
  shinyWidgets::actionBttn(inputId = "add-food",
                           label = "Add",
                           color = "primary",
                           style = "unite",
                           icon = icon("add"))
),
card_body(
  fluidRow(column(12, align = 'center',
                  h1("SEE HOW WELL YOU DID"))),
fluidRow(column(8,
                p("Plot")),
         column(4,
                p("Table"))
),
fluidRow(column(4, offset = 4, align = 'center',
                layout_column_wrap(width = 1/5,
                icon("star"),
                icon("star"),icon("star"),
                icon("star"),icon("star")
                ))
)
)
)
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nutrientCalculator"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
