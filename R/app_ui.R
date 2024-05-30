#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @import dplyr
#' @noRd

lifestages <- read.csv("./data/lifestages.csv", check.names=FALSE)

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
                       heading_font = font_google("Quattrocento"),
                       base_font = font_google("Karla")),
card(
  card_header(
    fluidRow(column(6, align = 'center',
                    h3("1) DESCRIBE THE PERSON THIS DIETARY ANALYSIS IS FOR:"),
                    virtualSelectInput("lifestage",
                                       label = NULL,
                                       inline = T,
                                       width = "100%",
                                       choices = lifestages$lifestages)),
             column(6, align = 'center',
                    h3("2) HOW MANY INDIVIDUAL FOODS DID THEY EAT?"),
                    numericInputIcon("num_inputs",
                                 icon = icon("carrot"),
                                 label = NULL,
                                 min = 1, step = 1, value = 1))),
    ),
card_body(
  div(style = "color:green;",
      fluidRow(column(12, align = 'center',
                      h3("3) DESCRIBE THE FOODS AND THEIR AMOUNTS AS ACCURATELY AS POSSIBLY")
                      )
               ),
      fluidRow(column(6, align = 'left',
                      p("Food"),
                      htmlOutput("foods")),
               column(3, align = 'center',
                      p("Number of Servings"),
                      htmlOutput("portion_sizes")),
               column(3, align = 'right',
                      p("Serving Size"),
                      htmlOutput("portion_types")))
  ),
  fluidRow(column(12, align = 'center',
                  h3("SEE HOW WELL YOU DID"))),
fluidRow(column(8,
                p("Plot")),
         column(4,
                p("Table"),
                p("Include Cost"),
                p("How many days of nutrients did you get?"))
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
