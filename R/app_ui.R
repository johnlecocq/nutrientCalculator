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
      setBackgroundColor(color = "ghostwhite"),
      theme = bs_theme(version = 5,
                       bg = "white",
                       fg = "black",
                       primary = "forestgreen",
                       secondary = "goldenrod",
                       danger = "red",
                       font_scale = 1.25,
                       heading_font = font_google("Quattrocento"),
                       base_font = font_google("Karla")),
      card_header(layout_column_wrap(
        width = 1 / 2,
        h3(style = "color:black;",
           "Who is this analysis for?"),
        virtualSelectInput(
          "lifestage",
          autoSelectFirstOption = T,
          showValueAsTags = T,
          optionsCount = 12,
          label = NULL,
          inline = T,
          width = "75%",
          choices = lifestages$lifestages
        )
      )),
  navset_hidden(id = "selections-results",
    nav_panel_hidden(value = "foods-tab",
      fluidRow(column(12,align='center',
                      numericInputIcon("num_inputs",
                                      size = "lg",
                                      label = NULL,
                                      icon = icon("plus-minus"),
                                      min = 0, value = 1, max = 30)
                      )
               ),

      fluidRow(column(12, align = 'center',
                      h3("Select Foods", icon("arrow-down")),
                      htmlOutput("foods"))),
      fluidRow(column(7, align = 'center',
                      h3("Serving Size")),
               column(4,offset=1, align = 'center',
                      h3("# of Servings"))),
      fluidRow(
               column(10,
                      htmlOutput("portion_types")),
               column(2,
                      htmlOutput("portion_sizes"))),
      fluidRow(column(12,
                      align = 'center',
                      actionBttn(inputId = "switchtoresults",
                                               label = "Calculate Results",
                                               icon = icon("calculator"))))
  ),
  nav_panel_hidden(value = "results-tab",
                   fluidRow(column(12,
                                   align = 'center',
                                   actionBttn(inputId = "switchtofoods",
                                                            label = "Adjust Diet",
                                                            icon = icon("sliders")))),
fluidRow(
         column(12, align='center',
                p("Table"),
                plotOutput("testT", height = 800),
                tableOutput("testF"),
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
