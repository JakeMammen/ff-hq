library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(DT)

source("R/scoring.R", local = TRUE)
source("R/valuation.R", local = TRUE)

proj <- read_csv("data/projections.csv", show_col_types = FALSE)

ui <- page_sidebar(
  title = "Custom Fantasy Values",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 320,
    h4("League settings"),
    selectInput("ppr_type", "Scoring preset", c("PPR", "Half PPR", "Standard"), "PPR"),
    sliderInput("teams", "# Teams", min = 8, max = 14, value = 12, step = 1),
    selectInput("qb_type", "QB / Superflex", c("1 QB", "Superflex")),
    numericInput("rb", "RB starters", 2, min = 1, max = 4, step = 1),
    numericInput("wr", "WR starters", 2, min = 1, max = 5, step = 1),
    numericInput("te", "TE starters", 1, min = 0, max = 3, step = 1),
    numericInput("flex", "FLEX starters", 1, min = 0, max = 3, step = 1),
    numericInput("bench", "Bench spots", 5, min = 0, max = 10, step = 1),
    sliderInput("lambda", "ADP blend", min = 0, max = 1, value = 0.35, step = 0.05),
    helpText("0 = projections only. 1 = follow ADP. 0.35 is a good PPR start."),
    hr(),
    selectInput("pos_filter", "Position", c("All", "QB", "RB", "WR", "TE")),
    downloadButton("download", "Download rankings")
  ),
  card(
    full_screen = TRUE,
    card_header(textOutput("league_label")),
    p(
      class = "small text-muted",
      "Value = weekly points over replacement, blended with ADP. ",
      "Replacement uses your roster plus a FLEX and bench pad."
    ),
    DTOutput("board")
  )
)

server <- function(input, output, session) {
  settings <- reactive({
    list(
      teams = input$teams,
      qb = 1,
      superflex = identical(input$qb_type, "Superflex"),
      rb = input$rb,
      wr = input$wr,
      te = input$te,
      flex = input$flex,
      bench = input$bench
    )
  })
  
  ranked <- reactive({
    scoring <- preset_scoring(input$ppr_type)
    rank_players(proj, settings(), scoring, lambda = input$lambda)
  })
  
  shown <- reactive({
    df <- ranked()
    if (input$pos_filter != "All") {
      df <- dplyr::filter(df, pos == input$pos_filter)
    }
    df
  })
  
  output$league_label <- renderText({
    qb <- if (settings()$superflex) "Superflex" else "1 QB"
    sprintf(
      "%s-team %s | %s, %s RB, %s WR, %s TE, %s FLEX | Bench: %s",
      input$teams, input$ppr_type, qb,
      input$rb, input$wr, input$te, input$flex, input$bench
    )
  })
  
  output$board <- renderDT({
    shown() |>
      mutate(
        across(c(adp, floor, value, ceiling, points, porp_week), ~ round(.x, 2))
      ) |>
      datatable(
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 30, scrollX = TRUE),
        colnames = c(
          "Rank", "Pos", "Player", "Team", "ADP",
          "Floor", "Value", "Ceiling", "Pos Rank",
          "Season Pts", "PORP/Wk"
        )
      ) |>
      formatStyle("value", backgroundColor = "#d4edda")
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("rankings-", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(shown(), file)
    }
  )
}

shinyApp(ui, server)