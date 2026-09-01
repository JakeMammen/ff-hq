library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(DT)
library(tidyr)
library(htmltools)

source("R/scoring.R", local = TRUE)
source("R/valuation.R", local = TRUE)
source("R/auction.R", local = TRUE)

norm_player <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\b(jr|sr|ii|iii|iv|v)\\b", "", x)
  trimws(gsub("\\s+", " ", x))
}

proj <- read_csv("data/projections.csv", show_col_types = FALSE) |>
  mutate(
    player_norm = norm_player(player),
    pos = toupper(trimws(pos)),
    team = toupper(trimws(as.character(team)))
  ) |>
  group_by(player_norm, pos) |>
  summarise(
    player = player[which.max(nchar(as.character(player)))],
    team = {
      teams <- team[!is.na(team) & team != ""]
      if (length(teams)) teams[[1]] else NA_character_
    },
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  select(-player_norm)

player_choices <- sort(unique(proj$player))

adp_for_format <- function(stats, ppr_type) {
  col <- switch(ppr_type,
                "PPR" = "adp_ppr",
                "Half PPR" = "adp_half",
                "Standard" = "adp_std",
                "adp"
  )
  out <- stats
  if (col %in% names(out)) out$adp <- out[[col]]
  out
}

player_cell <- function(player, status) {
  taken <- status %in% c("Drafted", "Keeper")
  checked <- if (identical(status, "Drafted")) "checked" else ""
  disabled <- if (identical(status, "Keeper")) "disabled" else ""
  label <- htmlEscape(player)
  if (taken) label <- paste0("<s>", label, "</s>")
  sprintf(
    '<label class="draft-lab"><input type="checkbox" class="draft-box" data-player="%s" %s %s/> %s</label>',
    htmlEscape(player, attribute = TRUE), checked, disabled, label
  )
}

espn_abbr <- function(team) {
  abbr <- toupper(trimws(as.character(team)))
  abbr[is.na(abbr)] <- ""
  abbr <- gsub("[^A-Z]", "", abbr)
  dplyr::recode(
    abbr,
    JAC = "JAX", JAX = "JAX",
    WAS = "WSH", WSH = "WSH",
    LA = "LAR", LAR = "LAR", STL = "LAR",
    OAK = "LV", LV = "LV",
    SD = "LAC", LAC = "LAC",
    ARZ = "ARI", ARI = "ARI",
    GNB = "GB", GB = "GB",
    KAN = "KC", KCC = "KC", KC = "KC",
    NWE = "NE", NE = "NE",
    NOR = "NO", NO = "NO",
    SFO = "SF", SF = "SF",
    TAM = "TB", TB = "TB",
    .default = abbr
  )
}

team_logo_cell <- function(team) {
  abbr <- espn_abbr(team)
  if (!nzchar(abbr) || abbr %in% c("FA", "NA", "NONE", "NULL")) return("")
  url <- sprintf("https://a.espncdn.com/i/teamlogos/nfl/500/%s.png", tolower(abbr))
  sprintf(
    '<img src="%s" alt="%s" title="%s" class="team-logo" onerror="this.style.display=\'none\'"/>',
    url, htmlEscape(abbr), htmlEscape(abbr)
  )
}

draft_js <- JS(
  "table.on('change', 'input.draft-box', function() {",
  "  Shiny.setInputValue('draft_toggle', {",
  "    player: this.getAttribute('data-player'),",
  "    drafted: this.checked,",
  "    nonce: Math.random()",
  "  }, {priority: 'event'});",
  "});"
)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    base_font = font_google("Oswald"),
    heading_font = font_google("Silkscreen")
  ),
  tags$head(
    tags$style(HTML("
      .title-header-banner {
        background: linear-gradient(135deg, #112233 0%, #1f3a60 100%);
        color: #ffffff !important;
        padding: 18px 24px;
        margin: -20px -15px 16px -15px;
        border-bottom: 4px solid #31a354;
      }
      .title-header-banner h2 {
        margin: 0;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        font-size: 28px;
      }
      .title-header-banner .byline {
        margin: 6px 0 0 0;
        font-size: 14px;
        color: #c5d4e8;
        font-family: Oswald, sans-serif;
      }
      .crowd-note { font-size: 12px; color: #475569; margin: 0 0 8px 0; }
      .sidebar-panel-styled {
        background-color: #ffffff;
        border: 1px solid #e2e8f0;
        border-radius: 12px;
        padding: 14px;
      }
      .sidebar-panel-styled .form-group { margin-bottom: 8px; }
      .draft-lab { display: flex; align-items: center; gap: 8px; margin: 0; font-weight: 500; cursor: pointer; }
      .draft-box { width: 16px; height: 16px; accent-color: #31a354; }
      .taken-row td { color: #94a3b8 !important; }
      .team-logo { width: 28px; height: 28px; object-fit: contain; vertical-align: middle; }
      .legend-box {
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 10px;
        padding: 12px 16px;
        margin: 0 0 12px 0;
        font-size: 13px;
        line-height: 1.45;
        color: #334155;
      }
      .compact-row .form-group { margin-bottom: 6px; }
    "))
  ),
  div(
    class = "title-header-banner",
    tags$h2("Fantasy Sports Pack Player HQ"),
    tags$p(class = "byline", "By @FantasySPack")
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      "Rankings",
      br(),
      sidebarLayout(
        sidebarPanel(
          class = "sidebar-panel-styled",
          width = 3,
          p(class = "crowd-note", "Wisdom-of-the-crowd projections using {ffanalytics}"),
          tabsetPanel(
            id = "side_tabs",
            type = "pills",
            tabPanel(
              "League",
              br(),
              selectInput("league_format", "Format", c("Redraft" = "redraft", "Dynasty auction" = "auction"), "redraft"),
              sliderInput("teams", "# Teams", min = 8, max = 14, value = 12, step = 1),
              selectInput("qb_type", "QB / Superflex", c("1 QB", "Superflex")),
              fluidRow(class = "compact-row",
                       column(6, numericInput("rb", "RB", 2, min = 1, max = 4, step = 1)),
                       column(6, numericInput("wr", "WR", 2, min = 1, max = 5, step = 1))
              ),
              fluidRow(class = "compact-row",
                       column(6, numericInput("te", "TE", 1, min = 0, max = 3, step = 1)),
                       column(6, numericInput("flex", "FLEX", 1, min = 0, max = 3, step = 1))
              ),
              numericInput("bench", "Bench", 5, min = 0, max = 10, step = 1),
              sliderInput("lambda", "ADP blend", min = 0, max = 1, value = 0.35, step = 0.05),
              conditionalPanel(
                condition = "input.league_format == 'auction'",
                numericInput("budget", "Cap $", 200, min = 50, max = 1000, step = 5),
                checkboxInput("keepers_on", "Use keepers", TRUE),
                conditionalPanel(
                  condition = "input.keepers_on == true",
                  selectizeInput("keeper_player", "Keeper", choices = player_choices, selected = NULL),
                  numericInput("keeper_salary", "Salary", 20, min = 1, max = 200, step = 1),
                  actionButton("add_keeper", "Add", class = "btn-success btn-sm"),
                  actionButton("remove_keeper", "Remove", class = "btn-sm")
                )
              )
            ),
            tabPanel(
              "Scoring",
              br(),
              selectInput("ppr_type", "Preset", c("PPR", "Half PPR", "Standard", "Custom"), "PPR"),
              fluidRow(class = "compact-row",
                       column(6, numericInput("pass_yd", "Pass yd", 0.04, step = 0.01)),
                       column(6, numericInput("pass_td", "Pass TD", 4, step = 0.5))
              ),
              fluidRow(class = "compact-row",
                       column(6, numericInput("pass_int", "INT", -2, step = 0.5)),
                       column(6, numericInput("fumble", "Fum", -2, step = 0.5))
              ),
              fluidRow(class = "compact-row",
                       column(6, numericInput("rush_yd", "Rush yd", 0.1, step = 0.01)),
                       column(6, numericInput("rush_td", "Rush TD", 6, step = 0.5))
              ),
              fluidRow(class = "compact-row",
                       column(6, numericInput("rec", "Rec", 1, step = 0.1)),
                       column(6, numericInput("rec_yd", "Rec yd", 0.1, step = 0.01))
              ),
              numericInput("rec_td", "Rec TD", 6, step = 0.5)
            ),
            tabPanel(
              "Board",
              br(),
              selectInput("pos_filter", "Position", c("All", "QB", "RB", "WR", "TE")),
              checkboxInput("hide_taken", "Hide drafted / keepers", FALSE),
              actionButton("clear_drafted", "Clear drafted", class = "btn-sm"),
              br(), br(),
              downloadButton("download", "Download rankings"),
              br(), br(),
              downloadButton("download_snapshot", "Download league snapshot")
            )
          )
        ),
        mainPanel(
          width = 9,
          h4(textOutput("league_label")),
          div(
            class = "legend-box",
            HTML(paste(
              "<strong>Value</strong> = weekly points over replacement, blended with format-specific FFC ADP.",
              "Replacement uses your roster (starters + FLEX share + a bench pad).",
              "<strong>Floor / Ceiling</strong> = source disagreement around that projection.",
              "In <strong>Dynasty auction</strong>, those three columns become dollars: $1 per remaining roster spot, leftover cap split by surplus.",
              "<strong>ADP</strong> switches with PPR / Half / Standard.",
              "Check a name to mark drafted."
            ))
          ),
          conditionalPanel(
            condition = "input.league_format == 'auction' && input.keepers_on == true",
            DTOutput("keeper_table"),
            br()
          ),
          DTOutput("board")
        )
      )
    ),
    tabPanel(
      "League",
      br(),
      fluidRow(
        column(
          6,
          div(
            class = "sidebar-panel-styled",
            h4("League snapshot"),
            tableOutput("league_snapshot"),
            downloadButton("download_snapshot2", "Download league snapshot"),
            conditionalPanel(
              condition = "input.league_format == 'auction'",
              h4("Auction cap"),
              tableOutput("auction_snapshot")
            )
          )
        ),
        column(
          6,
          div(
            class = "sidebar-panel-styled",
            h4("How values are built"),
            tags$ul(
              tags$li("Crowd stats: ESPN, CBS, FantasyPros."),
              tags$li("ADP: Fantasy Football Calculator lists for PPR, Half PPR, and Standard."),
              tags$li("Points use the Scoring tab multipliers."),
              tags$li("Value is weekly PORP, then optionally blended with ADP."),
              tags$li("Auction $ = $1 floor + share of leftover cap.")
            )
          )
        )
      ),
      br(),
      fluidRow(
        column(6, h4("Keepers"), DTOutput("league_keepers")),
        column(6, h4("Drafted"), DTOutput("league_drafted"))
      )
    )
  )
)

server <- function(input, output, session) {
  keepers <- reactiveVal(tibble(player = character(), salary = numeric()))
  drafted <- reactiveVal(character(0))
  is_auction <- reactive(identical(input$league_format, "auction"))
  use_keepers <- reactive(is_auction() && isTRUE(input$keepers_on))
  
  observeEvent(input$ppr_type, {
    if (identical(input$ppr_type, "Custom")) return()
    rec <- switch(input$ppr_type, "PPR" = 1, "Half PPR" = 0.5, "Standard" = 0, 1)
    updateNumericInput(session, "pass_yd", value = 0.04)
    updateNumericInput(session, "pass_td", value = 4)
    updateNumericInput(session, "pass_int", value = -2)
    updateNumericInput(session, "rush_yd", value = 0.1)
    updateNumericInput(session, "rush_td", value = 6)
    updateNumericInput(session, "rec", value = rec)
    updateNumericInput(session, "rec_yd", value = 0.1)
    updateNumericInput(session, "rec_td", value = 6)
    updateNumericInput(session, "fumble", value = -2)
  })
  
  observeEvent(input$rec, {
    preset_rec <- switch(input$ppr_type, "PPR" = 1, "Half PPR" = 0.5, "Standard" = 0, NA_real_)
    if (!is.na(preset_rec) && !isTRUE(all.equal(input$rec, preset_rec))) {
      updateSelectInput(session, "ppr_type", selected = "Custom")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_keeper, {
    req(use_keepers(), input$keeper_player)
    cur <- keepers()
    if (input$keeper_player %in% cur$player) {
      cur$salary[cur$player == input$keeper_player] <- input$keeper_salary
    } else {
      cur <- bind_rows(cur, tibble(player = input$keeper_player, salary = input$keeper_salary))
    }
    keepers(cur)
    drafted(setdiff(drafted(), input$keeper_player))
  })
  
  observeEvent(input$remove_keeper, {
    rows <- input$keeper_table_rows_selected
    if (length(rows)) keepers(keepers()[-rows, , drop = FALSE])
  })
  
  observeEvent(input$draft_toggle, {
    req(input$draft_toggle$player)
    ply <- input$draft_toggle$player
    if (isTRUE(input$draft_toggle$drafted)) drafted(unique(c(drafted(), ply)))
    else drafted(setdiff(drafted(), ply))
  })
  
  observeEvent(input$clear_drafted, drafted(character(0)))
  
  settings <- reactive({
    list(
      teams = input$teams, qb = 1,
      superflex = identical(input$qb_type, "Superflex"),
      rb = input$rb, wr = input$wr, te = input$te,
      flex = input$flex, bench = input$bench,
      budget = if (is_auction()) input$budget else NA_real_
    )
  })
  
  scoring <- reactive({
    list(
      pass_yd = input$pass_yd, pass_td = input$pass_td, pass_int = input$pass_int,
      rush_yd = input$rush_yd, rush_td = input$rush_td,
      rec = input$rec, rec_yd = input$rec_yd, rec_td = input$rec_td,
      fumble = input$fumble
    )
  })
  
  active_keepers <- reactive({
    if (use_keepers()) keepers() else tibble(player = character(), salary = numeric())
  })
  
  taken_players <- reactive(unique(c(active_keepers()$player, drafted())))
  
  ranked <- reactive({
    board <- rank_players(adp_for_format(proj, input$ppr_type), settings(), scoring(), lambda = input$lambda)
    kept <- active_keepers()
    available <- board |> filter(!(player %in% taken_players()))
    if (is_auction()) {
      available <- auction_dollars(available, settings(), sum(kept$salary, na.rm = TRUE), nrow(kept))
    } else {
      available <- mutate(available, dollars = NA_real_, floor_dollars = NA_real_, ceiling_dollars = NA_real_)
    }
    taken <- board |>
      filter(player %in% taken_players()) |>
      mutate(dollars = NA_real_, floor_dollars = NA_real_, ceiling_dollars = NA_real_)
    bind_rows(available, taken) |>
      mutate(
        status = case_when(
          player %in% kept$player ~ "Keeper",
          player %in% drafted() ~ "Drafted",
          TRUE ~ "Available"
        )
      ) |>
      arrange(desc(value)) |>
      mutate(rank = row_number())
  })
  
  shown <- reactive({
    df <- ranked()
    if (isTRUE(input$hide_taken)) df <- filter(df, status == "Available")
    if (input$pos_filter != "All") df <- filter(df, pos == input$pos_filter)
    df
  })
  
  league_snapshot_df <- reactive({
    qb <- if (settings()$superflex) "Superflex" else "1 QB"
    snap <- tibble(
      Item = c("Format", "Teams", "Scoring", "QB", "RB", "WR", "TE", "FLEX", "Bench", "ADP blend", "Reception pts"),
      Value = c(
        if (is_auction()) "Dynasty auction" else "Redraft",
        as.character(input$teams), input$ppr_type, qb,
        as.character(input$rb), as.character(input$wr), as.character(input$te),
        as.character(input$flex), as.character(input$bench),
        sprintf("%.2f", input$lambda), sprintf("%.2f", input$rec)
      )
    )
    if (is_auction()) {
      kept <- active_keepers()
      snap <- bind_rows(
        snap,
        tibble(
          Item = c("Team cap", "League cap", "Keepers", "Keeper salaries", "Remaining league cap"),
          Value = c(
            paste0("$", input$budget),
            paste0("$", input$teams * input$budget),
            as.character(nrow(kept)),
            paste0("$", sum(kept$salary, na.rm = TRUE)),
            paste0("$", input$teams * input$budget - sum(kept$salary, na.rm = TRUE))
          )
        )
      )
    }
    snap
  })
  
  output$league_label <- renderText({
    qb <- if (settings()$superflex) "Superflex" else "1 QB"
    fmt <- if (is_auction()) "dynasty auction" else "redraft"
    cap <- if (is_auction()) paste0(" | Cap: $", input$budget) else ""
    sprintf(
      "%s-team %s %s | %s, %s RB, %s WR, %s TE, %s FLEX | Bench: %s%s",
      input$teams, input$ppr_type, fmt, qb,
      input$rb, input$wr, input$te, input$flex, input$bench, cap
    )
  })
  
  output$keeper_table <- renderDT({
    datatable(
      if (!use_keepers() || nrow(keepers()) == 0) tibble(player = character(), salary = numeric()) else keepers(),
      selection = "multiple",
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, info = FALSE)
    )
  })
  
  output$board <- renderDT({
    df <- shown() |>
      mutate(across(c(adp, floor, value, ceiling, points), ~ round(as.numeric(.x), 2))) |>
      mutate(
        player_html = mapply(player_cell, player, status),
        team_html = vapply(team, team_logo_cell, character(1))
      )
    
    if (is_auction()) {
      df <- mutate(df, floor_show = floor_dollars, value_show = dollars, ceiling_show = ceiling_dollars)
      value_labels <- c("Floor $", "Value $", "Ceiling $")
    } else {
      df <- mutate(df, floor_show = floor, value_show = value, ceiling_show = ceiling)
      value_labels <- c("Floor", "Value", "Ceiling")
    }
    
    cols <- c("rank", "status", "pos", "player_html", "team_html", "adp", "floor_show", "value_show", "ceiling_show", "pos_rank", "points")
    labels <- c("Rank", "Status", "Pos", "Player", "Team", "ADP", value_labels, "Pos Rank", "Season Pts")
    round_cols <- if (is_auction()) c("adp", "points") else c("adp", "floor_show", "value_show", "ceiling_show", "points")
    
    datatable(
      select(df, all_of(cols)),
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      filter = "top",
      callback = draft_js,
      options = list(
        paging = FALSE,
        info = FALSE,
        lengthChange = FALSE,
        searching = FALSE,
        dom = "t",
        scrollX = TRUE,
        rowCallback = JS(
          "function(row, data) {",
          "  if (data[1] === 'Drafted' || data[1] === 'Keeper') $(row).addClass('taken-row');",
          "}"
        )
      ),
      colnames = labels
    ) |>
      formatRound(columns = round_cols, digits = 2) |>
      formatStyle(
        "value_show",
        color = styleInterval(0, c("#b91c1c", "#15803d")),
        fontWeight = "700",
        backgroundColor = styleInterval(0, c("#fee2e2", "#dcfce7"))
      ) |>
      formatStyle(
        "status",
        backgroundColor = styleEqual(c("Available", "Drafted", "Keeper"), c("#d4edda", "#f8d7da", "#fff3cd"))
      )
  })
  
  output$download <- downloadHandler(
    filename = function() paste0("rankings-", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(shown() |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> select(-any_of("porp_week")), file)
    }
  )
  
  output$download_snapshot <- downloadHandler(
    filename = function() paste0("league-snapshot-", Sys.Date(), ".csv"),
    content = function(file) write_csv(league_snapshot_df(), file)
  )
  
  output$download_snapshot2 <- downloadHandler(
    filename = function() paste0("league-snapshot-", Sys.Date(), ".csv"),
    content = function(file) write_csv(league_snapshot_df(), file)
  )
  
  output$league_snapshot <- renderTable(league_snapshot_df())
  
  output$auction_snapshot <- renderTable({
    req(is_auction())
    kept <- active_keepers()
    tibble(
      Item = c("Team cap", "League cap", "Keepers", "Keeper salaries", "Remaining league cap"),
      Value = c(
        paste0("$", input$budget),
        paste0("$", input$teams * input$budget),
        nrow(kept),
        paste0("$", sum(kept$salary, na.rm = TRUE)),
        paste0("$", input$teams * input$budget - sum(kept$salary, na.rm = TRUE))
      )
    )
  })
  
  output$league_keepers <- renderDT({
    datatable(active_keepers(), rownames = FALSE, options = list(paging = FALSE, info = FALSE, dom = "t"))
  })
  
  output$league_drafted <- renderDT({
    datatable(
      ranked() |> filter(status == "Drafted") |> mutate(value = round(value, 2)) |> select(player, pos, team, value),
      rownames = FALSE,
      options = list(paging = FALSE, info = FALSE, dom = "t")
    )
  })
}

shinyApp(ui, server)