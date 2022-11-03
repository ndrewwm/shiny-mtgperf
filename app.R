library(tidyverse)
library(scales)
library(shiny)
library(shinyalert)
library(ggtext)
library(gt)

theme_set(
  theme_minimal(base_size = 20) +
    theme(
      panel.grid       = element_blank(),
      axis.ticks       = element_line(),
      axis.title.x     = element_text(hjust = 0.01),
      axis.title.y     = element_text(hjust = 1.00),
      panel.background = element_rect(fill = "#ecf0f5", color = "#ecf0f5"),
      plot.title       = element_markdown(),
      plot.background  = element_rect(fill = "#ecf0f5", color = "#ecf0f5")
    )
)

# import data -------------------------------------------------------------

# demo data
dat <- read_csv("testdata.csv")

d <- dat |>
  mutate(
    matchup = matchup |> str_squish() |> str_to_title(),
    event = event |> str_squish() |> str_to_title(),
    deck = deck |> str_squish() |> str_to_title()
  )

# plotting & table functions ----------------------------------------------

mwot_plot <- function(data, decks, matchups) {
  d <- data

  d$deck <- case_when(
    length(decks) > 1 & d$deck %in% decks ~ "Selected Deck(s)",
    length(decks) == 1 & d$deck == decks  ~ "Selected Deck(s)",
    TRUE                                  ~ d$deck
  )

  to_keep <- if (!is.null(matchups)) d$matchup %in% matchups else TRUE

  out <- d |>
    arrange(deck, date) |>
    filter(to_keep) |>
    group_by(deck) |>
    mutate(
      mnum = 1:n(),
      cmw = cumsum(matchwin == 1),
      cml = cumsum(matchwin == 0),
      cwp = cmw / (cmw + cml),
      hl  = factor(deck == "Selected Deck(s)", c(FALSE, TRUE), c(0, 1))
    ) |>
    ungroup()

  p0 <- ggplot() +
    geom_hline(
      yintercept = 0.5,
      lty = "dotted"
    ) +
    geom_line(
      data = filter(out, hl == 0),
      aes(x = mnum, y = cwp, color = hl, group = deck), size = 0.7
    ) +
    geom_line(
      data = filter(out, hl == 1),
      aes(x = mnum, y = cwp, color = hl, group = deck), size = 1.4
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("#5f88a8", "darkblue")) + # orig: #D1DDE6
    scale_x_continuous(name = "Matches Played", n.breaks = 5) +
    scale_y_continuous(name = "Match-win Proportion", breaks = seq(0, 1, .25)) +
    labs(title = "<span style = 'color: #00008b; font-weight: 900;'>Selected</span> deck(s) versus <span style = 'color: #5f88a8; font-weight: 900;'>other</span> decks.")

  p0
}

mw_tbl <- function(data, decks, matchups) {
  d <- data

  d$deck <- case_when(
    length(decks) > 1 & d$deck %in% decks ~ "Selected Deck(s)",
    length(decks) == 1 & d$deck == decks  ~ "Selected Deck(s)",
    TRUE                                  ~ d$deck
  )

  to_keep <- if (!is.null(matchups)) d$matchup %in% matchups else TRUE

  by_matchup <- d |>
    filter(to_keep, deck == "Selected Deck(s)") |>
    group_by(matchup) |>
    summarise(
      matches = n(),
      gwp = sum(wins) / sum(wins + losses),
      mwp = sum(matchwin) / n()
    ) |>
    arrange(desc(matches))

  overall <- d |>
    filter(to_keep, deck == "Selected Deck(s)") |>
    summarise(
      matchup = "Overall",
      matches = n(),
      gwp = sum(wins) / sum(wins + losses),
      mwp = sum(matchwin) / n()
    )

  bind_rows(overall, by_matchup) |>
    mutate(frac_matches = matches / first(matches)) |>
    relocate(frac_matches, .before = gwp) |>
    gt(rowname_col = "matchup") |>
    cols_label(
      matchup = md("**Matchup**"),
      matches = md("**Matches Played**"),
      frac_matches = md("**Share of Matches Played**"),
      gwp = md("**GW%**"),
      mwp = md("**MW%**")
    ) |>
    fmt_percent(columns = frac_matches:mwp, decimals = 1) |>
    tab_stubhead(label = md("**Matchup**")) |>
    tab_options(
      table.background.color = "#ecf0f5",
      row.striping.include_table_body = TRUE
    )
}

# app ---------------------------------------------------------------------

ui <- navbarPage(
  "Matchup Performance Over Time",
  includeCSS("www/style.css"),
  useShinyalert(),
    
  tabPanel("Demonstration",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "deck",
          "Deck(s)",
          choices = unique(d$deck) |> na.omit(),
          multiple = TRUE,
          selected = "Murktide V11"
        ),

        selectInput(
          "matchup",
          "Matchup(s)",
          choices = d |> count(matchup, sort = TRUE) |> filter(!is.na(matchup)) |> pull(matchup),
          multiple = TRUE
        ),

        actionButton("demo_go", "Submit", icon = icon("refresh")),

        br(),
        p(
          "This Shiny app can be used to visualize the performance of a selected ",
          em("Magic: the Gathering"), " deck vs. a selected opposing matchup (or matchups).
          It can be configured to read a static file, or read directly from a Google sheet.
          I built the app to help summarize the testing data I collected during
          my preparation for the July 2022 Pacific Sound Battlegrounds Trios tournament.
          I was playing the ", em("Modern"), "format, one of the most popular ways of playing ",
          em("Magic,"), " and a format known for its diverse metagame.",
          br(),
          br(),
          "Andrew Elenbogen, a well-known player from Michigan, and Pro-Tour victor
          once ", a("tweeted", href = "https://twitter.com/Ajelenbogen/status/1083444796956524544?s=20&t=fW__X92fgPZHtZvMEhj07Q"),
          " that \"competitive Magic is the art of correctly generalizing from sample
          sizes too small to draw real conclusions.\" I've found this holds up in
          my limited experience with larger tournaments. In the deck
          I ended up submitting for the event (V11), I tested versus ", strong("24"),
          " unique matchups during my practice, but only encountered ", strong("2"),
          " of them in the tournament. That said, even with small sample sizes,
          collecting the data helped me understand what was and wasn't working across
          the different iterations I was trying. The V11 list ultimately emerged from
          small changes to my (somewhat successful) V6 skeleton."
        )
      ),

      mainPanel(
        plotOutput("mwot"),
        br(),
        br(),
        
        gt_output("summary_tbl"),
        br(),
        br()
      )
    )
  ),

  tabPanel("Upload Data",
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Upload a CSV file", accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),

        p(
          "You can upload your own data for summary. The file must be comma-delimited, and have the following names as a header:",
          tags$ul(
            tags$li(strong("deck"), " name/id for the deck you're playing"),
            tags$li(strong("date"), " date of match"),
            tags$li(strong("event"), " name/id of event (e.g. \"modern league\")"),
            tags$li(strong("matchup"), " opposing deck name/archetype"),
            tags$li(strong("wins"), " # game wins"),
            tags$li(strong("losses"), "# game losses"),
            tags$li(strong("matchwin"), "indicator, 1 = win, 0 = loss")
          )
        )
      ),

      mainPanel(
        fluidRow(
          column(width = 5, uiOutput("user_deck_input")),
          column(width = 5, uiOutput("user_matchup_input"))
        ),

        uiOutput("user_submit"),
        br(),
        br(),

        plotOutput("user_plot"),
        br(),
        br(),

        tableOutput("user_summary"),
        br(),
        br()
      )
    )
  )
)

server <- function(input, output, session) {
  # first tab: demonstration data
  demo_deck <- eventReactive(input$demo_go, input$deck)
  demo_matchup <- eventReactive(input$demo_go, input$matchup)

  observeEvent(input$demo_go, {
    if (is.null(input$deck)) shinyalert("Please choose at least 1 deck from the list.")
  })

  output$mwot <- renderPlot(mwot_plot(d, demo_deck(), demo_matchup()))
  output$summary_tbl <- render_gt(mw_tbl(d, demo_deck(), demo_matchup()))

  # second tab: user upload
  user <- reactive({
    req(input$file1)
    read_csv(
      file = input$file1$datapath,
      col_types = cols(
        deck = col_character(),
        date = col_datetime(format = ""),
        event = col_character(),
        matchup = col_character(),
        wins = col_double(),
        losses = col_double(),
        matchwin = col_double()
      ),
      na = c("", "NA", "NULL", "null"),
      comment = "#"
    )
  })

  output$user_deck_input <- renderUI({
    req(input$file1)
    
    decks <- user() |> pull(deck) |> unique() |> na.omit()

    selectInput(
      "user_deck",
      "Deck(s)",
      choices = decks,
      multiple = TRUE
    )
  })

  output$user_matchup_input <- renderUI({
    req(input$file1)

    selectInput(
      "user_matchup",
      "Matchup(s)",
      choices = user() |> pull(matchup) |> unique() |> na.omit(),
      multiple = TRUE
    )
  })

  output$user_submit <- renderUI({
    req(input$file1)
    actionButton("user_go", "Submit", icon = icon("refresh"))
  })

  user_deck <- eventReactive(input$user_go, input$user_deck)
  user_matchup <- eventReactive(input$user_go, input$user_matchup)

  observeEvent(input$user_go, {
    if (is.null(input$user_deck)) shinyalert("Please choose at least 1 deck from the list.")
  })

  output$user_plot <- renderPlot(mwot_plot(user(), user_deck(), user_matchup()))
  output$user_summary <- render_gt(mw_tbl(user(), user_deck(), user_matchup()))
}

# Run the application
shinyApp(ui = ui, server = server)
