library(tidyverse)
library(scales)
library(shiny)
library(ggtext)
library(gt)
# library(googlesheets4)

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

# configuration needed for googlesheets, commented out
# options(
#   gargle_oauth_cache = ".secrets",
#   gargle_oauth_email = TRUE
# )
#
# url <- "https://docs.google.com/spreadsheets/d/140TKlD2VNIwNGA3OntNXyQYKpXydrHFoyvu2W4eYgSw/edit?usp=drive_web&ouid=103984755385597722824"
# dat <- read_sheet(url, "Modern Testing", "B:H")

# created from the call to above, and written out into the folder using write_csv(dat, "testdata.csv")
dat <- read_csv("testdata.csv")

d <- dat |>
  mutate(
    matchup = matchup |> str_trim() |> str_to_title(),
    event = event |> str_trim() |> str_to_title(),
    deck = deck |> str_trim() |> str_to_title()
  )

# app ---------------------------------------------------------------------

ui <- fluidPage(
  includeCSS("www/style.css"),

  titlePanel("Matchup Performance Over Time"),

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

      submitButton("Submit", icon = icon("refresh")),

      br(),
      p(
        "This Shiny app can be used to visualize the performance of a selected ",
        em("Magic: the Gathering"), "(MtG) deck vs. a selected opposing matchup (or matchups).
        It can be configured to read a static file, or read directly from a Google sheet.
        I built the app to help summarize the testing data I collected during
        my preparation for the July 2022 Pacific Sound Battlegrounds Trios tournament.
        I was playing the ", em("Modern"), "format, one of the most popular ways of
        varieties of playing MtG, and a format known for its diverse metagame.",
        br(),
        br(),
        "Andrew Elenbogen, a well-known player from Michigan, and Pro-Tour victor
        once ", a("tweeted", href = "https://twitter.com/Ajelenbogen/status/1083444796956524544?s=20&t=fW__X92fgPZHtZvMEhj07Q"),
        " that \"competitive Magic is the art of correctly generalizing from sample
        sizes too small to draw real conclusions.\" I've found this holds up, in
        my relatively limited experiences with high-level tournaments. In the deck
        I ended up submitting for the event (V11), I ended up playing", strong("24"),
        "unique matchups during my practice, but only played against ", strong("2"),
        " of them during the tournament. That said, even with small sample sizes,
        collecting the data helped me understand what was and wasn't working across
        the iterations of deck-building I made. The V11 list ultimately emerged from
        small changes to my (mostly successful) V6 skeleton."
      )
    ),

    mainPanel(
      # plotlyOutput("mwot", height = "100%", width = "100%"),
      plotOutput("mwot"),
      br(),
      br(),
      gt::gt_output("summary_tbl"),
      br(),
      br()
    )
  )
)

server <- function(input, output, session) {
  output$mwot <- renderPlot({ # renderPlotly({
    d$deck <- case_when(
      length(input$deck) > 1 & d$deck %in% input$deck ~ "Selected Deck(s)",
      length(input$deck) == 1 & d$deck == input$deck  ~ "Selected Deck(s)",
      TRUE                                            ~ d$deck
    )

    to_keep <- if (!is.null(input$matchup)) d$matchup %in% input$matchup else TRUE

    out <- d |>
      arrange(deck, date) |>
      filter(to_keep) |>
      group_by(deck) |>
      mutate(
        mnum = 1:n(),
        cmw  = cumsum(matchwin == 1),
        cml  = cumsum(matchwin == 0),
        cwp  = cmw / (cmw + cml),
        hl   = factor(deck == "Selected Deck(s)", c(FALSE, TRUE), c(0, 1))
      ) |>
      ungroup()

    p0 <- ggplot() +
      geom_hline(
        yintercept = 0.5,
        lty = "dotted"
      ) +
      geom_line(
        data = filter(out, hl == 0),
        aes(x = mnum, y = cwp, color = hl, group = deck), size = 0.8
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
  })

  output$summary_tbl <- render_gt({
    d$deck <- case_when(
      length(input$deck) > 1 & d$deck %in% input$deck ~ "Selected Deck(s)",
      length(input$deck) == 1 & d$deck == input$deck  ~ "Selected Deck(s)",
      TRUE                                            ~ d$deck
    )

    to_keep <- if (!is.null(input$matchup)) d$matchup %in% input$matchup else TRUE

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
        gwp     = md("**GW%**"),
        mwp     = md("**MW%**")
      ) |>
      fmt_percent(columns = frac_matches:mwp, decimals = 1) |>
      tab_stubhead(label = md("**Matchup**")) |>
      tab_options(
        table.background.color = "#ecf0f5",
        row.striping.include_table_body = TRUE
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
