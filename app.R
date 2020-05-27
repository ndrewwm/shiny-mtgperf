library(tidyverse)
library(scales)
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(rstanarm)

theme_set(
    theme_minimal(base_size = 16) +
        theme(
            panel.grid       = element_line(color = "#b3b3b3"),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "#ecf0f5", color = "#ecf0f5"),
            plot.background  = element_rect(fill = "#ecf0f5", color = "#ecf0f5")
        )
)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    # includeCSS("www/style.css"),

    dashboardHeader(title = HTML("<b>MW%</b>"), titleWidth = 80),

    dashboardSidebar(
        sidebarMenu(
            menuItem("Upload", tabName = "upload", icon = icon("upload")),
            menuItem("Describe", tabName = "describe", icon = icon("th")),
            menuItem("Predict", tabName = "predict", icon = icon("fas fa-chart-area")),
            fileInput("file", "Excel Upload:", accept = ".xlsx"),
            textInput("email", "Email (optional):", value = "I don't do anything yet! :'("),
            actionButton(inputId = "okay", label = "Okay!", icon = icon("fas fa-check"))
        )
    ),

    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),

        tabItems(
            tabItem(
                tabName = "upload",
                fluidRow(
                    h3("Welcome!"),
                    p("This app can help you analyze your performance in limited MtG events. Before any analysis can be performed, you'll need to upload a data file that contains results from the events you want to examine."),
                    p("This application is meant to with data that is stored in an excel spreadsheet, with each set (i.e. limited format) stored in separate tabs of the file. In order to use this app, you'll need to provide a file structured like the one below.")
                ),
                fluidRow(
                    h3("Example File"),
                    tableOutput("example"),
                    p(HTML("Your file can contain more columns than what is listed, but <b>you must include these 5 columns, with the same column names.</b>")),
                    h3("Data Input/Recording Instructions"),
                    p(HTML(str_glue(
                        "<b>Each individual expansion/set should be recorded as its own sheet in the file. Please use the 3-character set abbreviations for sheet names!</b><br><br>",
                        "Below are brief descriptions on how data should be stored in each column.<br><br>",
                        "<b>date:</b> date of event (YYYY-MM-DD, YYYY/MM/DD, MM/DD/YYYY, MM-DD-YYYY accepted).<br>",
                        "<b>format:</b> a description of the event; the application will search for keywords in this field (e.g. 'draft', 'sealed', 'ptq', 'bo1', 'bo3'). Text will be converted to lowercase upon upload.<br>",
                        "<b>mw</b>: <i>matches</i> won during event.<br>",
                        "<b>ml</b>: <i>matches</i> lost during event.<br>",
                        "<b>colors:</b> the colors used in the event's deck. <i>Case-sensitive!</i> E.g. an entry of 'WUr' would represent a deck playing white & blue, with a splash for red.<br>",
                        ))
                     )
                ),

                fluidRow(
                    h3("Import Preview"),
                    tableOutput("importPreview")
                )
            ),

            tabItem(
                tabName = "describe",
                fluidRow(
                    h3("Describe"),
                    DTOutput("setDesc"),
                    hr(),
                    plotOutput("mwpOverTime")
                )
            ),

            tabItem(
                tabName = "predict",
                fluidRow(
                    h3("Predict")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    output$example <- renderTable(
        tribble(
            ~date, ~format, ~mw, ~ml, ~colors,
            "2020-01-25", "mtgo swiss draft", 2, 1, "UB",
            "2020-01-25", "mtgo swiss draft", 3, 0, "UBr",
            "2020-01-25", "mtgo swiss draft", 3, 0, "UR",
            "2020-01-26", "mtgo swiss draft", 0, 2, "UR",
            "2020-01-26", "mtgo swiss draft", 2, 1, "WU"
        )
    )

    in_data <- eventReactive(input$okay, {
        infile <- input$file

        validate(need(str_detect(infile$datapath, "xlsx"), "Please ensure you've uploaded an excel file."))

        sets <- excel_sheets(infile$datapath)

        # read in each sheet
        dat <- map_df(
            sets,
            ~read_excel(infile$datapath, sheet = .),
            .id = "set"
        )

        # replace numeric sheet ID w/ sheet names
        dat$set <- factor(dat$set, levels = unique(dat$set), sets)

        names(dat) <- str_to_lower(names(dat))

        if (!all(c("set", "date", "format", "mw", "ml", "colors") %in% names(dat))) {
            NULL
        } else {
            dat <- dat %>%
                select(set, date, format, mw, ml, colors) %>%
                mutate(
                    bo1    = str_detect(format, "bo1"),
                    sealed = str_detect(str_to_lower(format), "sealed|finals|main event"),
                    compet = str_detect(str_to_lower(format), "prelim|main event|ptq|mcq"),
                    splash = str_detect(colors, "[a-z]"),
                    cols   = str_remove_all(colors, "[a-z]"),
                    spcols = str_remove_all(colors, "[A-Z]")
                ) %>%
                mutate_at(vars(bo1:splash), as.numeric)

            # color combinations
            wubrg <- c("W", "U", "B", "R", "G")
            guild <- c("WU", "WB", "WR", "WG", "UB", "UR", "UG", "RB", "RG", "BG")

            cc <- c(wubrg, guild)

            for (combo in cc) dat[, combo] <- str_detect(dat$cols, combo)

            dat <- mutate_at(dat, vars(W:BG), as.numeric)

            dat
        }
    })

    output$importPreview <- renderTable({
        validate(need(!is.null(in_data()), "Please ensure that all the required columns are included."))

        in_data()[1:5, 1:6] %>% mutate(date = as.character(date))
    })

    output$setDesc <- renderDT({
        validate(need(!is.null(in_data()), "Please upload a data file for analysis."))

        in_data() %>%
            group_by(set) %>%
            summarise_at(vars(bo1:splash, W:G), mean, na.rm = T) %>%
            left_join(
                in_data() %>%
                    group_by(set) %>%
                    summarise(
                        events  = n(),
                        matches = sum(mw + ml),
                        mwp     = sum(mw) / matches
                    ),
                by = "set"
            ) %>%
            mutate_at(vars(-set, -events, -matches), round, 2) %>%
            select(set, events, matches, mwp, everything()) %>%
            DT::datatable(
                extensions = "FixedColumns",
                options = list(
                    dom = 't',
                    scrollX = TRUE,
                    scrollY = TRUE,
                    paging  = FALSE,
                    fixedHeader = TRUE,
                    fixedColumns = list(leftColumns = 2, rightColumns = 0))
            )
    })

    output$mwpOverTime <- renderPlot({
        validate(need(!is.null(in_data()), "Please upload a data file for analysis."))

        p_mwp_ot <- in_data() %>%
            group_by(set) %>%
            summarise(pct = sum(mw) / sum(mw + ml)) %>%
            ggplot(aes(x = fct_rev(set), y = pct)) +
            geom_col() +
            labs(
              x = "",
              y = "",
              title = "Match-win % by Set"
            )

        if (max(p_mwp_ot$pct) > .7) {
            p_mwp_ot + scale_y_continuous(labels = percent)
        } else {
            p_mwp_ot + scale_y_continuous(labels = percent, limits = c(0, .7))
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
