# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(BrailleR)
library(WebPower)

# Load additional dependencies and setup functions ----
baseSize <- function(k, alpha, power, f) {
  p.body <- quote(
    expr = {
      lambda <- n * f^2
      pf(
        q = qf(p = alpha, df1 = k - 1, df2 = n - k, lower.tail = FALSE),
        df1 = k - 1,
        df2 = n - k,
        ncp = lambda,
        lower.tail = FALSE
      )
    }
  )
  roots <- uniroot(
    f = function(n) {
      eval(p.body) - power
    },
    interval = c(2 + k + 1e-10, 1e+05)
  )
  return(roots$root)
}
getSize <- Vectorize(FUN = baseSize)

sampleRounding <- Vectorize(FUN = function(n, k) {
  if (ceiling(n) %% k == 0) {
    return(ceiling(n))
  } else {
    newSize <- ceiling(n) + (k - ceiling(n) %% k)
    return(newSize)
  }
})

# Define UI for App ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Design: Sample Size",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Study_Design_and_Sample_Size")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Study Design and Sample Size"),
          p("This is a sample Shiny application for BOAST. Remember, this page
            will act like the front page (home page) of your app. Thus you will
            want to have this page catch attention and describe (in general terms)
            what the user can do in the rest of the app."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Challenge yourself."),
            tags$li("Play the game to test how far you've come.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Neil J.
            Hatfield.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/30/2024 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Pre-req 1--Technical/Conceptual Prerequisites are ideas that
                    users need to have in order to engage with your app fully."),
            tags$li("Pre-req 2--Contextual Prerequisites refer to any information
                    about a context in your app that will enrich a user's
                    understandings."),
            tags$li("Pre-req 3"),
            tags$li("Pre-req 4")
          ),
          p("Notice the use of an unordered list; users can move through the
            list any way they wish."),
          box(
            title = strong("Null Hypothesis Significance Tests (NHSTs)"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In the Confirmatory Data Analysis tradition, null hypothesis
            significance tests serve as a critical tool to confirm that a
            particular theoretical model describes our data and to make a
            generalization from our sample to the broader population
            (i.e., make an inference). The null hypothesis often reflects the
            simpler of two models (e.g., 'no statistical difference',
            'there is an additive difference of 1', etc.) that we will use to
            build a sampling distribution for our chosen estimator. These
            methods let us test whether our sample data are consistent with this
            simple model (null hypothesis)."
          ),
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the
            p-value, the more often we would expect our estimator to take on a
            value at least as extreme as what we've seen; the smaller, the less
            often."
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Influences on Sample Size"),
          p("There are a number of elements that can influence how large of a
            sample we should plan for as we design a study. We need to draw upon
            how many groups our design necessitates, our chosen (overall) Type I
            risk, our chosen Type II risk (or its complement, power), and our
            estimate for the effect size. Thinking through how these ideas
            interact with each other and with sample size can help us become
            better designers."),
          p("Use the controls to explore how different elements impact the
            suggested sample size."),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                selectInput(
                  inputId = "horizQuant",
                  label = "Select the quantity on the horizontal axis",
                  choices = c("Effect size", "Type I Risk", "Type II Risk", "Power")
                ),
                numericInput(
                  inputId = "numGroups",
                  label = "Number of groups",
                  value = 5,
                  min = 2,
                  step = 1
                ),
                sliderInput(
                  inputId = "type1Risk",
                  label = "Type I Risk, \\(\\mathcal{E}_{I}\\)",
                  value = 0.07,
                  min = 0.01,
                  max = 0.15,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "type2Risk",
                  label = "Type II Risk, \\(\\mathcal{E}_{II}\\)",
                  value = 0.2,
                  min = 0.01,
                  max = 0.30,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "effectSize",
                  label = "Effect size, Cohen's \\(f\\)",
                  value = 0.1,
                  min = 0.05,
                  max = 1,
                  step = 0.05
                ),
                p("Suggested Values for Cohen's \\(f\\)"),
                tags$ul(
                  tags$li("Negligible: \\(0\\leq f < 0.1\\)"),
                  tags$li("Small: \\(0.1 \\leq f < 0.25\\)"),
                  tags$li("Medium: \\(0.25 \\leq f < 0.4\\)"),
                  tags$li("Large: \\(0.4 \\leq f\\)")
                )
              )
            ),
            column(
              width = 8,
              plotOutput(outputId = "sizePlot"),
              uiOutput(outputId = "plotDesc"),
              h3("Suggested Sample Size"),
              uiOutput(outputId = "suggestedSize")
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p("You'll need to fill in this page with all of the appropriate
            references for your app."),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Explore the relationship between design choices and sample size."
      )
    }
  )

  ## Go button ----
  observeEvent(
    eventExpr = input$go1,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = 'pages',
        selected = 'explore'
      )
    }
  )

  ## Explore Plot ----
  observeEvent(
    eventExpr = c(input$horizQuant, input$numGroups, input$type1Risk,
                  input$type2Risk, input$effectSize),
    handlerExpr = {
      if (input$horizQuant == "Effect size") {
        ### Effect size ----
        sizes <- sapply(
          X = seq(0.05, 0.5, 0.05),
          FUN = getSize,
          k = input$numGroups,
          alpha = input$type1Risk,
          power = 1 - input$type2Risk
        )
        points <- data.frame(
          effect_size = seq(0.05, 0.5, 0.05),
          size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = effect_size, y = size)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.05, 0.5),
            args = list(k = input$numGroups, alpha = input$type1Risk,
                        power = 1 - input$type2Risk)
          ) +
          labs(
            title = "Suggested Sample Size for Given Design",
            y = "Sample size (n)",
            x = "Effect size, f"
          ) +
          theme_bw() +
          theme(
            text = element_text(size = 18)
          )
      } else if (input$horizQuant == "Type I Risk") {
        ### Type I ----
        sizes <- sapply(
          X = seq(0.01, 0.15, 0.025),
          FUN = getSize,
          k = input$numGroups,
          f = input$effectSize,
          power = 1 - input$type2Risk
        )
        points <- data.frame(
          alpha = seq(0.01, 0.15, 0.025),
          raw_size = sizes,
          round_size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = alpha, y = raw_size)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.01, 0.15),
            args = list(k = input$numGroups, f = input$effectSize,
                        power = 1 - input$type2Risk)
          ) +
          labs(
            title = "Suggested Sample Size for Given Design",
            y = "Sample size (n)",
            x = "Type I Risk"
          ) +
          theme_bw() +
          theme(
            text = element_text(size = 18)
          )
      } else if (input$horizQuant == "Type II Risk") {
        ### Type II ----
        sizes <- sapply(
          X = seq(0.015, 0.3, 0.05),
          FUN = getSize,
          k = input$numGroups,
          f = input$effectSize,
          alpha = input$type1Risk
        )
        points <- data.frame(
          type2Risk = seq(0.015, 0.3, 0.05),
          raw_size = sizes,
          round_size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = type2Risk, y = raw_size)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.015, 0.30),
            args = list(k = input$numGroups, f = input$effectSize,
                        alpha = input$type1Risk)
          ) +
          labs(
            title = "Suggested Sample Size for Given Design",
            y = "Sample size (n)",
            x = "Type II Risk"
          ) +
          theme_bw() +
          theme(
            text = element_text(size = 18)
          )
      }

      output$sizePlot <- renderPlot(
        expr = {sampleSizePlot}
      )

      description <- BrailleR::VI(x = sampleSizePlot)
      description <- paste0("<p>", description$text, "</p>", collapse = "", recycle0 = TRUE)

      output$plotDesc <- renderUI(
        expr = {
          tags$details(
            id = "describeSampleSizePlot",
            tags$summary("Sample size plot description"),
            HTML(description)
          )
        }
      )

    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
