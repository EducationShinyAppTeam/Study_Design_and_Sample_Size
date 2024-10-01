# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(BrailleR)

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
        menuItem("Explore Influences", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("Explore Tradeoffs", tabName = "tradeoffs", icon = icon("wpexplorer")),
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
          p("An important part of designing any study is to think through how
            large your sample size should be. This app allows you to explore
            how several design elements interact with each other and sample size."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the relationships between key design elements and
                    sample size.")
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
            "This app was inspired by the G*Power application for Windows and Mac
            computers as well as the WebPower R package.",
            br(),
            br(),
            "This version of the app was developed and coded by Neil J.
            Hatfield.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 10/1/2024 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following concepts."),
          p(tags$strong("Sample size:"), "the (total) number of objects/living
            beings that we are recording information from in order to build our
            data set in our study. Depending on the design, these could be our
            measurement units or they could be the experimental units. This size
            can refer to two different counts: the total number across all groups
            (", tags$em("N"),") or the number within group", tags$em("i"),
            "\\(n_i\\). Pay attention to context to help you decide which is at
            play."),
          p(tags$strong("Number of Groups:"), "the number of groups in a study
            refers to how many sub-collections we're dividing up our units into.
            In an experiment, this encompass all combinations of treatments as
            well as any blocks. We often represent the number of groups with",
            tags$em("k.")),
          box(
            title = strong("Type I Risk"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Type I Risk is the probability that we will make a Type I error.
            That is, rejecting a null hypothesis that does the better job describing
            the phenomenon under study. The Type I risk can be represented with
            the symbols \\(\\mathcal{E}_{I}\\) or \\(\\alpha\\)."
          ),
          box(
            title = strong("Type II Risk & Power"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "Type II Risk is the probability that we will make a Type II error.
            That is, failing to reject a null hypothesis that does not
            describe the phenomenon under study as well as the alternative hypothesis.
            The complement of Type II Risk is", tags$strong("[statistical] power"),
            "or the probability of detecting an actual impact not covered by the
            null model. We can represent Type II Risk as \\(\\mathcal{E}_{II}\\)
            and power as \\(1-\\mathcal{E}_{II}\\). When doing power analysis or
            calculating sample size, we often use power instead of the Type II risk."
          ),
          box(
            title = strong("Effect Size, Cohen's f"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "To calculate a sample size, we will need an estimate of the effect
            size. In one-way ANOVA/ANCOVA contexts, one effect size estimator is
            Cohen's", tags$em("f"), "statistic. This provides a measure of how
            much variation in the response is explained by our factor."
          ),
          box(
            title = strong("Balanced and Imbalanced Designs"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "A", tags$strong("balanced design"), "is one where each group has
            exactly the same sample size as every other group in the study. An",
            tags$strong("imbalanced design"), "is a study where at least two groups
            do not have the same sample size. Balanced designs tend to have
            higher statistical power, better robustness to violations of assumptions,
            allow for better separation of factor effects, and are typicaly more
            efficient than imbalanced designs. While we might prefer balanced
            designs in general, there are some situations where imbalance is
            desirable."
          )
        ),
        ### Explore Influences Page ----
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
                  choices = c("Effect size", "Type I Risk", "Power")
                ),
                numericInput(
                  inputId = "numGroups",
                  label = "Number of groups, k",
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
                  inputId = "power",
                  label = "Power, \\(1-\\mathcal{E}_{II}\\)",
                  value = 0.85,
                  min = 0.6,
                  max = 0.99,
                  step = 0.01
                ),
                sliderInput(
                  inputId = "effectSize",
                  label = "Effect size, Cohen's \\(f\\)",
                  value = 0.1,
                  min = 0.05,
                  max = 0.55,
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
              tags$script(HTML(
                "$(document).ready(function() {
                document.getElementbyId('sizePlot').setAttribute('aria-describedby',
                `SampleSizePlotDesc`)
                })"
              )),
              tags$details(
                id = "SampleSizePlotDesc",
                tags$summary("Sample size plot description"),
                uiOutput(outputId = "plotDesc")
              ),
              br(),
              h3("Suggested Sample Size"),
              uiOutput(outputId = "suggestedSize")
            )
          )
        ),
        ### Tradeoffs Page ----
        tabItem(
          tabName = "tradeoffs",
          withMathJax(),
          h2("Tradeoffs"),
          p("When designing a study, we need to think about the values we choose
            for our Type I and Type II risks. Often times, these two risks involve
            a tradeoff between them. Use the tabs to explore the nature of the
            relationship between Type I and Type II risks when designing a study."),
          tabsetPanel(
            id = "T1T2Explore",
            type = "tabs",
            #### Fixed ----
            tabPanel(
              title = "Fixed",
              br(),
              h3("Fixed Values Setting"),
              p("In this setting you'll specify value for the number of groups,
                total sample size, and the effect size. The app will treat these
                values fixed and won't change them as you then adjust the Type I
                risk."),
              p("The shaded portions of graph represent the probability of making
                a Type I error (red shaded region of the Null model) or of making
                a Type II error (blue shaded region of the Alternative model)."),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    numericInput(
                      inputId = "fixedGrps",
                      label = "Number of groups, k",
                      value = 5,
                      min = 2,
                      step = 1
                    ),
                    numericInput(
                      inputId = "fixedSize",
                      label = "Total sample size, N",
                      value = 100,
                      min = 10,
                      step = 1
                    ),
                    numericInput(
                      inputId = "fixedEffect",
                      label = "Effect size, f",
                      value = 0.25,
                      min = 0.05,
                      max = 0.55,
                      step = 0.05
                    ),
                    sliderInput(
                      inputId = "fixedT1",
                      label = "Type I risk, \\(\\mathcal{E}_{I}\\)",
                      min = 0.01,
                      value = 0.07,
                      max = 0.15,
                      step = 0.01
                    ),
                    uiOutput(outputId = "fixedT2Risk")
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "fixedPlot"),
                  tags$script(HTML(
                    "$(document).ready(function() {
                document.getElementbyId('fixedPlot').setAttribute('aria-describedby',
                `FixedTradeoffsDesc`)
                })"
                  )),
                  tags$details(
                    id = "FixedTradeoffsDesc",
                    tags$summary("Plot description"),
                    uiOutput("fixedPlotDescription")
                  )
                )
              )
            ),
            #### Varied ----
            tabPanel(
              title = "Varied",
              br(),
              h3("Varied Sample Size"),
              p("In this setting you'll specify value for the number of groups
                and the effect size. As you adjust the Type I and Type II risks,
                the app embedded the general tradeoff between the risks in the
                calculation of sample size."),
              p("The shaded portions of graph represent the probability of making
                a Type I error (red shaded region of the Null model) or of making
                a Type II error (blue shaded region of the Alternative model)."),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    numericInput(
                      inputId = "variedGrps",
                      label = "Number of groups, k",
                      value = 5,
                      min = 2,
                      step = 1
                    ),
                    numericInput(
                      inputId = "variedEffect",
                      label = "Effect size, f",
                      value = 0.25,
                      min = 0.05,
                      max = 0.55,
                      step = 0.05
                    ),
                    sliderInput(
                      inputId = "variedT1",
                      label = "Type I risk, \\(\\mathcal{E}_{I}\\)",
                      min = 0.01,
                      value = 0.07,
                      max = 0.15,
                      step = 0.01
                    ),
                    sliderInput(
                      inputId = "variedT2",
                      label = "Type II risk, \\(\\mathcal{E}_{II}\\)",
                      min = 0.01,
                      value = 0.2,
                      max = 0.4,
                      step = 0.01
                    ),
                    uiOutput(outputId = "variedSize")
                  )
                ),
                column(
                  width = 8,
                  plotOutput(outputId = "variedPlot"),
                  tags$script(HTML(
                    "$(document).ready(function() {
                document.getElementbyId('variedPlot').setAttribute('aria-describedby',
                `VariedTradeoffsDesc`)
                })"
                  )),
                  tags$details(
                    id = "VariedTradeoffsDesc",
                    tags$summary("Plot description"),
                    uiOutput("variedPlotDescription")
                  )
                )
              )
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2022). shinyBS: Twitter bootstrap components for shiny.
            (v0.61.1). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2024). boastUtils: BOAST utilities.
            (v. 0.1.12.2). [R package]. Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Chang, W., and Borges Ribiero, B. (2021). shinydashboard: Create
            dashboards with 'shiny'. (v 0.7.2). [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B., Xie,
            Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2024). shiny:
            Web application framework for R. (v 1.9.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Faul, F., Earth fields, E., Lang, A.-G., and Buchner, A. (2007).
            G*Power 3: A flexible statistical power analysis program for the
            social, behavioral, and biomedical sciences. Behavior Research Methods,
            39, 175-191."
          ),
          p(
            class = "hangingindent",
            "Godfrey, A., Warren, D., Thompson, J., Murrell, P., Bilton, T., and
            Sorge, V. (2023). BrailleR: Improved access for blind users. (v 1.0.2).
            [R package]. Available from https://CRAN.R-project.org/package=BrailleR"
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: Custom
            input widgets for shiny. (v 0.8.6). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis. (v 3.5.1).
            [R package]. Springer-Verlag:New York. Available from
            https://ggplot2.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Zhang, Z. and Mai, Y. (2023). WebPower: Basic and advanced statistical
            power analysis. (v 0.9.4). [R package]. Available from https://CRAN.R-project.org/package=WebPower"
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

  ## Influences Plot ----
  observeEvent(
    eventExpr = c(input$horizQuant, input$numGroups, input$type1Risk,
                  input$power, input$effectSize),
    handlerExpr = {
      if (input$horizQuant == "Effect size") {
        ### Effect size ----
        sizes <- sapply(
          X = seq(0.05, 0.5, 0.05),
          FUN = getSize,
          k = input$numGroups,
          alpha = input$type1Risk,
          power = input$power
        )
        points <- data.frame(
          effect_size = seq(0.05, 0.5, 0.05),
          rawSize  = sizes,
          size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = effect_size, y = rawSize)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.05, 0.55),
            args = list(k = input$numGroups, alpha = input$type1Risk,
                        power = input$power)
          ) +
          geom_vline(
            xintercept = input$effectSize,
            color = "red"
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
          X = c(0.01, 0.025, 0.03, 0.05, 0.075, 0.1, 0.125, 0.15),
          FUN = getSize,
          k = input$numGroups,
          f = input$effectSize,
          power = input$power
        )
        points <- data.frame(
          alpha = c(0.01, 0.02, 0.03, 0.05, 0.07, 0.1, 0.125, 0.15),
          rawSize = sizes,
          size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = alpha, y = rawSize)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.01, 0.15),
            args = list(k = input$numGroups, f = input$effectSize,
                        power = input$power)
          )  +
          geom_vline(
            xintercept = input$type1Risk,
            color = "red"
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
      } else if (input$horizQuant == "Power") {
        ### Power ----
        sizes <- sapply(
          X = c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99),
          FUN = getSize,
          k = input$numGroups,
          f = input$effectSize,
          alpha = input$type1Risk
        )
        points <- data.frame(
          power = c(0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99),
          rawSize = sizes,
          size = sampleRounding(n = sizes, k = input$numGroups)
        )

        sampleSizePlot <- ggplot(
          data = points,
          mapping = aes(x = power, y = rawSize)
        ) +
          geom_point(color = "blue", size = 3) +
          stat_function(
            fun = getSize,
            xlim = c(0.6, 0.99),
            args = list(k = input$numGroups, f = input$effectSize,
                        alpha = input$type1Risk)
          )  +
          geom_vline(
            xintercept = input$power,
            color = "red"
          ) +
          labs(
            title = "Suggested Sample Size for Given Design",
            y = "Sample size (n)",
            x = "Power"
          ) +
          theme_bw() +
          theme(
            text = element_text(size = 18)
          )
      }

      output$sizePlot <- renderPlot(
        expr = {sampleSizePlot}
      )

      ### Generate Description ----
      description <- BrailleR::VI(x = sampleSizePlot)
      description <- paste0("<p>", description$text, "</p>", collapse = "", recycle0 = TRUE)

      output$plotDesc <- renderUI(
        expr = {HTML(description)}
      )

      ### Sample Size Message ----
      output$suggestedSize <- renderUI(
        expr = {
          p("Based upon your settings, the raw sample size such a study is",
            round(
              digits = 2,
              getSize(k = input$numGroups, f = input$effectSize,
                      alpha = input$type1Risk, power = input$power)
            ), ". Adjusting this value for a", tags$em("balanced study design"),
            "we get a suggested sample size of",
            sampleRounding(
              k = input$numGroups,
              n = getSize(k = input$numGroups, f = input$effectSize,
                          alpha = input$type1Risk, power = input$power)
            ), "."
          )
        }
      )

      ## Fixed Tradeoffs Plot ----
      observeEvent(
        eventExpr = c(input$fixedGrps, input$fixedSize, input$fixedEffect,
                      input$fixedT1),
        handlerExpr = {
          ### Crit Val Calc ----
          critVal <- qf(
            p = input$fixedT1,
            df1 = input$fixedGrps - 1,
            df2 = input$fixedSize - input$fixedGrps,
            lower.tail = FALSE
          )
          newT2Risk <- pf(
            q = critVal,
            df1 = input$fixedGrps - 1,
            df2 = input$fixedSize - input$fixedGrps,
            ncp = input$fixedSize * (input$fixedEffect^2)
          )
          output$fixedT2Risk <- renderUI(
            expr = {
              withMathJax(
                p("Type II Risk, \\(\\mathcal{E}_{II}=\\)", round(newT2Risk, digits = 2))
              )
            }
          )

          ### Make Plot ----
          mainPlot <- ggplot() +
            stat_function(
              mapping = aes(color = "Null"),
              fun = df,
              args = list(
                df1 = input$fixedGrps - 1,
                df2 = input$fixedSize - input$fixedGrps),
              xlim = c(0, 10)
            ) +
            stat_function(
              mapping = aes(fill = "Null"),
              geom = "area",
              fun = df,
              args = list(
                df1 = input$fixedGrps - 1,
                df2 = input$fixedSize - input$fixedGrps
              ),
              alpha = 0.2,
              xlim = c(critVal, 10)
            ) +
            stat_function(
              mapping = aes(color = "Alternative"),
              fun = df,
              args = list(
                df1 = input$fixedGrps - 1,
                df2 = input$fixedSize - input$fixedGrps,
                ncp = input$fixedSize * (input$fixedEffect^2)
              ),
              linetype = "dashed",
              xlim = c(0, 10)
            ) +
            stat_function(
              mapping = aes(fill = "Alternative"),
              geom = "area",
              fun = df,
              args = list(
                df1 = input$fixedGrps - 1,
                df2 = input$fixedSize - input$fixedGrps,
                ncp = input$fixedSize * (input$fixedEffect^2)
              ),
              alpha = 0.2,
              linetype = "dashed",
              xlim = c(0, critVal)
            ) +
            scale_color_manual(
              name = "Model/Hypothesis",
              values = c("Null" = "red", "Alternative" = "blue")
            ) +
            scale_fill_manual(
              name = "Model/Hypothesis",
              values = c("Null" = "red", "Alternative" = "blue")
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.02))
            ) +
            labs(
              title = "PDFs of Central and Noncenteral F",
              x = "Values of the F Ratio",
              y = "Probability Density"
            ) +
            theme_bw() +
            theme(
              text = element_text(size = 18)
            )

          output$fixedPlot <- renderPlot(
            expr = {mainPlot}
          )

          ### Generate Description ----
          description <- BrailleR::VI(x = mainPlot)
          description <- paste0("<p>", description$text, "</p>", collapse = "", recycle0 = TRUE)

          output$fixedPlotDescription <- renderUI(
            expr = {HTML(description)}
          )
        }
      )

      ## Varied Tradeoffs Plot ----
      observeEvent(
        eventExpr = c(input$variedGrps, input$variedEffect,
                      input$variedT1, input$variedT2),
        handlerExpr = {
          ### Sample Size Calc ----
          sampleSize <- getSize(
            k = input$variedGrps,
            alpha = input$variedT1,
            power = 1 - input$variedT2,
            f = input$variedEffect
          )
          balSize <- sampleRounding(n = sampleSize, k = input$variedGrps)

          output$variedSize <- renderUI(
            expr = {
              p("Suggested sample size for balanced design: N=", balSize)
            }
          )

          ### Crit Val Calc ----
          critVal <- qf(
            p = input$variedT1,
            df1 = input$variedGrps - 1,
            df2 = balSize - input$variedGrps,
            lower.tail = FALSE
          )
          newT2Risk <- pf(
            q = critVal,
            df1 = input$variedGrps - 1,
            df2 = balSize - input$variedGrps,
            ncp = balSize * (input$variedEffect^2)
          )

          ### Make Plot ----
          mainPlot <- ggplot() +
            stat_function(
              mapping = aes(color = "Null"),
              fun = df,
              args = list(
                df1 = input$variedGrps - 1,
                df2 = balSize - input$variedGrps),
              xlim = c(0, 10)
            ) +
            stat_function(
              mapping = aes(fill = "Null"),
              geom = "area",
              fun = df,
              args = list(
                df1 = input$variedGrps - 1,
                df2 = balSize - input$variedGrps
              ),
              alpha = 0.2,
              xlim = c(critVal, 10)
            ) +
            stat_function(
              mapping = aes(color = "Alternative"),
              fun = df,
              args = list(
                df1 = input$variedGrps - 1,
                df2 = balSize - input$variedGrps,
                ncp = balSize * (input$variedEffect^2)
              ),
              linetype = "dashed",
              xlim = c(0, 10)
            ) +
            stat_function(
              mapping = aes(fill = "Alternative"),
              geom = "area",
              fun = df,
              args = list(
                df1 = input$variedGrps - 1,
                df2 = balSize - input$variedGrps,
                ncp = balSize * (input$variedEffect^2)
              ),
              alpha = 0.2,
              linetype = "dashed",
              xlim = c(0, critVal)
            ) +
            scale_color_manual(
              name = "Model/Hypothesis",
              values = c("Null" = "red", "Alternative" = "blue")
            ) +
            scale_fill_manual(
              name = "Model/Hypothesis",
              values = c("Null" = "red", "Alternative" = "blue")
            ) +
            scale_y_continuous(
              expand = expansion(mult = c(0, 0.02))
            ) +
            labs(
              title = "PDFs of Central and Noncenteral F",
              x = "Values of the F Ratio",
              y = "Probability Density"
            ) +
            theme_bw() +
            theme(
              text = element_text(size = 18)
            )

          output$variedPlot <- renderPlot(
            expr = {mainPlot}
          )

          ### Generate Description ----
          description <- BrailleR::VI(x = mainPlot)
          description <- paste0("<p>", description$text, "</p>", collapse = "", recycle0 = TRUE)

          output$variedPlotDescription <- renderUI(
            expr = {HTML(description)}
          )
        }
      )

    }
  )


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
