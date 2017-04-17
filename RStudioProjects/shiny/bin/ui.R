code =
  
# Green dashboard page
shinydashboard::dashboardPage(
  skin = "blue",

  # Dashboard header and title
  shinydashboard::dashboardHeader(
    title = "NameMe"
  ),

  # Dashboard sidebar
  shinydashboard::dashboardSidebar(

    # Sidebar menu
    shinydashboard::sidebarMenu(
      id = "sidebarMenu",

      # Home menu item
      shinydashboard::menuItem(
        "Home",
        tabName = "home",
        icon = shiny::icon("home")
      ),

      # Structure menu item
      shinydashboard::menuItem(
        "Structure",
        icon = shiny::icon("random"),
        tabName = "structure"
      ),

      # Parameters menu item
      shinydashboard::menuItem(
        "Parameters",
        tabName = "paramaters",
        icon = shiny::icon("bar-chart")
      ),

      # Inference menu item
      shinydashboard::menuItem(
        "Inference",
        icon = shiny::icon("arrow-right"),
        tabName = "inference"
      ),

      # Measures menu item
      shinydashboard::menuItem(
        "Measures",
        tabName = "measures",
        icon = shiny::icon("table")
      )
    )
  ),

  # Dashboard body
  shinydashboard::dashboardBody(
    id = "dashboardBody",

    # Add favicon and title to header
    tags$head(
      tags$link(rel = "icon", type = "image/png", href = "favicon.png"),
      tags$title("NameMe")
    ),

    # Include introjs UI
    rintrojs::introjsUI(),

    # Dashboard tab items
    shinydashboard::tabItems(

      # Home tab item
      shinydashboard::tabItem(
        tabName = "home",
        shiny::fluidRow(

          # Welcome box
          shinydashboard::box(
            title = "",
            status = "primary",
            width = 8,
            shiny::img(src = "favicon.png",
                       height = 50,
                       width = 50
            ),
            shiny::h2("NameMe"),
            shiny::h4("Visualizing Bayesian Networks using shiny"),
            br(),
            shiny::h4("NameMe is a RProject for visualizing Bayesian networks using via shiny app."
            ),
            shiny::h4("This project is still under development."
            ),
            br(),
            shiny::h4(shiny::HTML('&copy'),
                      '2017 By Kelvin Li. ',
                      shiny::a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.')
            ),
            br()
          ),

          # Nodes and arcs value boxes
          shiny::uiOutput("nodesBox"),
          shiny::uiOutput("arcsBox")
        )
      ),

      # Structure tab item
      shinydashboard::tabItem(tabName = "structure",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,

                                  # Network input box
                                  shinydashboard::box(
                                    title = "Structure Input",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    #shiny::helpText("Select a sample network or upload your Bayesian network data:"),

                                    # Demo network input select
                                    shiny::selectInput(
                                      inputId = "net",
                                      h5("Input:"),
                                      c("Upload your structure" = 1
                                      )
                                    ),

                                    shiny::conditionalPanel(
                                      condition = "input.net == 1",
                                      shiny::p('Note: your structure must be in a matrix format with row and column names and
                                               saved in .csv.'),
                                      
                                      # File input
                                      shiny::fileInput(
                                        'file',
                                        strong('File Input:'),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values',
                                                   '.csv'
                                        )
                                      )
                                    )
                                    
                                  ),
                                  
                                  # Structural learning box
                                  shinydashboard::box(
                                    title = "Opacity",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    sliderInput("opacity", "Opacity", 0.6, min = 0.1, max = 1, step = 0.1)
                                  )

                                  # # Structural learning box
                                  # shinydashboard::box(
                                  #   title = "Structural Learning",
                                  #   status = "primary",
                                  #   collapsible = TRUE,
                                  #   width = NULL,
                                  #   #shiny::helpText("Select a structural learning algorithm:"),
                                  # 
                                  #   # Structural learning algorithm input select
                                  #   shiny::selectizeInput(
                                  #     inputId = "alg",
                                  #     shiny::h5("Learning Algorithm:"),
                                  #     choices = list(
                                  #       "Matrix to DAG" = 
                                  #         c("Matrix to DAG" = "matrix2dag")
                                  #     )
                                  #   )
                                  # ),

                                  # # Network score box
                                  # shinydashboard::box(
                                  #   title = "Network Score",
                                  #   status = "primary",
                                  #   collapsible = TRUE,
                                  #   width = NULL,
                                  # 
                                  #   # Network score function input select
                                  #   shiny::selectInput(
                                  #     "type",
                                  #     h5("Network Score:"),
                                  #     c("Log-Likelihood" = "loglik",
                                  #       "Akaike Information Criterion" = "aic",
                                  #       "Bayesian Information Criterion" = "bic",
                                  #       "Bayesian Equivalent" = "be"
                                  #     ), 'loglik-g'
                                  #   ),
                                  # 
                                  #   # Network score output
                                  #   shiny::verbatimTextOutput("score")
                                  # )
                                ),
                                shiny::column(
                                  width = 8,

                                  # Bayesian network box
                                  shinydashboard::box(
                                    title = "Bayesian Network",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,

                                    # d3 force directed network
                                    networkD3::simpleNetworkOutput("netPlot")
                                  )
                                )
                              )
      ),

      # Paramaters tab item
      shinydashboard::tabItem(tabName = "paramaters",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,

                                  # Paramater learning box
                                  shinydashboard::box(
                                    title = "Paramater Learning",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    shiny::helpText("Select a parameter learning method:"),

                                    # Paramater learning method input select
                                    shiny::selectInput(
                                      "met",
                                      shiny::h5("Learning Method:"),
                                      c("Maximum Likelihood Estimation" = "mle",
                                        "Bayesian Estimation" = "bayes"
                                      )
                                    ),

                                    shiny::helpText("Select an imaginary sample size:"),

                                    # Imaginary Sample Size for illustrative purposes
                                    shiny::numericInput(
                                      "iss",
                                      shiny::h5("Sample Size:"),
                                      value = 10,
                                      min = 1
                                    )
                                  ),

                                  # Paramater infographic box
                                  shinydashboard::box(
                                    title = "Paramater Graphic",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    helpText("Select a paramater infographic:"),

                                    # Paramater infographic input select
                                    selectInput("param", label = h5("Paramater Infographic:"),
                                                ""),

                                    # Conditional panel for discrete data
                                    shiny::conditionalPanel(
                                      "input.param == 'barchart' || input.param == 'dotplot'",

                                      # Node input select
                                      shiny::selectInput("Node", label = shiny::h5("Node:"), "")
                                    )
                                  )
                                  #                                    shinydashboard::box(
                                  #                                      title = "Expert Knowledge", status = "primary", solidHeader = TRUE, collapsible = TRUE, width = NULL, height = 1000,
                                  #                                      shiny::selectInput("Node", label = h5("Node:"),
                                  #                                                  ""),
                                  #                                      shiny::helpText("Add expert knowledge to your model (Experimental):"),
                                  #                                      shiny::actionButton("saveBtn", "Save"),
                                  #                                      rhandsontable::rHandsontableOutput("hot")
                                  #                                    )
                                ),
                                shiny::column(
                                  width = 8,

                                  # Network paramaters box
                                  shinydashboard::box(
                                    title = "Network Paramaters",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,

                                    # Conditional PD plot
                                    shiny::plotOutput("condPlot")
                                  )
                                )
                              )
      ),

      # Inference tab item
      shinydashboard::tabItem(tabName = "inference",
                              shiny::fluidRow(
                                shiny::column(
                                  width = 4,

                                  # Evidence box
                                  shinydashboard::box(
                                    title = "Evidence",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    helpText("Select evidence to add to the model:"),
                                    shiny::fluidRow(
                                      shiny::column(6,

                                                    # Evidence node input select
                                                    shiny::selectInput(
                                                      "evidenceNode", label = shiny::h5("Evidence Node:"),
                                                      ""
                                                    )),
                                      shiny::column(6,

                                                    # Conditional panel for discrete data
                                                    shiny::conditionalPanel(
                                                      "input.param == 'barchart' || input.param == 'dotplot'",

                                                      # Evidence input select
                                                      shiny::selectInput(
                                                        "evidence", label = shiny::h5("Evidence:"),
                                                        ""
                                                      )
                                                    )
                                      )
                                    )
                                  ),

                                  # Event box
                                  shinydashboard::box(
                                    title = "Event",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,
                                    helpText("Select an event of interest:"),

                                    # Event node input select
                                    shiny::selectInput("event", label = shiny::h5("Event Node:"),
                                                       "")
                                  )
                                ),
                                shiny::column(
                                  width = 8,

                                  # Event paramater box
                                  shinydashboard::box(
                                    title = "Event Paramater",
                                    status = "primary",
                                    collapsible = TRUE,
                                    width = NULL,

                                    # Event conditional PD plot
                                    shiny::plotOutput("distPlot")
                                  )
                                )
                              )
      ),

      # Measures tab item
      shinydashboard::tabItem(tabName = "measures",
                              shiny::fluidRow(

                                # Node measure controls box
                                shinydashboard::box(
                                  title = "Node Measure Control",
                                  status = "primary",
                                  collapsible = TRUE,
                                  width = 4,
                                  shiny::helpText("Select a node measure:"),

                                  # Node measure input select
                                  shiny::selectInput(
                                    "nodeMeasure",
                                    h5("Node Measure:"),
                                    c("Markov Blanket" = "mb",
                                      "Neighborhood" = "nbr",
                                      "Parents" = "parents",
                                      "Children" = "children",
                                      "In Degree" = "in.degree",
                                      "Out Degree" = "out.degree",
                                      "Incident Arcs" = "incident.arcs",
                                      "Incoming Arcs" = "incoming.arcs",
                                      "Outgoing Arcs" = "outgoing.arcs"
                                    )
                                  ),

                                  # Node input select
                                  shiny::selectInput("nodeNames", label = shiny::h5("Node:"),
                                                     "")
                                ),

                                # Node measure box
                                shinydashboard::box(
                                  title = "Node Measure",
                                  status = "primary",
                                  collapsible = TRUE,
                                  width = 8,

                                  # Node measure output
                                  shiny::verbatimTextOutput("nodeText")
                                )
                              ),
                              fluidRow(

                                # Network measure control box
                                shinydashboard::box(
                                  title = "Network Measure Control",
                                  status = "primary",
                                  collapsible = TRUE,
                                  width = 4,
                                  shiny::helpText("Select a network measure:"),

                                  # Network measure input select
                                  shiny::selectInput(
                                    "dendrogram",
                                    h5("Dendrogram:"),
                                    c("Both" = "both",
                                      "Row" = "row",
                                      "Column" = "column",
                                      "None" = "none"
                                    )
                                  )
                                ),

                                # Network measure box
                                shinydashboard::box(
                                  title = "Network Measure",
                                  status = "primary",
                                  collapsible = TRUE,
                                  width = 8,

                                  # d3 heatmap
                                  d3heatmap::d3heatmapOutput("netTable")
                                )
                              )
      )
    )
  )
)
