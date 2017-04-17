#' @import bnlearn
#' @import rintrojs
#' @import shiny
#' @import shinyAce
#' @import shinydashboard
# Define required server logic
require(bnlearn)
require(shinydashboard)

matrix2dag = function(mtx) {
  
  dag = empty.graph(colnames(mtx)) 
  
  for (i in 1:nrow(mtx)) {
    
    for (j in 1:ncol(mtx)) {
      
      if (mtx[i, j] == 1) {
        
        if (mtx[j, i] == 1) {
          
          dag = set.edge(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
          mtx[j, i] = 0
          
        } else {
          
          dag = set.arc(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
          
        } # end else 
        
        mtx[i, j] = 0
        
      } # end if 
      
    }
    
  }
  
  return(dag)
  
}

shinyServer(function(input, output, session) {

  # Get the data selection from user
  dat <- shiny::reactive({
    if (input$net == 1) {
      inFile <- input$file
      if (is.null(inFile))
        return(NULL)
      dat <- read.csv(inFile$datapath, row.names = 1)
    }
  })

  # Learn the structure of the network
  dag <- shiny::reactive({
    if (is.null(dat()))
      return(NULL)

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Learning network structure", value = 0)

    # Get the selected learning algorithm from the user and learn the network
    # if (input$alg == "matrix2dag") {
    #   dag <- matrix2dag(dat())
    # }
    dag <- matrix2dag(dat())

  })

  # Create the nodes value box
  output$nodesBox <- shiny::renderUI({
    if (is.null(dat()))
      return(NULL)

    # Get the number of nodes in the network
    nodes <- bnlearn::nnodes(dag())

    shinydashboard::valueBox(nodes,
                             "Nodes",
                             icon = shiny::icon("circle"),
                             color = "blue")
  })

  # Create the arcs value box
  output$arcsBox <- renderUI({
    if (is.null(dat()))
      return(NULL)

    # Get the number of arcs in the network
    arcs <- bnlearn::narcs(dag())

    shinydashboard::valueBox(arcs,
                             "Arcs",
                             icon = shiny::icon("arrow-right"),
                             color = "green")
  })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$homeIntro,
                      rintrojs::introjs(session, options = list(steps = homeHelp))
  )

  # Plot the d3 force directed network
  # output$netPlot <- networkD3::renderSimpleNetwork({
  #   if (is.null(dat()))
  #     return(NULL)
  # 
  #   # Get the arc directions
  #   networkData <- data.frame(bnlearn::arcs(dag()))
  # 
  #   networkD3::simpleNetwork(
  #     networkData,
  #     Source = "from",
  #     Target = "to",
  #     zoom = TRUE
  #   )
  # 
  # })

  output$netPlot <- networkD3::renderForceNetwork({
    if (is.null(dat()))
      return(NULL)
    
    # Get the arc directions
    #networkData <- data.frame(bnlearn::arcs(dag()))
    vars = bnlearn::nodes(dag())
    nodesDF = data.frame(name = vars, group = 1:(bnlearn::nnodes(dag())), size = 10)
    edgesDF = data.frame(source = match(dag()$arcs[, 1], vars) - 1, target = match(dag()$arcs[, 2], vars) - 1, value = 10)
    
    networkD3::forceNetwork(Links = edgesDF, Nodes = nodesDF,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = input$opacity, arrows = TRUE, zoom = TRUE)
    
  })
  
  # # Print the network score
  # output$score <- shiny::renderText({
  #   if (bnlearn::directed(dag())) {
  # 
  #     # If all of the data is numeric,...
  #     if (all(sapply(dat(), is.numeric))) {
  # 
  #       # Get the selected score function from the user and calculate the score
  #       if (input$type == "loglik") {
  #         bnlearn::score(dag(), dat(), type = "loglik-g")
  #       } else if (input$type == "aic") {
  #         bnlearn::score(dag(), dat(), type = "aic-g")
  #       } else if (input$type == "bic") {
  #         bnlearn::score(dag(), dat(), type = "bic-g")
  #       } else {
  #         bnlearn::score(dag(), dat(), type = "bge")
  #       }
  #     }
  # 
  #     # If the data is discrete,...
  #     else {
  #       if (input$type == "loglik") {
  #         bnlearn::score(dag(), dat(), type = "loglik")
  #       } else if (input$type == "aic") {
  #         bnlearn::score(dag(), dat(), type = "aic")
  #       } else if (input$type == "bic") {
  #         bnlearn::score(dag(), dat(), type = "bic")
  #       } else {
  #         bnlearn::score(dag(), dat(), type = "bde")
  #       }
  #     }
  #   } else
  #     shiny::validate(
  #       shiny::need(
  #         try(score != "")
  #         ,
  #         "Make sure your network is completely directed in order to view your network's score..."
  #       )
  #     )
  # })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$structureIntro,
                      rintrojs::introjs(session, options = list(steps = structureHelp))
  )

  # Fit the model parameters
  fit <- shiny::reactive({
    if (is.null(dat()))
      return(NULL)
    if (bnlearn::directed(dag())) {

      # Get the selected paramater learning method from the user and learn the paramaters
      fit <- bnlearn::bn.fit(dag(), dat(), method = input$met, iss = input$iss)
    }
  })

  # Set the paramater graphic options
  graphic <- shiny::reactive({

    # If data is continuous, ...
    if (all(sapply(dat(), is.numeric))) {
      graphic <- c("Histogram" = "histogram",
                   "XY Plot" = "xyplot",
                   "QQ Plot" = "qqplot")

      # If data is discrete,...
    } else {
      graphic <- c("Bar Chart" = "barchart",
                   "Dot Plot" = "dotplot")
    }
  })

  # Send the paramater choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "param", choices = graphic())
  })

  # Send the node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "Node", choices = colnames(dat()))
  })

  # Plot the model parameters
  output$condPlot <- shiny::renderPlot({
    if (is.null(dat()))
      return(NULL)
    if (bnlearn::directed(dag())) {

      # Get the selected graphic from the user and plot the paramaters
      if (input$param == "histogram") {
        bnlearn::bn.fit.histogram(fit())
      } else if (input$param == "xyplot") {
        bnlearn::bn.fit.xyplot(fit())
      } else if (input$param == "qqplot") {
        bnlearn::bn.fit.qqplot(fit())
      } else if (input$param == "barchart") {
        bnlearn::bn.fit.barchart(fit()[[input$Node]])
      } else if (input$param == "dotplot") {
        bnlearn::bn.fit.dotplot(fit()[[input$Node]])
      }
    } else
      shiny::validate(
        shiny::need(
          try(condPlot != "")
          ,
          "Make sure your network is completely directed in order to view the paramater infographics..."
        )
      )
  })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$parametersIntro,
                      rintrojs::introjs(session, options = list(steps = parametersHelp))
  )

  # Send the evidence node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "evidenceNode", choices = names(dat()))
  })

  # Send the evidence choices to the user
  shiny::observe({
    whichNode <- which(colnames(dat()) == input$evidenceNode)
    evidenceLevels <- as.vector(unique(dat()[,whichNode]))
    shiny::updateSelectInput(session, "evidence", choices = evidenceLevels)
  })

  # Send the event node choices to the user
  shiny::observe({
    shiny::updateSelectInput(session, "event", choices = names(dat()))
  })

  # Perform Bayesian inference based on evidence and plot results
  output$distPlot <- shiny::renderPlot({
    if (is.null(dat()))
      return(NULL)
    if (all(sapply(dat(), is.numeric)))
      shiny::validate(
        shiny::need(
          try(distPlot != ""),
          "Inference is currently not supported for continuous variables..."
        )
      )

    # Create a string of the selected evidence
    str1 <<- paste0("(", input$evidenceNode, "=='", input$evidence, "')")

    # Estimate the conditional PD and tabularize the results
    nodeProbs <- prop.table(table(bnlearn::cpdist(fit(), input$event, eval(parse(text = str1)))))

    # Create a bar plot of the conditional PD
    barplot(
      nodeProbs,
      col = "lightblue",
      main = "Conditional Probabilities",
      border = NA,
      xlab = "Levels",
      ylab = "Probabilities",
      ylim = c(0, 1)
    )
  })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$inferenceIntro,
                      rintrojs::introjs(session, options = list(steps = inferenceHelp))
  )

  # Send the node names to the user
  shiny::observe({
    shiny::updateSelectInput(session, "nodeNames", choices = colnames(dat()))
  })

  # Get the selected node measure from the user and print the results
  output$nodeText <- shiny::renderText({
    if (is.null(dat()))
      return(NULL)
    if (input$nodeMeasure == "mb") {
      bnlearn::mb(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "nbr") {
      bnlearn::nbr(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "parents") {
      bnlearn::parents(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "children") {
      bnlearn::children(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "in.degree") {
      bnlearn::in.degree(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "out.degree") {
      bnlearn::out.degree(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "incident.arcs") {
      bnlearn::incident.arcs(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "incoming.arcs") {
      bnlearn::incoming.arcs(dag(), input$nodeNames)
    } else if (input$nodeMeasure == "outgoing.arcs") {
      bnlearn::outgoing.arcs(dag(), input$nodeNames)
    } else
      bnlearn::incident.arcs(dag(), input$nodeNames)
  })

  # Get the selected network measure from the user and plot the results
  output$netTable <- d3heatmap::renderD3heatmap({
    if (is.null(dat()))
      return(NULL)

    # Plot a d3 heatmap of the adjacency matrix
    d3heatmap::d3heatmap(
      bnlearn::amat(dag()),
      dendrogram = input$dendrogram,
      symm = TRUE,
      cexRow = 0.7,
      cexCol = 0.7,
      colors = "Blues"
    )
  })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$measuresIntro,
                      rintrojs::introjs(session, options = list(steps = measuresHelp))
  )

  # Knit shinyAce editor code
  output$knitr <- shiny::renderUI({

    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Building report...", value = 0)

    input$eval
    return(
      shiny::isolate(
        shiny::HTML(
          knitr::knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE)
        )
      )
    )
  })

  # Observe intro btn and start the intro
  shiny::observeEvent(input$editorIntro,
                      rintrojs::introjs(session, options = list(steps = editorHelp))
  )

})

