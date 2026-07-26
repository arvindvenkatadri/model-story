# Shiny App for Sierpinski Chaos Game with Animation
# Modified to include controllable animation speed

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sierpinski Gasket via Chaos Game - Animated"),
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      sliderInput("total_points", "Target Total Points:",
        min = 500, max = 50000, value = 10000, step = 500
      ),
      sliderInput("batch_size", "Points per Batch:",
        min = 10, max = 500, value = 50, step = 10
      ),
      sliderInput("delay_ms", "Delay between Batches (ms):",
        min = 1, max = 500, value = 50, step = 10,
        post = " ms"
      ),
      actionButton("start", "Start / Resume Animation", icon = icon("play")),
      actionButton("pause", "Pause", icon = icon("pause")),
      actionButton("reset", "Reset", icon = icon("refresh")),
      hr(),
      p("Watch the Sierpinski triangle emerge gradually as points are added."),
      p("Adjust the delay slider to control animation speed.")
    ),
    mainPanel(
      plotOutput("chaosPlot", height = "650px"),
      verbatimTextOutput("status")
    )
  )
)

server <- function(input, output, session) {
  # Triangle vertices (equilateral)
  vertices <- reactive({
    data.frame(
      x = c(0, 1, 0.5),
      y = c(0, 0, sqrt(3) / 2),
      label = c("A", "B", "C")
    )
  })

  # Reactive values
  rv <- reactiveValues(
    points = NULL,
    current_point = NULL,
    is_running = FALSE,
    timer = NULL
  )

  # Initialize
  reset_game <- function() {
    v <- vertices()
    rv$current_point <- c(runif(1, 0, 1), runif(1, 0, sqrt(3) / 2 * 0.9))
    rv$points <- matrix(rv$current_point, nrow = 1, ncol = 2)
    rv$is_running <- FALSE
  }

  reset_game()

  # Timer for animation
  observe({
    if (is.null(rv$timer)) {
      rv$timer <- reactiveTimer(Inf) # initially off
    }
  })

  # Animation logic
  observeEvent(input$start, {
    rv$is_running <- TRUE
    rv$timer <- reactiveTimer(input$delay_ms)
  })

  observeEvent(input$pause, {
    rv$is_running <- FALSE
  })

  observeEvent(input$reset, {
    reset_game()
    rv$timer <- reactiveTimer(Inf)
  })

  # Main animation loop
  observe({
    if (!rv$is_running) {
      return()
    }

    req(rv$timer())
    rv$timer() # trigger

    current_total <- if (is.null(rv$points)) 0 else nrow(rv$points)
    if (current_total >= input$total_points) {
      rv$is_running <- FALSE
      return()
    }

    # Add a batch of points
    v <- vertices()
    n_to_add <- min(input$batch_size, input$total_points - current_total)

    new_points <- matrix(NA, nrow = n_to_add, ncol = 2)
    pt <- rv$current_point

    for (i in 1:n_to_add) {
      idx <- sample(1:3, 1)
      chosen <- as.numeric(v[idx, c("x", "y")])
      pt <- pt + input$contraction * (chosen - pt) # contraction from UI (add slider if needed)
      new_points[i, ] <- pt
    }

    rv$points <- rbind(rv$points, new_points)
    rv$current_point <- pt
  })

  # Add contraction slider (was missing in previous version)
  output$ui_contraction <- renderUI({
    sliderInput("contraction", "Contraction Factor:",
      min = 0.1, max = 0.9, value = 0.5, step = 0.05
    )
  })

  # Plot
  output$chaosPlot <- renderPlot({
    req(rv$points)
    v <- vertices()
    pts <- as.data.frame(rv$points)
    colnames(pts) <- c("x", "y")

    n_show <- nrow(pts)

    ggplot() +
      geom_point(data = v, aes(x = x, y = y), color = "red", size = 6) +
      geom_text(
        data = v, aes(x = x, y = y, label = label),
        vjust = -1.8, color = "darkred", size = 6, fontface = "bold"
      ) +
      geom_point(
        data = pts, aes(x = x, y = y),
        color = "#0066cc", alpha = 0.7, size = 1.2
      ) +
      coord_equal(xlim = c(-0.1, 1.1), ylim = c(-0.1, 0.95)) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Sierpinski Chaos Game",
        subtitle = paste(n_show, "points | r =", round(input$contraction, 2))
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)
      )
  })

  output$status <- renderText({
    total <- if (is.null(rv$points)) 0 else nrow(rv$points)
    paste0(
      "Points plotted: ", total, " / ", input$total_points,
      "\nAnimation: ", if (rv$is_running) "RUNNING" else "PAUSED",
      "\nDelay: ", input$delay_ms, " ms"
    )
  })

  # Update timer delay when slider changes
  observeEvent(input$delay_ms, {
    if (rv$is_running) {
      rv$timer <- reactiveTimer(input$delay_ms)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
