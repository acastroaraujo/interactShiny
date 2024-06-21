
library(shiny)
library(actsims)
library(actdata)
library(dplyr)
library(tidyr) ## only used in dictionary_reactive()
library(bslib)
library(shinyjs)
library(plotly)
library(DT)
library(histoslider)
#source("utils.R")

available_equations <- actdata::equations |> 
  dplyr::filter(equation_type == "impressionabo") |> 
  dplyr::select(key, group)

available_dictionaries <- purrr::map(actdata::get_dicts(), \(x) x@groups)
names(available_dictionaries) <- purrr::map_chr(actdata::get_dicts(), \(x) x@key)

ok <- purrr::map(actdata::get_dicts(), \(x) x@components) |> 
  purrr::map_lgl(\(x) all(c("identity", "behavior") %in% x))

available_dictionaries <- available_dictionaries[ok]


# Customize, print stuff from Aidan's package!

# sometimes the dots have a weird %{text} bold name. This happens when there's
# only one left.

# Make code that decides which dictionaries are worthy of inclusion—i.e., that 
# they have at least identity and behavior components.

# "Add 3d plot with observers, and add closest terms panel. Use an observer
# so that the plot doesn't restart all the time"

# https://linking.plotly-r.com/linking-views-with-shiny#fig:shiny-scatterplot

# dictionary --------------------------------------------------------------

ui_dictionary <- bslib::card(
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = "25%",
      open = TRUE,
      title = "Dictionary Options",
      bslib::accordion(
        bslib::accordion_panel(
          title = "Filters",
          shiny::uiOutput("dict_filters")
        ),
        bslib::accordion_panel(
          title = "Closest Terms",
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput("e_closest", shiny::helpText("Evaluation"), value = 0, step = 0.05)), 
            shiny::column(4, shiny::numericInput("p_closest", shiny::helpText("Potency"), value = 0, step = 0.05)), 
            shiny::column(4, shiny::numericInput("a_closest", shiny::helpText("Activity"), value = 0, step = 0.05))
          ),
          shiny::numericInput("closest_max_dist", shiny::helpText("Type number for maximum distance"), value = 0.8, min = 0, max = Inf, step = 0.2),
          shiny::uiOutput("closest_components"),
          shiny::verbatimTextOutput("closest_terms")
        )
      )
    ),
    bslib::navset_card_underline(
      bslib::nav_panel("Visual", bslib::card(full_screen = TRUE, plotly::plotlyOutput("epa3D"))),
      bslib::nav_panel("Spreadsheet", bslib::card(full_screen = TRUE, DT::dataTableOutput("spreadsheet")))
    ),
    shiny::uiOutput("source_info", inline = TRUE)
  )
)


# solve -------------------------------------------------------------------

ui_solve <- bslib::layout_columns(
  bslib::card(
    shiny::radioButtons("solve_for", shiny::h5("Solve for:"), choiceNames = purrr::map(c("Actor Identity", "Behavior", "Object Identity"), helpText), choiceValues = c("actor", "behavior", "object"), selected = "behavior", inline = TRUE),
    shiny::selectizeInput("solve_actor", "Actor", choices = NULL),
    shiny::verbatimTextOutput("solve_actor_epa"),
    shiny::selectizeInput("solve_behavior", "Behavior", choices = NULL),
    shiny::verbatimTextOutput("solve_behavior_epa"),
    shiny::selectizeInput("solve_object", "Object", choices = NULL),
    shiny::verbatimTextOutput("solve_object_epa")
  ),
  bslib::card(
    shiny::h5("Solution"),
    shiny::verbatimTextOutput("solution", placeholder = TRUE),
    shiny::h5("Closest Terms"),
    shiny::numericInput("solve_max_dist", shiny::helpText("Type number for maximum distance"), value = 0.8, min = 0, max = Inf, step = 0.2),
    shiny::verbatimTextOutput("solution_closest", placeholder = TRUE)
  )
  
)


# analyze events --------

ui_analyze_events <- bslib::card(
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      open = TRUE,
      width = "25%",
      title = "Setup",
      shiny::actionButton("init_button", "Initialize", icon = shiny::icon("play")),
      shiny::actionButton("init_restart", "Restart", icon = shiny::icon("triangle-exclamation"), disabled = TRUE),
      shiny::uiOutput("init_deflection"),
      shiny::selectizeInput("init_actor", "Person 1 (Actor)", choices = NULL),
      shiny::verbatimTextOutput("init_actor_epa"),
      shiny::selectizeInput("init_behavior", "Behavior", choices = NULL),
      shiny::verbatimTextOutput("init_behavior_epa"),
      shiny::selectizeInput("init_object", "Person 2 (Object)", choices = NULL),
      shiny::verbatimTextOutput("init_object_epa")
    ),
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib::card("Controls", shiny::br(), shiny::helpText("add grey-out optimal behavior and reidentification panels with closest words to optimal in the bottom")),
      bslib::card("Main Output", shiny::br(), shiny::helpText("add history panel with deflection scores, possibility of looking at metadata, and visualization"))
    )
  )
)

# customize ---------------------------------------------------------------

ui_customize <- bslib::card(
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      shiny::selectizeInput("dictionary", "Dictionary", choices = names(available_dictionaries), selected = "usfullsurveyor2015"),
      shiny::selectizeInput("dictionary_subset", "Group", choices = "all"),
      shiny::selectizeInput("equations", "Equations", choices = unique(available_equations$key), selected = "us2010"),
      shiny::selectizeInput("equations_subset", "Group", choices = "all")
    ),
    bslib::card("Info")
  )
)


ui <- bslib::page_navbar(
  shinyjs::useShinyjs(),
  title = "Minimalist Interact",
  
 # theme = bslib::bs_theme() |> 
 #   bs_add_rules(list(sass::sass_file("styles.css"))),
  
  nav_panel(title = "Dictionary", ui_dictionary),
  nav_panel(title = "Solve", ui_solve),
  nav_panel(title = "Analyze Events", ui_analyze_events),
  nav_panel(title = "Impression Change Equation", card(uiOutput("equations_source_info"), verbatimTextOutput("equationsPRINT"))),
  nav_panel(title = "Customize", ui_customize)
  
)

server <- function(input, output, session) {
  
  ## Customize ---------------------------------------------------------------
  
  
  observeEvent(input$dictionary, {
    freezeReactiveValue(input, "dictionary_subset")
    updateSelectizeInput(session, "dictionary_subset", label = "Group", choices = available_dictionaries[[input$dictionary]])
  })
  
  observeEvent(input$equations, {
    freezeReactiveValue(input, "equations_subset")
    i <- with(available_equations, key == input$equations)
    updateSelectizeInput(session, "equations_subset", label = "Group", choices = available_equations$group[i])
  })
  
  ## This is the most important line [!]
  ACT <- reactive({
    interact(dictionary = list(input$dictionary, input$dictionary_subset), equations = list(input$equations, input$equations_subset))
  }) 
  
  identities <- reactive({

    i <- ACT()$dictionary$component == "identity"
    out <- ACT()$dictionary$term[i]
    names(out) <- gsub("_", " ", out)
    return(out)
    
  })
  
  behaviors <- reactive({
    
    i <- ACT()$dictionary$component == "behavior"
    out <- ACT()$dictionary$term[i]
    names(out) <- gsub("_", " ", out)
    return(out)
      
  })

  ## Analyze Events ----------------------------------------------------------

  observeEvent(list(identities(), behaviors()), {
    updateSelectizeInput(session, 'init_actor', choices = identities(), selected = sample(identities(), 1), server = TRUE)
    updateSelectizeInput(session, 'init_behavior', choices = behaviors(), selected = sample(behaviors(), 1), server = TRUE)
    updateSelectizeInput(session, 'init_object', choices = identities(), selected = sample(identities(), 1), server = TRUE)
  })
  
  output$init_deflection <- renderUI({
    req(input$init_actor)
    req(input$init_behavior)
    req(input$init_object)
    
    out <- ACT()$deflection(list(A = input$init_actor, B = input$init_behavior, O = input$init_object))
    msg <- paste0("Deflection: ", round(out$deflection, 3))
    
    helpText(msg, style = "font-size: 16px; font-style: italic;")
    
  })
  
  output$init_actor_epa <- renderPrint({
    req(input$init_actor)
    
    ACT()$fundamentals(input$init_actor) |> 
        dplyr::filter(component == "identity") |> 
        dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
        unlist()
    
  })
  
  output$init_behavior_epa <- renderPrint({
    req(input$init_behavior)
    
    ACT()$fundamentals(input$init_behavior) |> 
      dplyr::filter(component == "behavior") |> 
      dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
      unlist()
    
  })
  
  output$init_object_epa <- renderPrint({
    req(input$init_object)
    
    ACT()$fundamentals(input$init_object) |> 
      dplyr::filter(component == "identity") |> 
      dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
      unlist()
    
  })
  
  observeEvent(input$init_button, {
    shinyjs::disable("init_actor")
    shinyjs::disable("init_behavior")
    shinyjs::disable("init_object")
    updateActionButton(session, "init_button", disabled = TRUE, icon = icon("circle-pause"))
    updateActionButton(session, "init_restart", disabled = FALSE)
  })
  
  observeEvent(input$init_restart, {
    shinyjs::enable("init_actor")
    shinyjs::enable("init_behavior")
    shinyjs::enable("init_object")
    updateActionButton(session, "init_button", disabled = FALSE, icon = icon("play"))
    updateActionButton(session, "init_restart", disabled = TRUE)
  })
  
  # HERE situation ---------- 
  
  situation <- reactive({
    req(input$init_actor)
    req(input$init_behavior)
    req(input$init_object)
    
    actsims::start_situation(
      init_event = list(A = input$init_actor, B = input$init_behavior, O = input$init_object), 
      dictionary = list(input$dictionary, input$dictionary_subset),
      equations = list(input$equations, input$equations_subset)
    )
  })

  
  ## Solve -------------------------------------------------------------------

  observeEvent(list(identities(), behaviors()), {
    updateSelectizeInput(session, 'solve_actor', choices = identities(), selected = sample(identities(), 1), server = TRUE)
    updateSelectizeInput(session, 'solve_behavior', choices = behaviors(), selected = sample(behaviors(), 1), server = TRUE)
    updateSelectizeInput(session, 'solve_object', choices = identities(), selected = sample(identities(), 1), server = TRUE)
  })
  
  output$solve_actor_epa <- renderPrint({
    req(input$solve_actor)
    
    ACT()$fundamentals(input$solve_actor) |> 
      dplyr::filter(component == "identity") |> 
      dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
      unlist()
    
  })
  
  output$solve_behavior_epa <- renderPrint({
    req(input$solve_behavior)
    
    ACT()$fundamentals(input$solve_behavior) |> 
      dplyr::filter(component == "behavior") |> 
      dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
      unlist()
    
  })
  
  output$solve_object_epa <- renderPrint({
    req(input$solve_object)
    
    ACT()$fundamentals(input$solve_object) |> 
      dplyr::filter(component == "identity") |> 
      dplyr::select(dplyr::all_of(c("e", "p", "a"))) |> 
      unlist()
    
  })
  
  
  observeEvent(input$solve_for, {
    
    if (input$solve_for == "actor") {
      shinyjs::hide("solve_actor")
      shinyjs::show("solve_behavior")
      shinyjs::show("solve_object")
      
      shinyjs::hide("solve_actor_epa")
      shinyjs::show("solve_behavior_epa")
      shinyjs::show("solve_object_epa")
    }
    
    if (input$solve_for == "behavior") {
      shinyjs::hide("solve_behavior")
      shinyjs::show("solve_actor")
      shinyjs::show("solve_object")
      
      shinyjs::hide("solve_behavior_epa")
      shinyjs::show("solve_actor_epa")
      shinyjs::show("solve_object_epa")
    }
    
    if (input$solve_for == "object") {
      shinyjs::hide("solve_object")
      shinyjs::show("solve_actor")
      shinyjs::show("solve_behavior")
      
      shinyjs::hide("solve_object_epa")
      shinyjs::show("solve_actor_epa")
      shinyjs::show("solve_behavior_epa")
    }
    
  })
  
  SOLUTION <- reactive({
    
    req(input$solve_actor)
    req(input$solve_behavior)
    req(input$solve_object)
    
    ACT()$max_confirm(
      events = list(A = input$solve_actor, B = input$solve_behavior, O = input$solve_object),
      solve_for = input$solve_for
    )
    
  })
  
  
  output$solution <- renderPrint({
    round(unlist(SOLUTION()), 3)
  })
  
  output$solution_closest <- renderPrint({
    
    what <- switch(input$solve_for,
      "actor" = "identity",
      "behavior" = "behavior",
      "object" = "identity"
    )
    
    out <- ACT()$closest_terms(
      SOLUTION(),
      component = what, 
      max_dist = input$solve_max_dist
    )
    
    data.frame(distance = out)
    
  })
  

  ## Dictionary --------------------------------------------------------------
  
  output$source_info <- renderUI({
    msg <- paste0("Source: ",input$dictionary, " (", input$dictionary_subset, ")")
    helpText(msg, style = "font-size: 14px;")
  })
  
  output$dict_filters <- renderUI({
    req(ACT())
    
    component_choices <- unique(ACT()$dictionary$component)
    epa <- do.call(rbind, ACT()$dictionary$ratings)

    tagList(
      histoslider::input_histoslider("filter_e", label = "Evaluation", values = epa[, "e"], height = 150),
      histoslider::input_histoslider("filter_p", label = "Potency", values = epa[, "p"], height = 150),
      histoslider::input_histoslider("filter_a", label = "Activity", values = epa[, "a"], height = 150),
      checkboxGroupInput("filter_components", label = "Components", choiceNames = purrr::map(component_choices, helpText), choiceValues = component_choices, inline = TRUE, selected = component_choices)
    )
    
  })
  
  dictionary_reactive <- reactive({
    req(input$filter_e)
    req(input$filter_p)
    req(input$filter_a)
    
    ACT()$dictionary |> 
      dplyr::select(term, component, ratings) |> 
      tidyr::unnest_wider(ratings) |> 
      dplyr::mutate(component = as.factor(component)) |> 
      dplyr::filter(
        dplyr::between(e, input$filter_e[[1]], input$filter_e[[2]]),
        dplyr::between(p, input$filter_p[[1]], input$filter_p[[2]]),
        dplyr::between(a, input$filter_a[[1]], input$filter_a[[2]]),
        component %in% input$filter_components
      ) 
    
  })
  
  
  output$epa3D <- plotly::renderPlotly({
    
    p <- plot_ly(
      source = "epa3D",
      data = dictionary_reactive(),
      type = "scatter3d", 
      mode = "markers",
      x = ~e, y = ~a, z = ~p, 
      color = ~component,
      text = ~term,
      hovertemplate = paste(
        "<b>%{text}</b><br><br>",
        "E: %{x} <br>",
        "P: %{z} <br>",
        "A: %{y} <br>",
        "<extra></extra>")
    ) |> 
      layout(scene = list(
        xaxis = list(title = 'Evaluation', range = list(4.3, -4.3)),
        zaxis = list(title = 'Potency', range = list(-4.3, 4.3)),
        yaxis = list(title = 'Activity', range = list(4.3, -4.3))
      )) |> 
      event_register("plotly_click")
    
  })

  
  clickData <- reactive({
    req(event_data("plotly_click", source = "epa3D"))
    event_data("plotly_click", source = "epa3D")
  })
  
  observeEvent(clickData(), {
    
    click <- clickData()
    
    updateNumericInput(session, "e_closest", value = click$x)
    updateNumericInput(session, "p_closest", value = click$z)
    updateNumericInput(session, "a_closest", value = click$y)
    
  })
  
  observeEvent(input$spreadsheet_rows_selected, {
    
    d <- dictionary_reactive()[input$spreadsheet_rows_selected, ]
    
    updateNumericInput(session, "e_closest", value = d$e)
    updateNumericInput(session, "p_closest", value = d$p)
    updateNumericInput(session, "a_closest", value = d$a)
    
    
  })
  
  output$spreadsheet <- DT::renderDataTable({
    
      DT::datatable(
        data = dictionary_reactive(),
        extensions = 'Buttons',
        selection = "single",
        options = list(
          searching = TRUE,
          dom = 'Bfti',
          paging = FALSE,
          scrollY = 500,
          scroller = TRUE,
          buttons = c("copy", "excel", "csv"),
          columnDefs = list(list(width = '20%', targets = 3:5), list(targets = 2:5, searchable = FALSE))
        )
      ) |> 
      DT::formatRound(columns = c("e", "p", "a"), digits = 3)
    
    
  }, server = TRUE)
  
  
  output$closest_components <- renderUI({
    req(ACT())
    component_choices <- unique(ACT()$dictionary$component)
    shiny::radioButtons("closest_components", label = "Components", choiceNames = purrr::map(component_choices, helpText), choiceValues = component_choices, inline = TRUE, selected = component_choices[[1]])
  })
  
  output$closest_terms <- renderPrint({
    
    req(input$closest_components)
    
    epa <- c(input$e_closest, input$p_closest, input$a_closest)
    
    lookup <- dictionary_reactive()[dictionary_reactive()$component == input$closest_components, ]
    fundamentals <- as.matrix(lookup[c("e", "p", "a")])
    rownames(fundamentals) <- lookup$term
    
    ssd <- rowSums(sweep(fundamentals, MARGIN = 2, FUN = "-", unlist(epa))^2)
    i <- which(ssd <= input$closest_max_dist)
    out <- sort(ssd[i])
    
    data.frame(distance = out, row.names = names(out))
    
  })
  
  
  ## Equations ---------------------------------------------------------------
  
  output$equations_source_info <- renderUI({
    msg <- paste0("Source: ",input$equations, " (", input$equations_subset, ")")
    helpText(msg, style = "font-size: 14px;")
  })
  
  
  output$equationsPRINT <- renderPrint({
    req(ACT())
    
    ACT()$equations
    
  })
  
  
}
  

shinyApp(ui, server)

