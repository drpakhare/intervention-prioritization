# INTEGRATED PRIORITIZATION TOOL - VERSION 2.7 (Stable Release)
# A modular Shiny app combining the Impact-Effort Grid and RE-AIM Analysis.

# -----------------------------------------------------------------------------
# 1. LOAD PACKAGES
# -----------------------------------------------------------------------------
# --- Installation ---
# The 'ggradar' package is not on CRAN and must be installed from GitHub.
# Run the following two lines in your R console one time to install it:
#
# install.packages("remotes")
# remotes::install_github("ricardo-bion/ggradar", dependencies = TRUE)
#
# Ensure these other packages are also installed:
# install.packages(c("shiny", "ggplot2", "plotly", "DT", "rmarkdown", "shinycssloaders", "shinyjs", "dplyr", "tidyr", "scales"))

library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(rmarkdown)
library(shinycssloaders)
library(shinyjs)
library(dplyr)
library(tidyr)
library(ggradar)
library(scales)


# -----------------------------------------------------------------------------
# 2. DEFINE UI (USER INTERFACE)
# -----------------------------------------------------------------------------
ui <- fluidPage(
  
  # --- App Title and Styling ---
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {font-family: sans-serif;}
      .shiny-output-error-validation { color: #c0392b; }
      .quadrant-label { font-size: 16px; font-weight: bold; color: #7f8c8d; }
      .nav-tabs>li>a { font-weight: bold; }
      details { border: 1px solid #ddd; border-radius: 4px; padding: 0.5em 0.5em 0; margin-top: 1em;}
      summary { font-weight: bold; margin: -0.5em -0.5em 0; padding: 0.5em; cursor: pointer;}
      details[open] { padding: 0.5em; }
      details[open] summary { border-bottom: 1px solid #ddd; margin-bottom: 10px; }
      .section-box { border: 1px solid #eee; border-radius: 5px; padding: 15px; margin-top: 10px; }
      .total-weight-error { color: #c0392b; font-weight: bold; }
      .total-weight-ok { color: #27ae60; font-weight: bold; }
    "))
  ),
  
  titlePanel("Tool for Intervention Prioritization"),
  
  # --- Layout ---
  sidebarLayout(
    
    # --- Sidebar for Inputs ---
    sidebarPanel(
      width = 3,
      
      h4("1. Manage Interventions"),
      textInput("name", "Intervention Name:", placeholder = "e.g., Peer Support Groups"),
      textInput("short_name", "Short Name (for plots):", placeholder = "e.g., PSG"),
      actionButton("add", "Add Intervention", class = "btn-primary", icon = icon("plus"), width = "100%"),
      br(),br(),
      selectInput("item_to_manage", "Select Intervention to Manage:", choices = NULL),
      uiOutput("remove_item_ui"),
      hr(),
      
      h4("2. Score Selected Item"),
      p("Use the tabs in the main panel to score the selected item."),
      hr(),
      
      h4("3. Help & Export"),
      actionButton("show_guide", "How to Use (Quick)", icon = icon("info-circle"), width = "100%"),
      br(),br(),
      downloadButton("download_report", "Download Report (HTML)", class="btn-success", style="width:100%"),
      hr(),
      actionButton("reset", "Reset All", class = "btn-danger", icon = icon("refresh"), width = "100%"),
      
      tags$details(
        tags$summary("Project & Grid Setup"),
        textInput("project_title", "Project Title:", value = "Intervention Prioritization Matrix"),
        hr(),
        h5("Grid Customization"),
        textInput("x_axis_label", "X-Axis Label:", value = "Effort"),
        textInput("y_axis_label", "Y-Axis Label:", value = "Influence"),
        textInput("size_axis_label", "Size Label:", value = "Scalability"),
        textInput("q_top_left", "Top-Left:", value = "Quick Wins"),
        textInput("q_top_right", "Top-Right:", value = "Major Projects"),
        textInput("q_bottom_left", "Bottom-Left:", value = "Fill-ins"),
        textInput("q_bottom_right", "Bottom-Right:", value = "Thankless Tasks")
      )
    ),
    
    # --- Main Panel for Outputs ---
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_module_tabs",
        tabPanel("Impact-Effort Grid", 
                 div(class="section-box",
                     h4("Grid Visualization"),
                     withSpinner(plotlyOutput("grid_plot", height = "600px"))
                 ),
                 uiOutput("grid_scoring_ui")
        ),
        tabPanel("RE-AIM Analysis",
                 tabsetPanel(
                   id = "reaim_subtabs",
                   tabPanel("Qualitative (Radar)",
                            div(class="section-box",
                                h4("Radar Chart"),
                                withSpinner(plotOutput("reaim_radar_plot", height = "500px"))
                            )
                   ),
                   tabPanel("Quantitative (Weighted)",
                            div(class="section-box",
                                h4("Set Weights for RE-AIM Dimensions"),
                                fluidRow(
                                  column(4, sliderInput("weight_reach", "Reach", min=0, max=100, value=20, step=5, post="%")),
                                  column(4, sliderInput("weight_effectiveness", "Effectiveness", min=0, max=100, value=20, step=5, post="%")),
                                  column(4, sliderInput("weight_adoption", "Adoption", min=0, max=100, value=20, step=5, post="%"))
                                ),
                                fluidRow(
                                  column(4, sliderInput("weight_implementation", "Implementation", min=0, max=100, value=20, step=5, post="%")),
                                  column(4, sliderInput("weight_maintenance", "Maintenance", min=0, max=100, value=20, step=5, post="%"))
                                ),
                                h5("Total Weight:"),
                                uiOutput("total_weight_ui")
                            ),
                            uiOutput("weighted_results_ui")
                   )
                 ),
                 uiOutput("reaim_scoring_ui")
        ),
        tabPanel("Master Data View",
                 div(class="section-box",
                     h4("All Item Data"),
                     withSpinner(DTOutput("master_table"))
                 )
        ),
        tabPanel("About & License",
                 div(class="section-box",
                     h4("About This Tool"),
                     p("The Tool for Intervention Prioritization is a web-based application designed to help public health professionals, researchers, and community stakeholders collaboratively prioritize interventions. It integrates two well-established frameworks: the Impact-Effort Matrix for rapid, qualitative assessment, and the RE-AIM framework for a more detailed, multi-dimensional evaluation."),
                     p("This tool was developed to facilitate structured, transparent, and evidence-informed decision-making during consultative meetings."),
                     hr(),
                     h4("Developers"),
                     p("This tool was developed by the STEPS-INDIA Collaborators."),
                     hr(),
                     h4("Resources"),
                     p(tags$a(href="https://github.com/drpakhare/intervention-prioritization", "View the source code on GitHub", target="_blank")),
                     p(tags$a(href="https://esteps.shinyapps.io/prioritization/", "View the live application on shinyapps.io", target="_blank")),
                     p(tags$a(href="https://docs.google.com/document/d/1HV-rlmhcUOQL4u8K2Dau4yvOnC15-suCJoKUxZlHER8/edit?usp=sharing", "View the full user guide", target="_blank")),
                     hr(),
                     h4("License"),
                     p("This software is open-source and is made available under the MIT License."),
                     tags$pre(
                       "Copyright (c) 2024 STEPS-INDIA Collaborators

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the \"Software\"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE."
                     )
                 )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 3. DEFINE SERVER (LOGIC)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- 3.1. Reactive Values for Data Storage ---
  rv <- reactiveValues(
    master_data = data.frame(
      Name = character(), ShortName = character(), 
      XValue = numeric(), YValue = numeric(), SizeValue = numeric(), Quadrant = character(),
      Reach = numeric(), Effectiveness = numeric(), Adoption = numeric(), Implementation = numeric(), Maintenance = numeric(), 
      stringsAsFactors = FALSE
    )
  )
  
  # --- 3.2. Item Management Logic ---
  observeEvent(input$add, {
    req(input$name, input$short_name)
    new_name <- isolate(input$name)
    
    if (new_name != "" && !new_name %in% rv$master_data$Name) {
      new_row <- data.frame(
        Name = new_name, ShortName = input$short_name,
        XValue = 5, YValue = 5, SizeValue = 5, Quadrant = isolate(input$q_bottom_left),
        Reach = 5, Effectiveness = 5, Adoption = 5, Implementation = 5, Maintenance = 5,
        stringsAsFactors = FALSE
      )
      rv$master_data <- rbind(rv$master_data, new_row)
      updateSelectInput(session, "item_to_manage", choices = rv$master_data$Name, selected = new_name)
      updateTextInput(session, "name", value = ""); updateTextInput(session, "short_name", value = "")
    } else {
      showNotification("Item name cannot be empty or a duplicate.", type = "warning")
    }
  })
  
  output$remove_item_ui <- renderUI({
    req(input$item_to_manage)
    actionButton("remove", "Remove Selected Item", class = "btn-warning", icon = icon("minus"), width = "100%")
  })
  
  observeEvent(input$remove, {
    req(input$item_to_manage)
    item_to_remove <- isolate(input$item_to_manage)
    rv$master_data <- rv$master_data %>% filter(Name != item_to_remove)
    remaining_choices <- rv$master_data$Name
    updateSelectInput(session, "item_to_manage", choices = remaining_choices, selected = if(length(remaining_choices)>0) remaining_choices[1] else NULL)
  })
  
  observeEvent(input$reset, {
    rv$master_data <- data.frame(Name=character(), ShortName=character(), XValue=numeric(), YValue=numeric(), SizeValue=numeric(), Quadrant=character(), Reach=numeric(), Effectiveness=numeric(), Adoption=numeric(), Implementation=numeric(), Maintenance=numeric(), stringsAsFactors=FALSE)
    updateSelectInput(session, "item_to_manage", choices = character(0))
  })
  
  # --- 3.3. 'How to Use' Guide ---
  observeEvent(input$show_guide, {
    showModal(modalDialog(
      title = "How to Use the Integrated Prioritization Tool",
      p("This tool integrates two prioritization methods. Add an item once in the sidebar, and it becomes available in all modules for scoring and analysis."),
      hr(),
      tabsetPanel(
        tabPanel("General Workflow",
                 h4("Step 1: Add and Manage Items"),
                 tags$ol(
                   tags$li("In the sidebar, enter a full 'Item Name' (e.g., 'Community Health Worker Training') and a 'Short Name' for plots (e.g., 'CHW')."),
                   tags$li("Click 'Add Item'. The item is now available across all modules with default scores."),
                   tags$li("Use the 'Select Item to Manage' dropdown to choose which item you want to score or remove."),
                   tags$li("Click 'Remove Selected Item' to delete an item from the analysis.")
                 ),
                 h4("Step 2: Score the Selected Item"),
                 p("Navigate to either the 'Impact-Effort Grid' or 'RE-AIM Analysis' tab. The scoring controls for the currently selected item will appear at the bottom of the tab."),
                 h4("Step 3: Analyze and Export"),
                 p("Use the visualizations in each tab to analyze your results. When finished, click 'Download' to get a complete HTML report.")
        ),
        tabPanel("Impact-Effort Grid", 
                 h4("What it is:"), p("A quick, qualitative method to categorize items based on two key dimensions, such as the effort required versus their potential impact. It helps identify quick wins, major projects, and tasks to avoid."),
                 h4("How to use:"), tags$ol(
                   tags$li("Select an item in the sidebar."),
                   tags$li("In this tab, use the sliders below the plot to score the item. The default axes are 'Effort' and 'Influence', but you can customize these in the 'Project & Grid Setup' section in the sidebar."),
                   tags$li("Click 'Save Grid Scores' to update the plot."),
                   tags$li("The plot shows where your item falls. Items in the top-left are typically the highest priority (high impact, low effort).")
                 )),
        tabPanel("RE-AIM Analysis", 
                 h4("What it is:"), p("A detailed framework to evaluate public health interventions on five key dimensions to assess their real-world potential."),
                 tags$ul(
                   tags$li(tags$strong("Reach:"), "Who and what proportion of the target population will be affected?"),
                   tags$li(tags$strong("Effectiveness:"), "How well does the intervention work? What are the potential negative outcomes?"),
                   tags$li(tags$strong("Adoption:"), "What is the likelihood that organizations and staff will adopt this intervention?"),
                   tags$li(tags$strong("Implementation:"), "Can the intervention be delivered with fidelity? What are the costs?"),
                   tags$li(tags$strong("Maintenance:"), "Can the intervention be sustained over the long term?")
                 ),
                 h4("How to use:"), tags$ol(
                   tags$li("Select an item in the sidebar."),
                   tags$li("Use the sliders below the plots to score the item on all five RE-AIM dimensions."),
                   tags$li("Click 'Save RE-AIM Scores'."),
                   tags$li("Navigate to the 'Qualitative (Radar)' sub-tab to visually compare the strengths and weaknesses of all items."),
                   tags$li("Navigate to the 'Quantitative (Weighted)' sub-tab to assign importance (weights) to each dimension and calculate a final, ranked score for prioritization.")
                 ))
      ),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  output$project_title_header <- renderText({ req(input$project_title); input$project_title })
  
  # --- 3.4. Impact-Effort Grid Module ---
  output$grid_scoring_ui <- renderUI({
    req(input$item_to_manage)
    item_scores <- rv$master_data %>% filter(Name == input$item_to_manage)
    if(nrow(item_scores) == 0) return(p("Add an item to begin.", style="color:grey;"))
    
    div(class="section-box",
        h4(paste("Scoring:", input$item_to_manage)),
        fluidRow(
          column(4, sliderInput("grid_x", isolate(input$x_axis_label), min=0, max=10, value=item_scores$XValue, step=1)),
          column(4, sliderInput("grid_y", isolate(input$y_axis_label), min=0, max=10, value=item_scores$YValue, step=1)),
          column(4, sliderInput("grid_size", isolate(input$size_axis_label), min=1, max=10, value=item_scores$SizeValue, step=1))
        ),
        actionButton("save_grid_scores", "Save Grid Scores", icon=icon("save"), class="btn-success")
    )
  })
  
  observeEvent(input$save_grid_scores, {
    req(input$item_to_manage)
    quadrant <- if (input$grid_x <= 5 && input$grid_y > 5) { req(input$q_top_left); input$q_top_left } else if (input$grid_x > 5 && input$grid_y > 5) { req(input$q_top_right); input$q_top_right } else if (input$grid_x <= 5 && input$grid_y <= 5) { req(input$q_bottom_left); input$q_bottom_left } else { req(input$q_bottom_right); input$q_bottom_right }
    
    rv$master_data <- rv$master_data %>% mutate(
      XValue = ifelse(Name == input$item_to_manage, input$grid_x, XValue),
      YValue = ifelse(Name == input$item_to_manage, input$grid_y, YValue),
      SizeValue = ifelse(Name == input$item_to_manage, input$grid_size, SizeValue),
      Quadrant = ifelse(Name == input$item_to_manage, quadrant, Quadrant)
    )
    showNotification(paste("Grid scores saved for", input$item_to_manage), type="message")
  })
  
  output$grid_plot <- renderPlotly({
    df <- rv$master_data; req(nrow(df) > 0)
    quadrant_names <- c(input$q_top_left, input$q_top_right, input$q_bottom_left, input$q_bottom_right)
    quadrant_colors <- c("#27ae60", "#2980b9", "#f39c12", "#c0392b"); names(quadrant_colors) <- quadrant_names
    
    p <- ggplot(df, aes(x = XValue, y = YValue, size = SizeValue, color = Quadrant, text = paste("<b>Name:</b>", Name, "<br><b>", input$x_axis_label, ":</b> ", XValue, "<br><b>", input$y_axis_label, ":</b> ", YValue, "<br><b>", input$size_axis_label, ":</b> ", SizeValue))) +
      geom_vline(xintercept = 5, linetype = "dashed", color = "grey") + geom_hline(yintercept = 5, linetype = "dashed", color = "grey") +
      annotate("text", x = 2.5, y = 9.5, label = input$q_top_left, class = "quadrant-label") + annotate("text", x = 7.5, y = 9.5, label = input$q_top_right, class = "quadrant-label") +
      annotate("text", x = 2.5, y = 0.5, label = input$q_bottom_left, class = "quadrant-label") + annotate("text", x = 7.5, y = 0.5, label = input$q_bottom_right, class = "quadrant-label") +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) + scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
      geom_point(alpha = 0.8) + geom_text(aes(label = ShortName), size = 3.5, vjust = -1.5, color = "black") +
      scale_color_manual(values = quadrant_colors, name = "Quadrant", limits = names(quadrant_colors)) + scale_size_continuous(range = c(5, 15), name=input$size_axis_label) + theme_minimal() +
      labs(title = input$project_title, x = input$x_axis_label, y = input$y_axis_label) +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
    
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  })
  
  # --- 3.5. RE-AIM Analysis Module ---
  output$reaim_scoring_ui <- renderUI({
    req(input$item_to_manage)
    item_scores <- rv$master_data %>% filter(Name == input$item_to_manage)
    if(nrow(item_scores) == 0) return(p("Add an item to begin.", style="color:grey;"))
    
    div(class="section-box",
        h4(paste("Scoring:", input$item_to_manage)),
        fluidRow(
          column(4, sliderInput("reaim_reach", "Reach", min=0, max=10, value=item_scores$Reach, step=1)),
          column(4, sliderInput("reaim_effectiveness", "Effectiveness", min=0, max=10, value=item_scores$Effectiveness, step=1)),
          column(4, sliderInput("reaim_adoption", "Adoption", min=0, max=10, value=item_scores$Adoption, step=1))
        ),
        fluidRow(
          column(4, sliderInput("reaim_implementation", "Implementation", min=0, max=10, value=item_scores$Implementation, step=1)),
          column(4, sliderInput("reaim_maintenance", "Maintenance", min=0, max=10, value=item_scores$Maintenance, step=1))
        ),
        actionButton("save_reaim_scores", "Save RE-AIM Scores", icon = icon("save"), class="btn-success")
    )
  })
  
  observeEvent(input$save_reaim_scores, {
    req(input$item_to_manage)
    rv$master_data <- rv$master_data %>% mutate(
      Reach = ifelse(Name == input$item_to_manage, input$reaim_reach, Reach),
      Effectiveness = ifelse(Name == input$item_to_manage, input$reaim_effectiveness, Effectiveness),
      Adoption = ifelse(Name == input$item_to_manage, input$reaim_adoption, Adoption),
      Implementation = ifelse(Name == input$item_to_manage, input$reaim_implementation, Implementation),
      Maintenance = ifelse(Name == input$item_to_manage, input$reaim_maintenance, Maintenance)
    )
    showNotification(paste("RE-AIM scores saved for", input$item_to_manage), type = "message")
  })
  
  reaim_plot_object <- reactive({
    req(nrow(rv$master_data) > 0)
    plot_data <- rv$master_data %>% select(Name, Reach, Effectiveness, Adoption, Implementation, Maintenance) %>%
      rename(group = Name) %>%
      mutate_at(vars(-group), ~ .x / 10)
    ggradar(plot_data, values.radar = c("0", "5", "10"), grid.min = 0, grid.mid = 0.5, grid.max = 1, legend.position = "bottom")
  })
  
  output$reaim_radar_plot <- renderPlot({ reaim_plot_object() })
  
  total_weight <- reactive({ sum(input$weight_reach, input$weight_effectiveness, input$weight_adoption, input$weight_implementation, input$weight_maintenance) })
  
  output$total_weight_ui <- renderUI({
    total_w <- total_weight()
    if (total_w == 100) { span(class = "total-weight-ok", paste(total_w, "%")) } else { span(class = "total-weight-error", paste(total_w, "% (must be 100%)")) }
  })
  
  weighted_scores_reactive <- reactive({
    req(nrow(rv$master_data) > 0, total_weight() == 100)
    weights <- c(Reach = input$weight_reach/100, Effectiveness = input$weight_effectiveness/100, Adoption = input$weight_adoption/100, Implementation = input$weight_implementation/100, Maintenance = input$weight_maintenance/100)
    rv$master_data %>% rowwise() %>%
      mutate(FinalScore = sum(c_across(Reach:Maintenance) * weights)) %>%
      ungroup() %>% arrange(desc(FinalScore)) %>%
      mutate(Rank = row_number(), FinalScore = round(FinalScore, 2)) %>%
      select(Rank, Name, FinalScore)
  })
  
  output$weighted_results_ui <- renderUI({
    if (total_weight() != 100) { return(div(class="section-box", h4("Please adjust weights to equal 100% to see results."))) }
    tagList(
      div(class="section-box", h4("Ranked Results Chart"), withSpinner(plotOutput("weighted_plot"))),
      div(class="section-box", h4("Ranked Results Table"), withSpinner(DTOutput("weighted_table")))
    )
  })
  
  weighted_plot_object <- reactive({
    req(weighted_scores_reactive())
    df <- weighted_scores_reactive()
    ggplot(df, aes(x = reorder(Name, FinalScore), y = FinalScore, fill = FinalScore)) +
      geom_bar(stat = "identity") + coord_flip() +
      scale_fill_gradient(low = "#56B4E9", high = "#0072B2") +
      labs(y = "Final Weighted Score", x = "Item Name", title = "Weighted RE-AIM Prioritization Results") +
      theme_minimal(base_size = 14) + theme(legend.position = "none")
  })
  
  output$weighted_plot <- renderPlot({ weighted_plot_object() })
  output$weighted_table <- renderDT({ datatable(weighted_scores_reactive(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  
  # --- 3.6. Master Data Table ---
  output$master_table <- renderDT({
    datatable(rv$master_data, options = list(scrollX = TRUE, pageLength=10), rownames = FALSE)
  })
  
  # --- 3.7. Download Handlers ---
  output$download_report <- downloadHandler(
    filename = function() { paste0("Integrated_Analysis_Report_", Sys.Date(), ".html") },
    content = function(file) {
      tryCatch({
        id <- showNotification("Generating report...", duration = NULL, closeButton = FALSE, type = "message")
        on.exit(removeNotification(id))
        
        temp_dir <- tempdir()
        grid_plot_path <- file.path(temp_dir, "grid_plot.png")
        radar_plot_path <- file.path(temp_dir, "radar_plot.png")
        
        # Define static grid plot
        static_grid_plot <- ggplot(rv$master_data, aes(x = XValue, y = YValue, size = SizeValue, color = Quadrant)) +
          geom_vline(xintercept = 5, linetype = "dashed", color = "grey") +
          geom_hline(yintercept = 5, linetype = "dashed", color = "grey") +
          annotate("text", x = 2.5, y = 9.5, label = isolate(input$q_top_left)) +
          annotate("text", x = 7.5, y = 9.5, label = isolate(input$q_top_right)) +
          annotate("text", x = 2.5, y = 0.5, label = isolate(input$q_bottom_left)) +
          annotate("text", x = 7.5, y = 0.5, label = isolate(input$q_bottom_right)) +
          geom_point(alpha = 0.8) +
          geom_text(aes(label = ShortName), color = "black", size = 3.5, vjust = -1.5) +
          scale_color_manual(values = c("#27ae60", "#2980b9", "#f39c12", "#c0392b"), name = "Quadrant") +
          scale_size_continuous(range = c(5, 15), name = isolate(input$size_axis_label)) +
          scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
          scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
          theme_minimal() +
          labs(
            title = isolate(input$project_title),
            x = isolate(input$x_axis_label),
            y = isolate(input$y_axis_label)
          ) +
          theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
        
        ggsave(grid_plot_path, plot = static_grid_plot, width = 8, height = 6, dpi = 150)
        ggsave(radar_plot_path, plot = reaim_plot_object(), width = 8, height = 6, dpi = 150)
        
        params <- list(
          grid_data = rv$master_data %>% select(Name, ShortName, `X-Axis` = XValue, `Y-Axis` = YValue, `Size` = SizeValue, Quadrant),
          reaim_data = rv$master_data %>% select(Name, Reach, Effectiveness, Adoption, Implementation, Maintenance),
          is_weighted = if (!is.null(total_weight())) total_weight() == 100 else FALSE,
          grid_plot_path = grid_plot_path,
          radar_plot_path = radar_plot_path,
          weighted_data = NULL,
          weighted_plot_path = NULL
        )
        
        names(params$grid_data)[3:5] <- c(isolate(input$x_axis_label), isolate(input$y_axis_label), isolate(input$size_axis_label))
        
        if (params$is_weighted) {
          weighted_plot_path <- file.path(temp_dir, "weighted_plot.png")
          ggsave(weighted_plot_path, plot = weighted_plot_object(), width = 8, height = 6, dpi = 150)
          params$weighted_plot_path <- weighted_plot_path
          params$weighted_data <- weighted_scores_reactive()
        }
        
        # Use external Rmd template
        template_path <- "report_template_intelligent.Rmd"
        
        rmarkdown::render(
          input = template_path,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
      }, error = function(e) {
        showNotification(paste("Error generating report:", e$message), type = "error", duration = 10)
      })
    }
  )
}

# -----------------------------------------------------------------------------
# 4. RUN THE APP
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
