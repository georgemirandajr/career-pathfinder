# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Work PC uses: rsconnect 0.4.3 

# Initialize the user selections and tooltip (title)
selections <- vector(mode = "character", length = 0)
edgeLabels <- vector(mode = "character", length = 0)
tips <- vector(mode = "character", length = 0)

# Initialize empty data.frames for nodes and edges
nodes <- data.frame(id = integer(), label = character(), title = character(), 
                    shape = character(), icon.face = character(), icon.code = character(), 
                    color = character(), stringsAsFactors = FALSE)

# Initialize edges data
edges <- data.frame(from = numeric(), to = numeric(), length = numeric())

# Load all datasets
load("./data/item_pairs_30.rda")  # Load data  - old files end in rds, e.g., "item_pairs.rds"
load("./data/item_pairs_15.rda")
# load("./data/item_ref.rds")  
load("./data/item_ref_30.rda")
item_ref <- item_ref_30; rm(item_ref_30)

source("./www/getLink.R")
source("./www/make_breaks.R")

shinyServer(function(input, output, session) {
    
    # Navbar ------------------------------------------------------------------
    shinyjs::addClass(id = "navBar", class = "navbar-right")
    
    # DT Options --------------------------------------------------------------
    options(DT.options = list( lengthMenu = c(10, 20),
                               dom = 'tl'
    ))  # table and lengthMenu options
    
    # Intro JS ----------------------------------------------------------------
    observeEvent(input$help,
                 introjs(session, options = list("nextLabel"="Next",
                                                 "prevLabel"="Back",
                                                 "skipLabel"="Exit"))
    )
    
    # Select dataset -----------------------------------------------------------
    item_pairs <- reactive({
        switch(input$selectData,
               "30 Years" = item_pairs_30,
               "15 Years" = item_pairs_15)
    })
    
    # Initialize a variable to count how many times "btn1" is clicked.
    values <- reactiveValues(data = 1) 
    
    # Btn1 ---------------------------------------------------------------------
    # React to "btn1" being pressed by adding 1 to the values$data variable
    observeEvent( input$btn1, {
        if ( input$item_name == "" ) {
            showModal(modalDialog(title = "Pick a starting job first.",
                                  "It looks like you forgot to select a starting job. Please select a job from the drop-down
                                  menu to begin your career path.",
                                  easyClose = FALSE, size = "s" ))
        } else { 
            values$data = values$data + 1 }
        
    })
    
    # Go Back Button -----------------------------------------------------------
    
    observeEvent( input$goBack, {
        
        if (values$data <= 5) {
            enable("btn1")
        }
        
        if( values$data == 5 & !is.null(input$select5_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 4 & !is.null(input$select4_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 3 & !is.null(input$select3_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else if ( values$data == 2 & !is.null(input$select2_rows_selected) ) {
            showModal(
                modalDialog("Please remove your selection before going back.", size = "s",
                            title = "Oops!",
                            footer = modalButton(label = "", icon = icon("close")))
            )
        } else {
            values$data = values$data - 1
        }
    })
    
    # Disable btn1 when step 5 is reached
    useShinyjs()
    observeEvent( input$btn1, {
        if( values$data == 5 )
            shinyjs::disable("btn1")
    })
    
    # Disable goBack button at start of session
    observe( 
        if(values$data == 1){
            disable("goBack")
        } else {
            enable("goBack")    
        }
    )
    
    # Show/Hide Settings -----------------------------------------------------------------
    # Hide settings at start of new Shiny session
    observe(c(hide("selectData"),
              hide("changeAvatar"),
              hide("userName"),
              hide("download")
    ))
    
    # Toggle visibility of settings
    observeEvent(input$settings, {
        shinyjs::toggle("selectData", anim = TRUE)  # toggle is a shinyjs function
        shinyjs::toggle("changeAvatar", anim = TRUE)
        shinyjs::toggle("userName", anim = TRUE)
    })
    
    # Determine which 'select' options to display (Input choices)
    output$btns <- renderUI({
        if (values$data == 0) {
            return()
        } else if (values$data == 1) {
            uiOutput("select1")
        } else if (values$data == 2) {
            dataTableOutput("select2")
        } else if (values$data == 3) {
            dataTableOutput("select3")
        } else if (values$data == 4) {
            dataTableOutput("select4")
        } else if (values$data >= 5) {
            dataTableOutput("select5")
        } 
    })
    
    # Reset Button -------------------------------------------------------------
    # useShinyjs()
    # observeEvent( input$resetBtn, {
    #     values$data = 1
    #     shinyjs::enable("btn1")
    #     selections <- vector(mode = "character", length = 0)
    # })
    
    # Search NeoGov ------------------------------------------------------------
    link <- eventReactive(input$searchTerm, {
        
        addr <- getLink(input$searchTerm)  # create a hyperlink based on the text input
        
        paste0( "window.open('", addr, "', '_blank')" )  # this code opens the link in a separate window
    })
    
    output$searchNeo <- renderUI({
        actionButton("srchNeo", 
                     label = "Search", 
                     onclick = link(),
                     icon = icon("external-link"))  # when clicked, the link() code executes
    })
    
    # Start Button -------------------------------------------------------------
    observeEvent(input$startBtn, {
        updateNavbarPage(session, "navBar",
                         selected = "careerPF"
        )
    })
    
    # Select Input (First Job) -------------------------------------------------
    output$select1 <- renderUI({
        selectizeInput("item_name", label = "",
                       choices = item_ref$TitleLong,
                       width = "100%",
                       options = list(
                           placeholder = 'Start your path by choosing from one of our jobs.',
                           onInitialize = I('function() { this.setValue(""); }'))
        )
    })
    
    # Table Inputs (Next 2-5 Selections) ---------------------------------------
    
    # Table 1 (Step 2)
    # eventReactive( input$item_name,
    top1 <- reactive({
        
        top <- dplyr::filter(item_pairs(), Item1Name == input$item_name) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
    })
    
    output$select2 <- DT::renderDataTable({
        datatable( top1(), escape = FALSE,
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
                   ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    # outputOptions(output, "select2", suspendWhenHidden = FALSE)
    
    proxy1 = dataTableProxy('select2')
    
    # observeEvent(input$goBack, {
    #     proxy1 <- proxy1 %>% selectRows(NULL)
    #     # values$data <- values$data - 1
    # })
    
    # Table 2 (Step 3)
    # eventReactive( input$select2_cell_clicked, 
    top2 <- reactive({
        
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select3 <- DT::renderDataTable({
        datatable( top2(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
                   ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    # outputOptions(output, "select3", suspendWhenHidden = FALSE)
    
    proxy2 = dataTableProxy('select3')
    
    # observeEvent(input$goBack, {
    #     proxy2 %>% selectRows(NULL)
    #     # values$data <- values$data - 1
    # })
    
    # Table 3 (Step 4)
    top3 <- reactive({
        
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select4 <- DT::renderDataTable({
        datatable( top3(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
                   ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    # outputOptions(output, "select4", suspendWhenHidden = FALSE)
    
    proxy3 = dataTableProxy('select4')
    
    # observeEvent(input$goBack, {
    #     proxy3 %>% selectRows(NULL)
    #     # values$data <- values$data - 1
    # })
    
    # Table 4 (Step 5)
    top4 <- reactive({
        
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        
        top <- dplyr::filter(item_pairs(), Item1Name == itemName) %>%
            select(Item2Name, Item2, Prob, Salary2Min, SalaryDiff, Incumbents, Hyperlink)
        top
    })
    
    output$select5 <- DT::renderDataTable({
        datatable( top4(), escape = FALSE, 
                   extensions = 'Responsive',
                   selection = list(mode = 'single', target = 'row'),
                   colnames = c("Title", "Job Code", "Popularity %", "Starting Salary", "Max Salary Difference", "Incumbents", "Job Description"),
                   rownames = FALSE, style = "bootstrap", 
                   callback = JS("
                                 var tips = ['Classification Title', 'Title Code', 'Percent of employees that moved into that job from your last selected job',
                                 'Starting salary', 'Difference between the highest possible salaries for the selected jobs',
                                 'Number of employees currently holding the title', 'Link to requirements and description'],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")
                   ) %>%
            formatCurrency('SalaryDiff') %>% 
            formatPercentage('Prob', 1) %>%
            formatCurrency("Salary2Min")
    })
    
    # outputOptions(output, "select5", suspendWhenHidden = FALSE)
    
    proxy4 = dataTableProxy('select5')
    
    # observeEvent(input$goBack, {
    #     proxy4 <- proxy4 %>% selectRows(NULL)
    #     # values$data <- values$data - 1
    # })
    
    # User name ----------------------------------------------------------------
    plotTitle <- reactive({
        
        if(input$userName == "") {
            paste("Your Career Path")
        } else {
            paste(input$userName, "'s Career Path", sep = "")
        }
        
    })
    
    
    output$displayName <- renderUI({
        tags$h4( plotTitle() )
        
    })
    
    # Show the current step -------------------
    output$stepNo <- renderUI({
        if(values$data == 1) {
            tags$h4("Step 1:")
        } else if (values$data == 2) {
            # tags$h4("Step 2:")
            div(tags$h4("Step 2:"), div(tags$h6("Choose from one of the jobs in the table below")))
        } else if (values$data == 3) {
            tags$h4("Step 3:")
        } else if (values$data == 4) {
            tags$h4("Step 4:")
        } else if (values$data >= 5) {
            tags$h4("Step 5:")
        }
        
    })
    
    # Get selection data for printing, etc. -----------------------------------
    
    job_1_data <- reactive({
        # Obtain stats
        itemNo <- item_ref[ item_ref$TitleLong == input$item_name, "TitleCode"]
        salaryMin <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMin"]
        salaryMax <- item_ref[ item_ref$TitleLong == input$item_name, "SalaryMax"]
        incumb <- item_ref[ item_ref$TitleLong == input$item_name, "Incumbents"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        v <- c(input$item_name, itemNo, salaryMin, salaryMax, incumb)
        
        v
        
    })
    
    # Print each selection to a panel in sidebar
    output$printInput1 <- renderUI({
        # Display if item is selected
        if(input$item_name == ""){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "one.svg", width = "25px", height = "25px"), tags$h6( paste0(input$item_name, " (", job_1_data()[2], ")") ),
                        paste0( job_1_data()[3], " - ", job_1_data()[4], " /month"), 
                        div(paste0(job_1_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    # Create label for output report
    label_1 <- reactive({
        
        lab <- paste0( input$item_name, "\n",
                       job_1_data()[3], " - ", job_1_data()[4], " Monthly",
                       " | ", job_1_data()[5], " Incumbents")
        
        lab
    })
    
    job_2_data <- reactive({
        # Obtain stats
        itemName <- top1()[ input$select2_rows_selected,  "Item2Name"]
        itemNo <- top1()[ input$select2_rows_selected,  "Item2"]
        salaryMin <- top1()[ input$select2_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top1()[ input$select2_rows_selected,  "Incumbents"]
        prob <- top1()[ input$select2_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput2 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select2_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "two.svg", width = "25px", height = "25px"), tags$h6( paste0(job_2_data()[1], " (", job_2_data()[2], ")") ),
                        paste0( job_2_data()[3], " - ", job_2_data()[4], " /month"), 
                        div(paste0(job_2_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_2 <- reactive({
        
        try(
            paste0( job_2_data()[1], "\n",
                    job_2_data()[3], " - ", job_2_data()[4], " Monthly", "\n",
                    job_2_data()[6], " Popularity", " | ", job_2_data()[5], " Incumbents"),
            
            TRUE
        )
        
    })
    
    job_3_data <- reactive({
        # Obtain stats
        itemName <- top2()[ input$select3_rows_selected,  "Item2Name"]
        itemNo <- top2()[ input$select3_rows_selected,  "Item2"]
        salaryMin <- top2()[ input$select3_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top2()[ input$select3_rows_selected,  "Incumbents"]
        prob <- top2()[ input$select3_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput3 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select3_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "three.svg", width = "25px", height = "25px"), tags$h6( paste0(job_3_data()[1], " (", job_3_data()[2], ")") ),
                        paste0( job_3_data()[3], " - ", job_3_data()[4], " /month"), 
                        div(paste0(job_3_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_3 <- reactive({
        
        try(
            paste0( job_3_data()[1], "\n",
                    job_3_data()[3], " - ", job_3_data()[4], " Monthly", "\n",
                    job_3_data()[6], " Popularity", " | ", job_3_data()[5], " Incumbents"),
            TRUE
        )
        
    })
    
    job_4_data <- reactive({
        # Obtain stats
        itemName <- top3()[ input$select4_rows_selected,  "Item2Name"]
        itemNo <- top3()[ input$select4_rows_selected,  "Item2"]
        salaryMin <- top3()[ input$select4_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top3()[ input$select4_rows_selected,  "Incumbents"]
        prob <- top3()[ input$select4_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput4 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select4_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "four.svg", width = "25px", height = "25px"), tags$h6( paste0(job_4_data()[1], " (", job_4_data()[2], ")") ),
                        paste0( job_4_data()[3], " - ", job_4_data()[4], " /month"), 
                        div(paste0(job_4_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_4 <- reactive({
        try(
            paste0( job_4_data()[1], "\n",
                    job_4_data()[3], " - ", job_4_data()[4], " Monthly", "\n",
                    job_4_data()[6], " Popularity", " | ", job_4_data()[5], " Incumbents"),
            TRUE
        )
        
    })
    
    job_5_data <- reactive({
        # Obtain stats
        itemName <- top4()[ input$select5_rows_selected,  "Item2Name"]
        itemNo <- top4()[ input$select5_rows_selected,  "Item2"]
        salaryMin <- top4()[ input$select5_rows_selected,  "Salary2Min"] 
        salaryMax <- item_ref[ which( itemName == item_ref$TitleLong ), "SalaryMax" ]
        incumb <- top4()[ input$select5_rows_selected,  "Incumbents"]
        prob <- top4()[ input$select5_rows_selected,  "Prob"]
        
        salaryMax <- format(salaryMax, big.mark = ",")
        salaryMax <- paste0("$", salaryMax)
        
        salaryMin <- format(salaryMin, big.mark = ",")
        salaryMin <- paste0("$", salaryMin)
        
        prob <- paste0( round( prob*100, 1 ), "%" )
        
        v <- c(itemName, itemNo, salaryMin, salaryMax, incumb, prob)
        
        v
    })
    
    output$printInput5 <- renderUI({
        
        # Display if item is selected
        if( is.null(input$select5_rows_selected) ){
            return()
        } else {
            div(class="panel panel-default",
                div(class="panel-body",
                    div(tags$img(src = "five.svg", width = "25px", height = "25px"), tags$h6( paste0(job_5_data()[1], " (", job_5_data()[2], ")") ),
                        paste0( job_5_data()[3], " - ", job_5_data()[4], " /month"), 
                        div(paste0(job_5_data()[5], " incumbents"))
                    )
                ))
        }
    })
    
    label_5 <- reactive({
        
        try(
            paste0( job_5_data()[1], "\n",
                    job_5_data()[3], " - ", job_5_data()[4], " Monthly", "\n",
                    job_5_data()[6], " Popularity", " | ", job_5_data()[5], " Incumbents"),
            TRUE
        )
    })
    
    # Visualization ------------------------------------------------------------
    
    # Avatar to use in the visualization
    avatar <- reactive({
        switch(input$changeAvatar,
               # "traveler" = "f21d",  # not compatible with new FA
               "map-marker" = "f041",
               "rocket" = "f135",
               # "paper-plane" = "f1d8",  # not compatible with new FA
               "leaf" = "f06c")
    })
    
    colorIcon <- reactive({
        # Automatically change avatar color based on avatar selection
        switch(input$changeAvatar,
               "traveler" = "#0c84e4",      # Blue
               "map-marker" = "#000000",  # Black
               "rocket" = "#f44141",      # Red
               "paper-plane" = "#663096", # Purple  deeper purple --> #663096
               "leaf" = "#10d13a"         # Green
        )
    })
    
    tip1 <- reactive({
        paste0( "<h6>", job_1_data()[1], "</h6>")
        
    })
    
    visNode <- reactive({
        
        item_name1 <- input$item_name  
        item_name2 <- try( top1()[ input$select2_rows_selected,  "Item2Name"], TRUE ) 
        item_name3 <- try( top2()[ input$select3_rows_selected,  "Item2Name"], TRUE ) 
        item_name4 <- try( top3()[ input$select4_rows_selected,  "Item2Name"], TRUE ) 
        item_name5 <- try( top4()[ input$select5_rows_selected,  "Item2Name"], TRUE ) 
        
        # Collect user selections
        selections <- append(selections,
                             c(item_name1, item_name2, item_name3,
                               item_name4, item_name5))
        
        # tips <- append(tips,
        #                c(tip1(), tip2(), tip3(), tip4(), tip5() ))
        
        # Insert line breaks where there's more than 2 words in a title
        selections <- sapply(selections, make_breaks, simplify = "array", USE.NAMES = FALSE)
        
        # Add selections to data.frame
        nodes[1:length(selections),2] <- selections
        
        # # Add tips to data.frame
        # nodes[1:length(tips), 3] <- tips
        
        # Add id
        nodes$id <- 1:length(selections)
        
        # Add icons, which requires defining 3 properties
        nodes$shape <- rep("icon", length(selections))
        nodes$icon.face <- rep('fontAwesome', length(selections))
        nodes$icon.code <- rep(avatar(), length(selections))
        # nodes$color <- rep(colorIcon(), length(selections))  
        # Color is now added via icon options in visNodes()
        
        # Add shadow
        # nodes$shadow <- TRUE
        
        # Keep only the rows that don't have errors
        nodes <- nodes[grep("Error", nodes$label, invert = TRUE),]
        
        # Keep rows that are not NA in Label column
        nodes <- nodes[ !is.na(nodes$label), ]  
        
    })
    
    visEdge <- reactive({
        
        num_selections <- nrow( visNode() )
        
        if ( num_selections > 0)
            for ( i in 1:(num_selections-1) ) {
                edges[i, ] <- c( i, i+1, 200)
            }
        
        edges
    })
    
    # Under Development - Adding popularity percentage to edge label 
    edgeLab <- reactive({
        prob1 <- try( top1()[ input$select2_rows_selected,  "Prob"], TRUE ) 
        prob2 <- try( top2()[ input$select3_rows_selected,  "Prob"], TRUE ) 
        prob3 <- try( top3()[ input$select4_rows_selected,  "Prob"], TRUE ) 
        prob4 <- try( top4()[ input$select5_rows_selected,  "Prob"], TRUE ) 
        
        # Collect user selections
        edgeLabels <- c(prob1, prob2, prob3, prob4)
        
        # Keep only the rows that don't have errors
        edgeLabels <- edgeLabels[grep("Error", edgeLabels, invert = TRUE)]
    })
    
    # Set the seed (layout) for the graph based on number of nodes in graph
    visSeed <- reactive({
        if( nrow(visNode()) == 1 ) {
            1
        } else if ( nrow(visNode()) == 2 ) {
            6
        } else if ( nrow(visNode()) == 3 ) {
            21
        } else if ( nrow(visNode()) == 4 ) {
            30
        } else if ( nrow(visNode()) == 5 ) {
            5432
        }
    })
    
    # Creating the dynamic graph
    output$visTest <- visNetwork::renderVisNetwork({
        
        # The below uses a different random seed to determine layout based on num of nodes
        
        visNetwork::visNetwork(visNode(), visEdge(), height = "275px", width = "100%") %>%
            addFontAwesome() %>%
            visNetwork::visEdges(dashes = TRUE, shadow = TRUE,
                                 arrows = list(to = list(enabled = TRUE, scaleFactor = 2)),
                                 color = list(color = "#587fb4", highlight = "red")) %>%
            visNodes(shadow = list(enabled = TRUE, size = 15),
                     icon = list( color = colorIcon() )) %>%
            visLayout(randomSeed = visSeed() ) %>%
            visPhysics(solver = "barnesHut", stabilization = list(enabled = FALSE))
    })
    
    # Output Report -----------------------------------------------------------
    # Report template is chosen based on the number of job selections made
    template <- reactive({
        if( nrow(visNode()) == 1 ) {
            ""
        } else if ( nrow(visNode()) == 2 ) {
            png::readPNG("./www/pathImage_2.png")
        } else if ( nrow(visNode()) == 3 ) {
            png::readPNG("./www/pathImage_3.png")
        } else if ( nrow(visNode()) == 4 ) {
            png::readPNG("./www/pathImage_4.png")
        } else if ( nrow(visNode()) == 5 ) {
            png::readPNG("./www/pathImage_5.png")
        }
    })
    
    # Determine how many plot labels are needed for the output report
    plot_labels <- reactive({
        if( nrow(visNode()) == 1 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
        } else if ( nrow(visNode()) == 2 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 3 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 4 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
        } else if ( nrow(visNode()) == 5 ) {
            # Display job title 1
            text(25, 105, labels = label_1(), col = "white", pos = 2, cex = 0.75)
            
            # Display job title 2
            text(5.5, 42, labels = label_2(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 3
            text(5.5, 33, labels = label_3(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 4
            text(5.5, 24.5, labels = label_4(), col = "white", pos = 4, cex = 0.75)
            
            # Display job title 5
            text(5.5, 15, labels = label_5(), col = "white", pos = 4, cex = 0.75)
            
        }
    })
    
    # PDF Report
    plotInput <- reactive({
        
        if(input$returnpdf){
            
            pdf("./www/Career_Path_Report.pdf", width=as.numeric(8), height=as.numeric(11))
            plot(cars, type = "n", axes = F, xlab = "", ylab = "")
            lim <- par()
            
            rasterImage( template(), lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4] )
            
            plot_labels()
            
            dev.off()
        }
        
        plot(rnorm(sample(100:200,1)), type = "n", axes = F, xlab = "", ylab = "")  
        # This is a 'decoy' plot that needs to render
        
    })
    
    # This plots the blank 'decoy' plot
    # Progress bar is based on the computation time of this plot
    output$myplot <- renderPlot({ 
        withProgress(message = 'Building your report', {
            
            plotInput()
            
            incProgress(detail = "Putting on the finishing touches...", amount = 0.5)
            
        })
    })
    
    output$pdflink <- downloadHandler(
        filename <- "my-career.pdf",
        content <- function(file) {
            # old file that was copied was my-career-path.pdf
            file.copy("./www/Career_Path_Report.pdf", file)
        })
    
    # Generate the download link for UI only if the checkbox is checked
    output$download <- renderUI({
        if(input$returnpdf == FALSE){
            return()
        } else{
            downloadLink('pdflink')
        }
    })
    
    # Delay showing the download link to buy time to generate the PDF
    observeEvent(input$returnpdf,{
        
        delay(3000, shinyjs::show("download") )
        
    })
    
    # Survey link after the download button is clicked
    # AND reset the checkbox to the download button
    
    onclick("download",
            delay(5000, 
                  c(
                      showModal(
                          modalDialog(
                              title = "Tell us what you think!",
                              "We want to hear your thoughts on this career planning tool. Please take this 2 minute survey and your feedback will enhance this resource for years to come!", 
                              tags$a(href="https://survey.lacounty.gov/LACounty/se.ashx?s=645F266E52CB09F0", 
                                     target = "_blank", 
                                     "Click here for the survey"), 
                              size = "m", 
                              footer = modalButton(label = " ", icon = icon("close") )
                          )
                      ),
                      delay(1000, shinyjs::reset("returnpdf") )
                  )
            )
    )
    
    
    
    
    
})