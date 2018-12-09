library(shinydashboard)
library(rhandsontable)

source("legaspi_simplex.r")
source("legaspi_ex5.r")
source("legaspi_spline.r")


ui <- dashboardPage(
  dashboardHeader(title = "CMSC 150 Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Polynomial Regression", tabName = "pr", icon = icon("th")),
      menuItem("Quadratic Spline Interpolation", tabName = "qsi", icon = icon("th")),
      menuItem("Simplex", tabName = "splx", icon = icon("th"))
    )
  ),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "pr",
        fileInput('datafile', 'Choose CSV file',
                  accept=c('csv', 'text/comma-separated-values,text/plain')
        ),
        box(
          numericInput("d", "Enter degree of polynomial:", min=1, value = 1),
          numericInput("n", "Enter value of x:", value = 0),
          actionButton("x_val", "Solve for f(x)"),
          verbatimTextOutput("computeEstimate")
        ),
        box(
          title = "Data Points",
          tableOutput("filetable")
        ),
        box(
          title = "Function",
          tableOutput("functionresult")
        ),
        box(
          title = "Estimate",
          tableOutput("estimateresult")
          
        )
      ),
      
      tabItem(tabName = "qsi",
        fileInput('datafile2', 'Choose CSV file',
          accept=c('csv', 'text/comma-separated-values,text/plain')
        ),
        box(
          numericInput("initx", "Enter value of x:", value = 0),
          actionButton("x_val_qsi", "Solve for f(x)"),
          verbatimTextOutput("computeEstimate_qsi"),
          br(),
          actionButton("interval_func", "Show function for interval"),
          verbatimTextOutput("func_estimate_qsi")
        ),
        box(
          title = "Data Points",
          tableOutput("filetable_qsi")
        ),
        box(
          title = "Functions per Interval",
          tableOutput("functionslist")
        ),
        box(
          title = "Estimate",
          tableOutput("estimateresult_qsi")
        ),
        box(
          title = "Interval Function",
          tableOutput("getfuncinterval")
          
        )
      ),
      tabItem(tabName = "splx",
              fluidRow(
                box(
                  column(
                    7,
                    helpText("Shipping costs from plant to warehouse"),
                    actionButton("slv", "Solve"),
                    verbatimTextOutput("performsimplex"),
                    br(),
                    rHandsontableOutput("table2")
                  ),
                  height = 260
                ),
                box(
                  column(
                    7,
                    helpText("Number to ship from plant to warehouse"),
                    rHandsontableOutput("table1")
                  ),
                  height = 260
                ),
                box(
                  title = "Phase One",
                  column(
                    12,
                    numericInput("one_iter", "Enter iteration:", min=1, max=12, value = 1, width = 200),
                    div(style = 'overflow-x: scroll', tableOutput('phaseone'))
                  ),
                  width = 12
                ),
                box(
                  title = "Phase Two",
                  column(
                    12,
                    numericInput("two_iter", "Enter iteration:", min=1, max=1, value = 1, width = 200),
                    div(style = 'overflow-x: scroll', tableOutput('phasetwo'))
                  ),
                  width = 12
                )
              )
      )
    )
  )
    
)


server <- function(input, output) {
  filedata <- reactive({
    infile <- input$datafile
    
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  getYValues <- function(data_list,row){
    col = 2
    y_values = c()
    for(i in 1:row){
      y_values <- c(y_values, data_list[i, col])
    } 
    return(y_values)
  }
  
  getXValues <- function(data_list,row){
    col = 1
    x_values = c()
    for(i in 1:row){
      x_values <- c(x_values, data_list[i, col])
    } 
    return(x_values)
  }
  
  PolyRegression <- function(data_list){
    row = nrow(data_list)
    col = ncol(data_list)
    
    x_values = getXValues(data_list,row)
    y_values = getYValues(data_list, row)
    
    polynomial <- PolynomialRegression(x_values, y_values, input$d)
    func = paste('f <- function(x) { return(' , polynomial$poly , ')}', sep='')
    args <- "x"
    result = list(coefficients =  polynomial$sol_set, func = func, f = eval(parse(text = paste('f <- function(', args, ') { return(' , polynomial$poly , ')}', sep=''))))
    return(result)
  }
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    data_list = filedata()
  })
  
  
  getDegree <- eventReactive(input$d_val, {
    return(input$d)
  })
  
  output$functionresult <- renderTable({
    result = PolyRegression(filedata())
    print(result$func)

  })
  
  
  computeEstimate <- eventReactive(input$x_val, {
    result = PolyRegression(filedata())
    result$f(input$n)
  })
  
  output$estimateresult <- renderTable({
    computeEstimate()
    
  })
  
  
  ####SPLINE
  QSI <- function(data_list2){
    row = nrow(data_list2)
    col = ncol(data_list2)
    
    x_values = getXValues(data_list2,row)
    y_values = getYValues(data_list2, row)
    
    qsi_res <- QuadraticSpline(input$initx, x_values, y_values)
    return(qsi_res)
  }
  
  computeEstimate_qsi <- eventReactive(input$x_val_qsi, {
    res = QSI(filedata2())
    res$func_eval(input$initx)
  })
  
  func_estimate_qsi <- eventReactive(input$interval_func, {
    res = QSI(filedata2())
    res$func_estimate
  })
  
  output$getfuncinterval <- renderTable({
    func_estimate_qsi()
    
  })
  
  output$estimateresult_qsi <- renderTable({
    computeEstimate_qsi()
    
  })
  
  filedata2 <- reactive({
    infile <- input$datafile2
    
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$filetable_qsi <- renderTable({
    data_list2 = filedata2()
  })
  
  output$functionslist <- renderTable({
    result_qsi = QSI(filedata2())
    print(result_qsi$func_list)
  })
  
  
  ####SIMPLEX
  df1 = matrix(c(0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 0
  ),
  nrow = 4,
  ncol = 6,
  dimnames = list(c("Denver", "Phoenix", "Dallas", "Totals" ), c("Total", "California", "Utah", "New Mexico", "Illinois", "New York")),
  byrow = TRUE)
  
  df = matrix(c(310, 10, 8, 6, 5, 4,
                260, 6, 5, 4, 3, 6,
                280, 3, 4, 5, 5, 9,
                0, 0, 0, 0, 0, 0,
                NA, 180, 80, 200, 160, 220
  ),
  nrow = 5,
  ncol = 6,
  dimnames = list(c("Denver", "Phoenix", "Dallas", "Shipping", "Demands" ), c("Supply", "California", "Utah", "New Mexico", "Illinois", "New York")),
  byrow = TRUE)
  
  df2 = data.frame(df)
  datavalues <- reactiveValues(df2=df2)
  
  observeEvent(input$table2$changes$changes,{
    
    xi=input$table2$changes$changes[[1]][[1]] # fetches the row index of the cell where change is made 
    yi=input$table2$changes$changes[[1]][[2]] # fetches the column index of the cell where change is made
    old = input$table2$changes$changes[[1]][[3]] # fecthes the old values of the cell
    new = input$table2$changes$changes[[1]][[4]] # fecthes the new value of the cell
    
    datavalues$df2 <- hot_to_r(input$table2)
    datavalues$df2[xi+1, yi+1] = new
    #return(datavalues$df2)
    #return(list(x=xi+1, y=yi+1, new_val=new))
  })
  
  
  performsimplex <- eventReactive(input$slv,{
    result_simplex = SimplexMethod(datavalues$df2)
    return(result_simplex)
    #result_simplex$phase_one$tableu
  })
  
  
  output$phaseone <- renderTable({
    res = performsimplex()
    res$phase_one$list_phase_one[input$one_iter]
  })
  
  output$phasetwo <- renderTable({
    res = performsimplex()
    res$phase_two$list_phase_two[input$two_iter]
  })
  
  output$table1 <- renderRHandsontable({
    res = performsimplex()
    ctr = 1
    for(row in 1:3){
      for (col in 2:6) {
        df1[row, col] = res$phase_two$values[[ctr]]
        ctr = ctr+1
      }
    }
    
    sum_x = 0
    for(row in 1:3){
      for (col in 2:6) {
        sum_x = sum_x + df1[row, col]
      }
      df1[row, 1] = sum_x
      sum_x =0
    }
    
    sum_y = 0
    for(col in 1:6){
      for (row in 1:3) {
        sum_y = sum_y + df1[row, col]
      }
      df1[4, col] = sum_y
      sum_y =0
    }
    
    rhandsontable(df1, width=1000, height=300)  %>%
      hot_col("California", readOnly = TRUE) %>%
      hot_col("Utah", readOnly = TRUE) %>%
      hot_col("New Mexico", readOnly = TRUE) %>%
      hot_col("Illinois", readOnly = TRUE) %>%
      hot_col("New York", readOnly = TRUE) %>%
      hot_col("Total", readOnly = TRUE)
  })
  
  output$table2 <- renderRHandsontable({
    rhandsontable(datavalues$df2, width=1000, height=300)
  })
  
}

shinyApp(ui, server)