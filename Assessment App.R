#Load packages----------------------------------------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(reshape2)

#UI--------------------------------------------------------------------------------------------------------------
 ui <- fluidPage(
   theme = "paper",
   titlePanel("R Shiny Assignment"), #add title for app
   sidebarLayout(
     sidebarPanel(
       strong("Inputs"),
       br(), br(),
       
       #file input
       fileInput(
         inputId = "claims_data",
         label = "Choose claims data file"
       ),
       
       #tail factor input
       numericInput(
         inputId = "tail_factor",
         label = "Tail Factor",
         value = 1,
         step = 0.1
       )
       
     ),
     mainPanel(
        #multiple tabs for different outputs
       tabsetPanel(
         type = "tabs",
         
         # tab for table output
         tabPanel(
           title = "Cumulative Paid Claims ($)",
           dataTableOutput(outputId = "projected_table")
         ),
         
         #tab for plot output
         tabPanel(
           title = "Plot",
           plotOutput(outputId="cumulative_paid_claims_plot")
         )
         
        )
      )
    )
  )
 
 # Define server function-----------------------------------------------------------------
 server <- function(input, output, session) {
   
  #Reactives
   #Reading the csv file
   claims_data <- reactive({
     req(input$claims_data)
     #read file
     inputfile <- input$claims_data
     read.csv(inputfile$datapath)
   })
   
   #Renaming the columns and arranging by loss years and development years
   # only works if csv file follows a certain format, ie column 1 = loss year, column 2 = development year, column 3 = claims paid
   # and csv file is in long format
   claims_data_renamed <- reactive({
     column.names <- names(claims_data())
     claims_data() %>%
       rename(LossYear = column.names[1], DevYear = column.names[2],ClaimsPaid =column.names[3]) %>%
       arrange(DevYear) %>%
       arrange(LossYear)
   })
   
   #Putting into the cumulative development triangle
   cumu_development_triangle <- reactive({
     newdata <- claims_data_renamed() #call data from reactive for easy referencing
     #naming the dimensions
     columnname <- unique(newdata$DevYear)
     rowname <- unique(newdata$LossYear) 
     devtri <- as.data.frame(matrix(numeric(0), 
                                    nrow=length(rowname), 
                                    ncol=length(columnname))) #make a data frame for development triangle
     dimnames(devtri) <-list(rowname, columnname)
     #filling in the data frame
     #Obtain the incremental paid claims triangle
     for(i in rowname){
       for(j in columnname){
         if (length(newdata[(newdata$LossYear==i)&(newdata$DevYear==j),"ClaimsPaid"])== 0){ #length()=0 when data not found in csv file 
           devtri[toString(i),toString(j)]<- 0 
         } else{
           devtri[toString(i),toString(j)]<- newdata[(newdata$LossYear==i)&(newdata$DevYear==j),"ClaimsPaid"]
         }
       }
     }
     #Obtain the cumulative paid claims triangle
     for(i in 1:length(devtri)){
       for (j in 2:length(devtri)){ #start from column 2 because column 1 does not require summing up
         if(devtri[i,j]==0){ 
           devtri[i,j] == 0
         } else {
           devtri[i,j] = devtri[i, j-1] + devtri[i,j]
         }
       }
     }
     devtri #converted long format to wide format
   })

   #Calculate weighted average
   weighted_avg <- reactive({
      #call reactives for easier referencing
     devtri <- cumu_development_triangle()
     
     #sum by column (development year)
     sum_by_DevYear <- colSums(devtri)
     
     #sum by column without diagonal
     diagonals <- devtri[row(devtri)+col(devtri)==length(devtri)+1] #for reverse diagonal of square matrix, row()+col() = ncol()/nrow() + 1
     sum_without_diagonals <- sum_by_DevYear - diagonals
     
     #Obtain weightage average data frame
     weighted_average <- as.data.frame(matrix(numeric(0),
                                ncol=1, 
                                nrow = nrow(devtri)-1))
     for(i in 1:nrow(weighted_average)){
       weighted_average[i,1] <- sum_by_DevYear[i+1]/sum_without_diagonals[i]
     }
     weighted_average
   })
   
   #Calculate Development
   # Consider ultimate development occurs at max(DevYear)+1
   # development from DevYear(n) to DevYear(n+1) has a factor of "weighted average"
   # development from max(DevYear) to ultimate has a factor of "tail factor"
   projected_development <- reactive({
      
      #call reactives for easier referencing
     devtri <- cumu_development_triangle()
     weighted_average <- weighted_avg()
     
     #obtain development using weighted average
     proj_devtri <- devtri
     for(i in 1:nrow(devtri)){
       for(j in 1:ncol(devtri)){
         if(proj_devtri[i,j]==0){ #element is zero when it has not occur yet and requires projection
           proj_devtri[i,j] <- round(proj_devtri[i,j-1]*weighted_average[j-1,1],0)
         } else {
           proj_devtri[i,j] <- proj_devtri[i,j]
         }
       }
     }
     
     #Obtain development using tail factor
     ultimate_paid <- as.data.frame(matrix(numeric(0),
                                          ncol=1, 
                                          nrow = nrow(proj_devtri))) #make a data frame for appending to main frame later
     
     #Obtain ultimate claim for all row (Loss Years)
    for(i in 1:nrow(proj_devtri)){
       ultimate_paid[i,1]<-round(proj_devtri[i, ncol(proj_devtri)]*input$tail_factor,0)
    }
     
     #Appending ultimate claim result to main data frame
     proj_devtri <- cbind(proj_devtri,ultimate_paid)
     colnames(proj_devtri) <- c(1:ncol(proj_devtri))
     proj_devtri
   })
   

   #Show Projected Development Table
   output$projected_table <- renderDataTable({
     projected_development()
   })
   
   #Graphing with ggplot
   # need to convert wide format to long format
   output$cumulative_paid_claims_plot <- renderPlot({
     proj_devtri <- projected_development()
     #get rownames
     proj_devtri["Years"] <- rownames(proj_devtri)
     #convert to long format
     long_proj_devtri <- reshape2::melt(proj_devtri, id.vars= "Years")
     
     #rename columns for easier referencing
     colnames(long_proj_devtri)<-c("Years", "DevYear", "ClaimsPaid")
     
     #Plotting with ggplot2
     ggplot(long_proj_devtri, 
            aes(x=DevYear, y=ClaimsPaid, 
                colour = Years)) +
       labs(title="Cumulative Paid Claims ($)", x="Development Year") +
       geom_point() + geom_line(aes(group=Years)) +
       geom_text(hjust=0, vjust=-1, aes(label=ClaimsPaid))
     
   })
 }
   
 
 
 # Create the app object
 shinyApp(ui = ui, server = server)
 