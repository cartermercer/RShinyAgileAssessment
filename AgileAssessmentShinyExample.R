rm(list=ls())

## Load Packages ##
library(shiny)
library(shinydashboard)
library(ggplot2)
library(devtools)
library(rdrop2)
library(tippy)
library(shinyWidgets)
library(shinymanager)


#LOGIN (commented out but this would create a password to login)
#tempUser = "tempUser"
#tempPass = "tempPass"

#inactivity <- "function idleTimer() {
#var t = setTimeout(logout, 120000);
#window.onmousemove = resetTimer; // catches mouse movements
#window.onmousedown = resetTimer; // catches mouse movements
#window.onclick = resetTimer;     // catches mouse clicks
#window.onscroll = resetTimer;    // catches scrolling
#window.onkeypress = resetTimer;  //catches keyboard actions

#function logout() {
#window.close();  //close the window
#}

#function resetTimer() {
#clearTimeout(t);
#t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
#}
#}
#idleTimer();"



# data.frame with credentials info
#credentials <- data.frame(
  #user = c(tempUser),
  #password = c(tempPass),
  #stringsAsFactors = FALSE
#)

#DATA SET UP
#authorize dropbox to write data to and from the app
token <- drop_auth()
saveRDS(token, file = "token.rds")

#a function reading in questions that come from dropbox
loadQuestions <- function() {
  # Read all the files into a list
  outputDir <- "AgileQuestions"
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path_display
  questions <- lapply(filePaths, drop_read_csv, header = F,stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  questions <- unlist(questions)
  
}

#load the questions for the survey
questions = loadQuestions()

#Begin Creating App Here
#R shiny apps are created with two main components - a User Interface (UI) and a Server Function

##             FIRST CREATE USER INTERFACE          ##

#User interface
ui = #secure_app(head_auth = tags$script(inactivity), #if login implemented this would present user with a login before getting into app
                ( dashboardPage(skin = "blue",
                                dashboardHeader(title = "Agile Assessment", titleWidth = 300),
                                #sidebar content
                                dashboardSidebar(
                                  sidebarMenu(
                                    id = "tabs",
                                    menuItem("Home", tabName = "home", icon = icon("home")),
                                    menuItem("Assessment Questions", tabName = "survey", icon = icon("poll")),
                                    menuItem("Team Report", tabName = "dashboard", icon = icon("dashboard"))
                                  )
                                ),
                                dashboardBody(
                                  #First tab content
                                  tabItems(
                                    tabItem(tabName = "home", h1("R Shiny App Example: Agile Assessment Tool"),
                                            fluidPage(
                                              h4("The Agile Assessment Tool will properly assess agile performance at the team level, and score and 
                                  report on a client organization’s agility. It will collect the data and securely display the results 
                                  of the organization being evaluated. The user will be able to download the organization's results 
                                  as a PDF to share the scores. The goal is to provide insights for a team to improve their agility."),
                                              h4("The questions asked in the assessment tool are shaped around the scrum methodology and were written with help by the"),
                                              tags$a("Scrum Guide", href="https://www.scrumguides.org/docs/scrumguide/v2017/2017-Scrum-Guide-US.pdf#zoom=100",
                                                     target = "_blank"),
                                              p(""),
                                            ),
                                            actionButton("goButton", "Take the Assessment") #actionButton() creates a button
                                    ),
                                    #Second tab content. note: the h1() call is a HTML header, and p() is HTML paragraph
                                    tabItem(
                                      tabName = "survey", h1("Complete the Assessment Questions & Find out your Team's Agility"),
                                      fluidPage(
                                        #create a tab box
                                        tabBox(id="tabBox_next_previous", type = "pills", width = 12,
                                               #Introduction tab
                                               tabPanel("Introduction",h2("Welcome"),
                                                        p("Each question is graded on a scale of 1-5 with an option of choosing
                                                  'NA'. Additional information is provided for each question."),
                                                        p("1- Not Present: This skill or capability is not being demonstrated by the team at all or the team does 
                                                      not have an idea on how to do it."),
                                                        p("2- Below Average:  This skill or capability on the team has clear room for improvement."),
                                                        p("3- Average: This skill or capability has some room for improvement."),
                                                        p("4- Above Average: This skill or capability on the team is demonstrated very well. 
                                                      There is little room for improvement."),
                                                        p("5- Top Performer: This skill or capability is being thoroughly demonstrated by the team and the 
                                                      team is extremely proficient in completing it."),
                                                        p("NA - The respondent has this option for a question because it does not apply. Please do your best
                                                  to answer each question unless you absolutely cannot."),
                                                        p(""),
                                                        p("There will be a comments field at the end of each section (category) to provide
                                                  additional information not described by the questions")),
                                               #Team Culture Tab. Note: the h4() is HTML call for a header
                                               tabPanel("Team Culture",h4("This category helps assess the team’s accountability, 
                                                                  trust/respect, creativity, collaboration, and happiness."),
                                                        #selectInput is an option for data input as drop-down
                                                        #questions [1] is how R selects first element of questions which is the first question
                                                        selectInput("teamCulture1", questions[1], c("NA",1,2,3,4,5),  selected = "Enter a response"),
                                                        fluidRow(
                                                          box(questions[25], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info1"),
                                                              width = 3)
                                                        ),
                                                        selectInput("teamCulture2", questions[2],c("NA",1,2,3,4,5)),
                                                        #create a way to add a collapsible box for question description to be shown
                                                        fluidRow(
                                                          box(questions[26], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info2"),
                                                              width = 3)
                                                        ),
                                                        selectInput("teamCulture3", questions[3],c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[27], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info3"),
                                                              width = 3)
                                                        ),
                                                        selectInput("teamCulture4", questions[4],c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[28], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info4"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("teamCultureComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Management Tab
                                               tabPanel("Management",h4("This category is possibly the most important as it can help assess issues 
                                                                in Agile leadership, team engagement, and the development of the team."),
                                                        selectInput("management1", questions[5] ,c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[29], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info5"),
                                                              width = 3)
                                                        ),
                                                        selectInput("management2", questions[6],c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[30], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info6"),
                                                              width = 3)
                                                        ),
                                                        selectInput("management3", questions[7],c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[31], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info7"),
                                                              width = 3)
                                                        ),
                                                        selectInput("management4", questions[8],c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[32], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info8"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("managementComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Objective tab
                                               tabPanel("Objectives",h4("This category asks questions about the team's ability to work at the highest 
                                                                level and it ensures that the team is working timely and completes their goals."),
                                                        selectInput("objectives1", questions[9], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[33], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info9"),
                                                              width = 3)
                                                        ),
                                                        selectInput("objectives2", questions[10], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[34], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info10"),
                                                              width = 3)
                                                        ),
                                                        selectInput("objectives3", questions[11], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[35], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info11"),
                                                              width = 3)
                                                        ),
                                                        selectInput("objectives4", questions[12], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[36], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info12"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("managementComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Precision Tab
                                               tabPanel("Precision",h4("This category assesses specific roles in the team, along with the team 
                                                               vision, and specialities. "),
                                                        selectInput("precision1", questions[13], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[37], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info13"),
                                                              width = 3)
                                                        ),
                                                        selectInput("precision2", questions[14], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[38], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info14"),
                                                              width = 3)
                                                        ),
                                                        selectInput("precision3", questions[15], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[39], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info15"),
                                                              width = 3)
                                                        ),
                                                        selectInput("precision4", questions[16], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[40], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info16"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("managementComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Agility Tab
                                               tabPanel("Agility",h4("This category assesses basic requirements for agility and how stakeholders are treated 
                                                             by the team, but the bulk of this category is to assess quality of work, time spent, and the 
                                                             value delivered by the team."),
                                                        selectInput("agility1", questions[17], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[41], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info17"),
                                                              width = 3)
                                                        ),
                                                        selectInput("agility2", questions[18], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[42], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info18"),
                                                              width = 3)
                                                        ),
                                                        selectInput("agility3", questions[19], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[43], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info19"),
                                                              width = 3)
                                                        ),
                                                        selectInput("agility4", questions[20], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[44], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info20"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("managementComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Planning Tab
                                               tabPanel("Planning",h4("This category discusses the teams ability to create successful and accurate 
                                                              sprint plans for the development team to follow."),
                                                        selectInput("planning1", questions[21], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[45], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info21"),
                                                              width = 3)
                                                        ),
                                                        selectInput("planning2", questions[22], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[46], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info22"),
                                                              width = 3)
                                                        ),
                                                        selectInput("planning3", questions[23], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[47], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info23"),
                                                              width = 3)
                                                        ),
                                                        selectInput("planning4", questions[24], c("NA",1,2,3,4,5)),
                                                        fluidRow(
                                                          box(questions[48], title = "Show/Hide Question Details", status = "primary", 
                                                              solidHeader = FALSE, collapsible = TRUE, collapsed = TRUE ,valueBoxOutput("info24"),
                                                              width = 3)
                                                        ),
                                                        textAreaInput("managementComments", "Please enter any comments about the questions above:")
                                               ),
                                               #Complete Tab
                                               tabPanel("Complete",h2("Thank you for completing the questions!"), 
                                                        p("Please press submit. Your responses will be recorded and you will be directed
                                                  back to the Home Page upon completion."),
                                                        #Review answers button, SUBMIT button, go home button
                                                        actionButton("review", "Review Answers"), actionButton("submit", "Submit"),
                                                        actionButton("goHome", "Go Home")),
                                               #Java script code in order to have Previous-Next tabs work
                                               tags$script("
                                                        $('body').mouseover(function() {
                                                        list_tabs=[];
                                                        $('#tabBox_next_previous li a').each(function(){
                                                        list_tabs.push($(this).html())
                                                        });
                                                        Shiny.onInputChange('List_of_tab', list_tabs);})
                                                    ") 
                                        )
                                      ),
                                      uiOutput("Next_Previous"), p("")
                                      # end of Previous-Next tabs
                                      
                                      #Third tab content - Team Report (Dashboard)
                                    ),
                                    tabItem(tabName = "dashboard", h1("Team Report"),
                                            fluidPage(
                                              downloadButton("report", "Generate report"),
                                              frow1 <- fluidRow(
                                                valueBoxOutput("value1") #number of responses box
                                                ,valueBoxOutput("value2") #average score box
                                                ,valueBoxOutput("value3") #box that says "Team Evaluation"
                                              ),
                                              frowDropdown <- fluidRow(
                                                #dropdown menu for selecting category
                                                selectInput("category", "Select a category:", 
                                                            choices = c("Dashboard Home", "Team Culture", "Management", "Objectives", "Precision", "Agility", "Planning"))
                                              ),
                                              frow2 <- fluidRow( 
                                                box(
                                                  title = "Radar" #RADAR CHART  
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE 
                                                  ,plotOutput("radar", height = "300px")
                                                )
                                                ,box(
                                                  title = "Boxplots" #BOXPLOTS
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE 
                                                  ,plotOutput("boxplot", height = "300px")
                                                )
                                                ,box(
                                                  title = "Bar Charts" #BAR PLOTS
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE 
                                                  ,plotOutput("bar", height = "300px")
                                                ),box(
                                                  title = "Comparing Respondents" #PARALLEL PLOTS / COMPARING RESPONDENTS
                                                  ,status = "primary"
                                                  ,solidHeader = TRUE 
                                                  ,collapsible = TRUE 
                                                  ,plotOutput("parallel", height = "300px")
                                                ) 
                                              ),
                                              fluidRow(
                                                downloadButton('download',"Download the data") #download data button
                                              ),
                                              fluidRow(
                                                column(12, DT::dataTableOutput("dataTable", width = "100%")) #output data table
                                              )
                                              
                                            )
                                    )
                                  )
                                )
                )
) 
#closing for dashboardPage (which was call at very start of UI)


# As mentioned above - R Shiny apps also have a server function #

##             SERVER FUNCTION          ##


#Server function
server <- function(input, output, session) {
  
  library(shiny)
  library(shinydashboard)
  library(ggplot2)
  library(devtools)
  library(rdrop2)
  library(reshape2)
  
  #login credentials (login is again commented out)
  #result_auth <- secure_server(check_credentials = check_credentials(credentials))
  #output$res_auth <- renderPrint({
    #reactiveValuesToList(result_auth)
  #})
  
  #take assessment button
  observeEvent(input$goButton, {
    updateTabItems(session = session, inputId = "tabs"  , selected = "survey")
  })
  
  #go home button
  observeEvent(input$goHome, {
    updateTabItems(session = session, inputId = "tabs"  , selected = "home")
  })
  
  #review answer button
  observeEvent(input$review, {
    updateTabItems(session = session, inputId = "tabBox_next_previous" , selected = "Team Culture")
  })
  
  #Tab previous next buttons
  Previous_Button=tags$div(actionButton("Prev_Tab",HTML('Previous Section')))
  Next_Button=div(actionButton("Next_Tab",HTML('Next Section')))
  
  output$Next_Previous=renderUI({
    tab_list=input$List_of_tab[-length(input$List_of_tab)]
    nb_tab=length(tab_list)
    if (which(tab_list==input$tabBox_next_previous)==nb_tab)
      column(1,offset=1,Previous_Button)
    else if (which(tab_list==input$tabBox_next_previous)==1)
      column(1,offset = 10,Next_Button)
    else
      div(column(1,offset=1,Previous_Button),column(1,offset=8,Next_Button))
  })
  observeEvent(input$Prev_Tab,
               {
                 tab_list=input$List_of_tab 
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab-1])
               })
  
  observeEvent(input$Next_Tab,
               {
                 tab_list=input$List_of_tab
                 current_tab=which(tab_list==input$tabBox_next_previous)
                 updateTabsetPanel(session,"tabBox_next_previous",selected=tab_list[current_tab+1])
               })
  #End of tab previous next buttons
  
  #Re-Authorize dropbox
  token <- drop_auth()
  saveRDS(token, file = "token.rds")
  
  # Then in any drop_* function, pass `dtoken = token
  # Tokens are valid until revoked.
  
  library(dplyr)
  #drop_acc() %>% data.frame()
  
  #These fields are defined as a result from survey ids from UI - needed to save data
  fieldsAll <- c("teamCulture1","teamCulture2", "teamCulture3", "teamCulture4", 
                 "management1","management2", "management3", "management4", 
                 "objectives1" ,"objectives2","objectives3", "objectives4",
                 "precision1", "precision2", "precision3", "precision4", #"precision5",
                 "agility1", "agility2", "agility3", "agility4",
                 "planning1", "planning2", "planning3", "planning4"
  )
  
  #function to output time in integer form
  epochTime <- function() {
    as.integer(Sys.time())
  }
  
  #formData is a reactive function which takes the data collected from the fields gets the data into a 
  #list format
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
  })
  
  #function used in naming the response file that gets saved 
  humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")
  
  #start of Save Data function. This function saves data into a folder called "responses
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    
    outputDir <- "responses"
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, path = outputDir)
  }
  
  #end of Save Data function.
  #####################################
  # SUBMIT BUTTON (crucial part of saving data, using functions just defined)
  ####################################
  observeEvent(input$submit, {
    saveData(formData())
    updateTabItems(session = session, inputId = "tabs"  , selected = "home")
  })
  
  #function to load responses
  loadData <- function() {
    # Read all the files into a list
    outputDir <- "responses"
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path_display
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data 
  }
  
  #load in data for reporting on dashboard
  data = loadData()
  #block of code for reformatting data is 
  #one Big if-statement that checks to see if there is any responses recorded yet and transforms data if there is
  #Start of big if statement
  if (length(data) > 0) {
    colnames(data) = c(fieldsAll, "id")
    data <- melt(data, id = "id")
    data = data[,-1]
    
    #Update these if needed
    nQuestionsPerCategory = 4
    nCategories = 6
    nTotalQuestions = nQuestionsPerCategory*nCategories
    nResponses = nrow(data)/(nCategories*nQuestionsPerCategory)
    #write a for loop to add a new column to the data for the respondent starting with empty vector called respondent
    respondent = c()
    for (i in seq(1,nResponses)) {
      respondent = c(print(paste("respondent",seq(1,i))))
    }
    
    #make category column information
    category = c(rep("Team Culture",nrow(data)/nCategories),rep("Management",nrow(data)/nCategories), 
                 rep("Objectives",nrow(data)/nCategories), rep("Precision",nrow(data)/nCategories), 
                 rep("Agility",nrow(data)/nCategories), rep("Planning",nrow(data)/nCategories) )
    #name columns for the new data frame
    dataCols = c("Question", "Score", "Category", "Respondent")
    
    #add category column
    data = cbind(data, category)
    #add respondent column
    data = cbind(data, respondent)
    colnames(data) = dataCols
    categories = c("Team Culture", "Management", "Objectives", "Precision", "Agility", "Planning")
  } 
  # end of big if statement
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    data = loadData() 
    valueBox(
      formatC(nrow(data), format="d", big.mark=',')
      ,paste('Number of Responses:',nrow(data))
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")  
  })
  
  output$value2 <- renderValueBox({
    
    if (input$category == "Dashboard Home") {
      data = data
    } else if (input$category == "Team Culture") {
      data = data[data$Category == "Team Culture",]
    } else if (input$category == "Management") {
      data = data[data$Category == "Management",]
    } else if (input$category == "Objectives") {
      data = data[data$Category == "Objectives",]
    } else if (input$category == "Precision") {
      data = data[data$Category == "Precision",]
    } else if (input$category == "Agility") {
      data = data[data$Category == "Agility",] 
    } else if (input$category == "Planning") {
      data = data[data$Category == "Planning",]
    }
    if (mean(data$Score, na.rm = TRUE) <= 5/3) {
      valueBox(
        paste("Average Score:" , round(mean(data$Score, na.rm = TRUE),2))
        ,paste("Rating: Low")
        ,icon = icon("frown")
        ,color = "red")  
    } else if (mean(data$Score, na.rm =TRUE) <= 10/3) {
      valueBox(
        paste("Average Score:", round(mean(data$Score, na.rm = TRUE),2))
        ,paste("Rating: Medium")
        ,icon = icon("meh")
        ,color = "yellow")  
    } else {
      valueBox(
        paste("Average Score:", round(mean(data$Score, na.rm = TRUE),2))
        ,paste("Rating: High")
        ,icon = icon("smile")
        ,color = "green")  
    }
    
  })
  
  output$value3 <- renderValueBox({
    infoBox(title = h3("Team Evaluation")
            ,icon = icon("users")
            ,color = "blue")  
  })
  
  
  #Plotting on dashboard
  #RADAR
  output$radar <- renderPlot({
#this logic is necessary for each plot as it is needed for the dropdown on the dashboard
    if (input$category == "Dashboard Home") {
      data = data
    } else if (input$category == "Team Culture") {
      data = data[data$Category == "Team Culture",]
    } else if (input$category == "Management") {
      data = data[data$Category == "Management",]
    } else if (input$category == "Objectives") {
      data = data[data$Category == "Objectives",]
    } else if (input$category == "Precision") {
      data = data[data$Category == "Precision",]
    } else if (input$category == "Agility") {
      data = data[data$Category == "Agility",] 
    } else if (input$category == "Planning") {
      data = data[data$Category == "Planning",]
    }
    #now plot it
    
    library("ggplot2")
    #radar plot is a bit challenging - I had to find online examples and restructure for my data
    coord_radar <- function (theta = "x", start = 0, direction = 1) {
      theta <- match.arg(theta, c("x", "y"))
      r <- if (theta == "x") 
        "y"
      else "x"
      ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
              direction = sign(direction),
              is_linear = function() TRUE)
    }
    
    y_levels <- levels(factor(1:5))
    ggplot(data = data, aes(x = Question, y = Score)) +
      geom_polygon(aes(group = Respondent, color = Respondent), fill = NA, size = 2, show.legend = FALSE) +
      geom_line(aes(group = Respondent, color = Respondent), size = 2) +
      scale_y_discrete(limits = y_levels, expand = c(0.5, 0)) +
      xlab("") + ylab("") +
      annotate("text", x = 2, y = 1:5, label = 1:5, hjust = 1) +
      scale_x_discrete() +
      guides(color = guide_legend(ncol=2)) +
      coord_radar()
  })
  
  #Bar
  output$bar = renderPlot({
    
    if (input$category == "Dashboard Home") {
      data = data
    } else if (input$category == "Team Culture") {
      data = data[data$Category == "Team Culture",]
    } else if (input$category == "Management") {
      data = data[data$Category == "Management",]
    } else if (input$category == "Objectives") {
      data = data[data$Category == "Objectives",]
    } else if (input$category == "Precision") {
      data = data[data$Category == "Precision",]
    } else if (input$category == "Agility") {
      data = data[data$Category == "Agility",] 
    } else if (input$category == "Planning") {
      data = data[data$Category == "Planning",]
    }
    #now plot it
    #plot it
    y_levels <- levels(factor(1:5))
    if (input$category == "Dashboard Home") {
      ggplot(data, aes(Question, fill = Category)) +
        #geom_bar() +
        stat_summary_bin(mapping = aes(y = Score), fun.y = "mean", geom = "bar") +
        labs(title = paste("Comparison of Average Scores Between Categories")) +
        theme(strip.text.x = element_text(size = rel(0.8)),
              axis.text.x = element_blank(),
              #axis.ticks.y = element_blank()
              axis.ticks.x = element_blank())
      
      
    } else {
      ggplot(data, aes(Question, Score)) +
        geom_bar(stat = "identity", aes(fill = Category)) +
        labs(title = paste("Comparison of Average Scores for", input$category)) +
        theme(strip.text.x = element_text(size = rel(0.8)),
              axis.text.x = element_blank(),
              #axis.ticks.y = element_blank()
              axis.text.y = element_blank())
    }
    
    
    
    
  })
  
  #Boxplot
  output$boxplot = renderPlot({
    
    if (input$category == "Dashboard Home") {
      data = data
    } else if (input$category == "Team Culture") {
      data = data[data$Category == "Team Culture",]
    } else if (input$category == "Management") {
      data = data[data$Category == "Management",]
    } else if (input$category == "Objectives") {
      data = data[data$Category == "Objectives",]
    } else if (input$category == "Precision") {
      data = data[data$Category == "Precision",]
    } else if (input$category == "Agility") {
      data = data[data$Category == "Agility",] 
    } else if (input$category == "Planning") {
      data = data[data$Category == "Planning",]
    }
    #now plot it
    #plot it
    if (input$category == "Dashboard Home") {
      ggplot(data, aes(Category, Score)) +
        geom_boxplot() +
        labs(title = "Comparison of Scores Between Categories")
    } else {
      ggplot(data, aes(Question, Score)) +
        geom_boxplot() +
        labs(title = paste("Comparison of Scores for", input$category))
    }
    
    #Parallel
  })
  
  #parallel
  output$parallel = renderPlot({
    
    if (input$category == "Dashboard Home") {
      data = data
    } else if (input$category == "Team Culture") {
      data = data[data$Category == "Team Culture",]
    } else if (input$category == "Management") {
      data = data[data$Category == "Management",]
    } else if (input$category == "Objectives") {
      data = data[data$Category == "Objectives",]
    } else if (input$category == "Precision") {
      data = data[data$Category == "Precision",]
    } else if (input$category == "Agility") {
      data = data[data$Category == "Agility",] 
    } else if (input$category == "Planning") {
      data = data[data$Category == "Planning",]
    }
    #now plot it
    #plot it
    y_levels <- levels(factor(1:4))
    ggplot(data = data, aes(x = Question, y = Score)) +
      geom_path(aes(group = Respondent, color = Respondent), size = 2) +
      scale_y_discrete(limits = y_levels, expand = c(0.5, 0)) +
      xlab("") + ylab("") +
      guides(color = guide_legend(ncol=2)) +
      theme(strip.text.x = element_text(size = rel(0.8)),
            axis.text.x = element_blank(),
            #axis.ticks.y = element_blank()
            axis.text.y = element_blank()) 
  })
  
  
  # Downloadable csv of selected dataset ----
  output$download <- 
    downloadHandler(
      filename = function() { paste('data.csv') }, content = function(file) {
        write.csv(data, file, row.names = FALSE)
      }
    )
  
  output$dataTable = DT::renderDataTable({
    DT::datatable(data)
  })
  
  
  # work in progress to effectively donwnload dashboard as pdf
  #output$report <- 
  #downloadHandler(
  #"results_from_shiny.pdf",
  #content = 
  #function(file)
  #{
  #rmarkdown::render(
  #input = "~/Desktop/SavvyApp/report.Rmd",
  #output_file = "~/Desktop/SavvyApp/built_report.pdf",
  #params = list(radar = output$radar,
  #boxplot = boxplot,
  #bar = bar,
  #parallel = parallel)
  #) 
  #readBin(con = "built_report.pdf", 
  #what = "raw",
  #n = file.info("built_report.pdf")[, "size"]) %>%
  #writeBin(con = file)
  #}
  #)
  
}

shinyApp(ui, server)      

