
library(shiny)
library(dplyr)
#login-------------
library(shinyauthr)

# u_info <- read.csv("u_info.csv",sep=";")
# u_info <- u_info[,1:5]
# u_info$user <- as.character(u_info$user)
# u_info$name <- as.character(u_info$name)
# u_info$sex <- as.character(u_info$sex)
# u_info$cat <- as.character(u_info$cat)
# str(u_info)
# for (i in paste0("S2B",seq(1:40))) {
#   u_info[,i] <- NA
# }
# 
# 
# u_info_list <- vector("list", nrow(u_info))
# for (i in 1:nrow(u_info)) {
#   u_info_list[[i]] <- u_info[i,]
# }
# names(u_info_list) <- u_info$user
# 
# for(i in names(u_info_list)){
#   write.csv(u_info_list[[i]], paste0(i,"_FlowKupa2",".csv"),
#             row.names = FALSE, quote = TRUE )
# }


users <-read.csv("user_table_season2.csv",sep=";")

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
    user = as.character(users$user),
    password = sapply(as.character(users$password),
                      sodium::password_store),
    permissions = as.character(users$permission),
    season1=users$seasons1
)
#




#1---------------------------
outputDir <- "responses"
outputDir_Admin <- "settings"
    
    saveData <- function(data,user) {
        data <- t(data)
        # Create a unique file name
        #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        fileName <- sprintf("%s_%s.csv", data[[1]], "FlowKupa2")
        # Write the file to the local system
        write.csv(
            x = data,
            file = file.path(outputDir, fileName), 
            row.names = FALSE, quote = TRUE )
    }
    
    saveData_Admin <- function(data,user) {
        data <- t(data)
        # Create a unique file name
        #fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
        fileName <- sprintf("%s_%s.csv", "FlowKupa2", "settings")
        # Write the file to the local system
        write.csv(
            x = data,
            file = file.path(outputDir_Admin, fileName), 
            row.names = FALSE, quote = TRUE )
    }
    
    loadData <- function() {
        # Read all the files into a list
        files <- list.files(outputDir, full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data
    }
    
    loadData_Admin <- function() {
        # Read all the files into a list
        files <- list.files(outputDir_Admin, full.names = TRUE,pattern = "^[FlowKupa2]")
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
        # Concatenate all data together into one data.frame
        data <- do.call(rbind, data)
        data
    }
    
    
    #2------------
    # Define the fields we want to save from the form
    fields <- c("user","season1","name","sex","cat",
                paste0("S2B",seq(1:40)))
    #fields_admin <- c("verseny","boulderek")
    
    
    
    #app--------------
    ui <- fluidPage(
        titlePanel(title = span(img(src = "flowlogo.png", height = 35) ),"2. Flow seasons 2024"),
        # logout button
        div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
        
        # login section
        shinyauthr::loginUI(id = "login"),
        
        # Sidebar to show user info after login
        uiOutput("sidebarpanel"),
        
        # Info after login
        #fluidPage("Ha beléptél add meg az eredményeid."),
        
        tableOutput("user_table")
        
    )
    
    server <- function(input, output, session) {
        
        credentials <- shinyauthr::loginServer(
            id = "login",
            data = user_base,
            user_col = user,
            pwd_col = password,
            sodium_hashed = TRUE,
            log_out = reactive(logout_init())
        )
        
        # Logout to hide
        logout_init <- shinyauthr::logoutServer(
            id = "logout",
            active = reactive(credentials()$user_auth)
        )
        
        # UI
        output$sidebarpanel <- renderUI({
            # Show only when authenticated
            req(credentials()$user_auth)
            if(credentials()$info[[3]]=="admin"){
                fluidPage(
                    tags$hr(),
                    #span(strong("Ez a próbaverzió! Az élőt a következő linken éred el: http://flowboulder.hopto.org:3838/FlowSeasons01/ "), style="color:red"),tags$hr(),
                    strong("Admin felület"),br(),
                    #textInput("verseny","Verseny neve","FlowKupa2"),
                    radioButtons("stop","Felnőtt/gyermek",choices = c("Adult","Child","Stop"),selected = "Adult"),
                    fluidRow(
                        checkboxGroupInput("boulderek","Boulderek",choices = fields[6:45],selected = fields[6:45],
                                           inline = T)
                    ),
                    
                    actionButton("submit", "Submit",class = "btn btn-primary"),tags$hr(),
                    strong("Settings"),br(),
                    DT::dataTableOutput("settings", width = 300), tags$hr(),
                    strong("Eredmények (Felnőtt)"),br(),
                    "Results (Adult)",
                    DT::dataTableOutput("results1", width = 300), tags$hr(),
                    strong("Eredmények (Gyermek)"),br(),
                    "Results (Children)",
                    DT::dataTableOutput("results2", width = 300), tags$hr(),
                    strong("Megmászások és Zónák"),br(),
                    "Tops & zones",
                    DT::dataTableOutput("responses", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Felnőtt (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: adult (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points1", width = 300), tags$hr(),
                    strong("Aktuális Pontok boulderenként: Gyermek (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    "Actual points for boulders: Children (Top=1000/number of tops, Zone=500/number of zones)",
                    DT::dataTableOutput("points2", width = 300), tags$hr()
                )
            }else{
                fluidPage(
                    tags$hr(),
                    #span(strong("Ez a próbaverzió! Az élőt a következő linken éred el: http://flowboulder.hopto.org:3838/FlowSeasons01/ "), style="color:red"),tags$hr(),
                    span(strong("Adataid módosítása után mindig kattints a 'Beküld/Submit' gombra!"), style="color:red"),tags$hr(),
                    strong("Kérlek töltsd ki az összes mezőt."),br(),
                    "Please fill all fields.",br(),
                    textInput("name", "Name", ""),
                    radioButtons("sex","Sex",choices = c("Man","Woman"),selected = "Man",inline = T),
                    radioButtons("cat","Category",
                                 choiceNames = c("Kezdő","Hobby","Nyílt","Senior","Masters", "U10", "U12", "U14", "U16"),
                                 choiceValues =c("kezdo","hobby","nyilt","senior","masters", "U10", "U12", "U14", "U16"),
                                 selected = "k",inline = T),
                    column(3,
                           (if("S2B1" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B1", "Bolulder 01", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B1" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B1", "Bolulder 01", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B2" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B2", "Bolulder 02", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B2" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B2", "Bolulder 02", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B3" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B3", "Bolulder 03", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B3" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B3", "Bolulder 03", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B4" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B4", "Bolulder 04", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B4" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B4", "Bolulder 04", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B5" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B5", "Bolulder 05", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B5" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B5", "Bolulder 05", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B6" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B6", "Bolulder 06", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B6" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B6", "Bolulder 06", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B7" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B7", "Bolulder 07", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B7" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B7", "Bolulder 07", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B8" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B8", "Bolulder 08", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B8" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B8", "Bolulder 08", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B9" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B9", "Bolulder 09", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B9" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B9", "Bolulder 09", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B10" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B10", "Bolulder 10", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B10" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B10", "Bolulder 10", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           })
                    ),
                    column(3,
                           (if("S2B11" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B11", "Bolulder 11", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B11" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B11", "Bolulder 11", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B12" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B12", "Bolulder 12", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B12" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B12", "Bolulder 12", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B13" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B13", "Bolulder 13", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B13" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B13", "Bolulder 13", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B14" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B14", "Bolulder 14", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B14" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B14", "Bolulder 14", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B15" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B15", "Bolulder 15", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B15" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B15", "Bolulder 15", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B16" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B16", "Bolulder 16", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B16" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B16", "Bolulder 16", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B17" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B17", "Bolulder 17", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B17" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B17", "Bolulder 17", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B18" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B18", "Bolulder 18", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B18" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B18", "Bolulder 18", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B19" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B19", "Bolulder 19", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B19" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B19", "Bolulder 19", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B20" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B20", "Bolulder 20", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B20" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B20", "Bolulder 20", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           })
                    ),
                    column(3,
                           (if("S2B21" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B21", "Bolulder 21", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B21" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B21", "Bolulder 21", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B22" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B22", "Bolulder 22", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B22" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B22", "Bolulder 22", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B23" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B23", "Bolulder 23", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B23" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B23", "Bolulder 23", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B24" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B24", "Bolulder 24", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B24" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B24", "Bolulder 24", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B25" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B25", "Bolulder 25", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B25" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B25", "Bolulder 25", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B26" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B26", "Bolulder 26", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B26" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B26", "Bolulder 26", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B27" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B27", "Bolulder 27", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B27" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B27", "Bolulder 27", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B28" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B28", "Bolulder 28", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B28" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B28", "Bolulder 28", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B29" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B29", "Bolulder 29", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B29" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B29", "Bolulder 29", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B30" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B30", "Bolulder 30", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B30" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B30", "Bolulder 30", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           })
                    ),
                    column(3,
                           (if("S2B31" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B31", "Bolulder 31", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B31" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B31", "Bolulder 31", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B32" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B32", "Bolulder 32", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B32" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B32", "Bolulder 32", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B33" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B33", "Bolulder 33", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B33" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B33", "Bolulder 33", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B34" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B34", "Bolulder 34", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B34" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B34", "Bolulder 34", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B35" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B35", "Bolulder 35", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B35" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B35", "Bolulder 35", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B36" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B36", "Bolulder 36", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B36" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B36", "Bolulder 36", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B37" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B37", "Bolulder 37", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B37" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B37", "Bolulder 37", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B38" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B38", "Bolulder 38", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B38" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B38", "Bolulder 38", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B39" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B39", "Bolulder 39", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B39" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B39", "Bolulder 39", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           }),
                           (if("S2B40" %in% settings() & "Adult" %in% settings()){
                             radioButtons("S2B40", "Bolulder 40", choices = c("None","Zone","Top"),selected = "None",inline = T)
                           }else{
                             if("S2B40" %in% settings() & "Child" %in% settings()){
                               radioButtons("S2B40", "Bolulder 40", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = "None",inline = T)
                             }
                           })
                    ),
                    column(12,actionButton("submit", "Beküld/Submit",class = "btn btn-danger", style='font-size:200%'),
                           align="center"),br(),
                    
                    (if("Stop" %in% settings()){
                        strong("A verseny lezárult, a módosításokat már nem mentheted el.")
                    }),
                    tags$hr(),
                    strong("Összesen:"),br(),
                    "Summary",
                    DT::dataTableOutput("summary", width = 300), tags$hr(),
                    # strong("Eredmények (Felnőtt)"),br(),
                    # "Results (adult)",
                    # DT::dataTableOutput("results1", width = 300), tags$hr(),
                    # strong("Eredmények (Gyermek)"),br(),
                    # "Results (Children)",
                    # DT::dataTableOutput("results2", width = 300), tags$hr(),
                    # strong("Megmászások és Zónák"),br(),
                    # "Tops & zones",
                    # DT::dataTableOutput("responses", width = 300), tags$hr(),
                    # strong("Aktuális Pontok boulderenként: Felnőtt (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    # "Actual points for boulders: adult (Top=1000/number of tops, Zone=500/number of zones)",
                    # DT::dataTableOutput("points1", width = 300), tags$hr(),
                    # strong("Aktuális Pontok boulderenként: Gyermek (Top=1000/topok száma, Zóna=500/zónák száma)"),br(),
                    # "Actual points for boulders: Children (Top=1000/number of tops, Zone=500/number of zones)",
                    # DT::dataTableOutput("points2", width = 300), 
                    tags$hr()
                )
            }
            
        })
        
        formData <- reactive({
            if(credentials()$info[[3]]=="admin"){
                data <- c(input$stop,input$boulderek)
                
            }else{
                data <- sapply(fields, function(x) input[[x]])
                data$user<-credentials()$info[[1]]
                data$season1 <- credentials()$info[[4]][1]
            }
            data
        })
        
        # When the Submit button is clicked, save the form data
        settings<-reactive({
            input$submit
            loadData_Admin()
        })
        
        observeEvent(input$submit, {
            if(credentials()$info[[3]]=="admin"){
                saveData_Admin(formData())
            }else{
                if(settings()[[1]]!="Stop"){
                    saveData(formData())
                }
                
            }
            
        })
        
        
        userdata<-reactive({
            input$submit
            user0<-credentials()$info[[1]][1]
            if(is.null(user0)){loadData()}else{
                loadData() %>% filter(user==user0)
            }
        })

        observe({
            updateRadioButtons(session,"stop","Adatbevitel leállítása",choices = c("Adult","Child","Stop"),selected = settings()[1])
            updateCheckboxGroupInput(session,"boulderek",choices = fields[6:45],selected = settings()[-1],inline = T)
            updateTextInput(session,"name","Name",userdata()$name)
            updateRadioButtons(session,"sex","Sex",choices = c("Man","Woman"),selected = userdata()$sex,inline = T)
            updateRadioButtons(session,"cat","Category",
                               choiceNames = c("Kezdő","Hobby","Nyílt","Senior","Masters", "U10", "U12", "U14", "U16"),
                               choiceValues =c("kezdo","hobby","nyilt","senior","masters", "U10", "U12", "U14", "U16"),
                               selected = userdata()$cat,inline = T)
            if("Adult" %in% settings() ){
              updateRadioButtons(session,"S2B1", "Bolulder 01", choices = c("None","Zone","Top"),selected = userdata()$S2B1,inline = T)
              updateRadioButtons(session,"S2B2", "Bolulder 02", choices = c("None","Zone","Top"),selected = userdata()$S2B2,inline = T)
              updateRadioButtons(session,"S2B3", "Bolulder 03", choices = c("None","Zone","Top"),selected = userdata()$S2B3,inline = T)
              updateRadioButtons(session,"S2B4", "Bolulder 04", choices = c("None","Zone","Top"),selected = userdata()$S2B4,inline = T)
              updateRadioButtons(session,"S2B5", "Bolulder 05", choices = c("None","Zone","Top"),selected = userdata()$S2B5,inline = T)
              updateRadioButtons(session,"S2B6", "Bolulder 06", choices = c("None","Zone","Top"),selected = userdata()$S2B6,inline = T)
              updateRadioButtons(session,"S2B7", "Bolulder 07", choices = c("None","Zone","Top"),selected = userdata()$S2B7,inline = T)
              updateRadioButtons(session,"S2B8", "Bolulder 08", choices = c("None","Zone","Top"),selected = userdata()$S2B8,inline = T)
              updateRadioButtons(session,"S2B9", "Bolulder 09", choices = c("None","Zone","Top"),selected = userdata()$S2B9,inline = T)
              updateRadioButtons(session,"S2B10", "Bolulder 10", choices = c("None","Zone","Top"),selected = userdata()$S2B10,inline = T)
              updateRadioButtons(session,"S2B11", "Bolulder 11", choices = c("None","Zone","Top"),selected = userdata()$S2B11,inline = T)
              updateRadioButtons(session,"S2B12", "Bolulder 12", choices = c("None","Zone","Top"),selected = userdata()$S2B12,inline = T)
              updateRadioButtons(session,"S2B13", "Bolulder 13", choices = c("None","Zone","Top"),selected = userdata()$S2B13,inline = T)
              updateRadioButtons(session,"S2B14", "Bolulder 14", choices = c("None","Zone","Top"),selected = userdata()$S2B14,inline = T)
              updateRadioButtons(session,"S2B15", "Bolulder 15", choices = c("None","Zone","Top"),selected = userdata()$S2B15,inline = T)
              updateRadioButtons(session,"S2B16", "Bolulder 16", choices = c("None","Zone","Top"),selected = userdata()$S2B16,inline = T)
              updateRadioButtons(session,"S2B17", "Bolulder 17", choices = c("None","Zone","Top"),selected = userdata()$S2B17,inline = T)
              updateRadioButtons(session,"S2B18", "Bolulder 18", choices = c("None","Zone","Top"),selected = userdata()$S2B18,inline = T)
              updateRadioButtons(session,"S2B19", "Bolulder 19", choices = c("None","Zone","Top"),selected = userdata()$S2B19,inline = T)
              updateRadioButtons(session,"S2B20", "Bolulder 20", choices = c("None","Zone","Top"),selected = userdata()$S2B20,inline = T)
              updateRadioButtons(session,"S2B21", "Bolulder 21", choices = c("None","Zone","Top"),selected = userdata()$S2B21,inline = T)
              updateRadioButtons(session,"S2B22", "Bolulder 22", choices = c("None","Zone","Top"),selected = userdata()$S2B22,inline = T)
              updateRadioButtons(session,"S2B23", "Bolulder 23", choices = c("None","Zone","Top"),selected = userdata()$S2B23,inline = T)
              updateRadioButtons(session,"S2B24", "Bolulder 24", choices = c("None","Zone","Top"),selected = userdata()$S2B24,inline = T)
              updateRadioButtons(session,"S2B25", "Bolulder 25", choices = c("None","Zone","Top"),selected = userdata()$S2B25,inline = T)
              updateRadioButtons(session,"S2B26", "Bolulder 26", choices = c("None","Zone","Top"),selected = userdata()$S2B26,inline = T)
              updateRadioButtons(session,"S2B27", "Bolulder 27", choices = c("None","Zone","Top"),selected = userdata()$S2B27,inline = T)
              updateRadioButtons(session,"S2B28", "Bolulder 28", choices = c("None","Zone","Top"),selected = userdata()$S2B28,inline = T)
              updateRadioButtons(session,"S2B29", "Bolulder 29", choices = c("None","Zone","Top"),selected = userdata()$S2B29,inline = T)
              updateRadioButtons(session,"S2B30", "Bolulder 30", choices = c("None","Zone","Top"),selected = userdata()$S2B30,inline = T)
              updateRadioButtons(session,"S2B31", "Bolulder 31", choices = c("None","Zone","Top"),selected = userdata()$S2B31,inline = T)
              updateRadioButtons(session,"S2B32", "Bolulder 32", choices = c("None","Zone","Top"),selected = userdata()$S2B32,inline = T)
              updateRadioButtons(session,"S2B33", "Bolulder 33", choices = c("None","Zone","Top"),selected = userdata()$S2B33,inline = T)
              updateRadioButtons(session,"S2B34", "Bolulder 34", choices = c("None","Zone","Top"),selected = userdata()$S2B34,inline = T)
              updateRadioButtons(session,"S2B35", "Bolulder 35", choices = c("None","Zone","Top"),selected = userdata()$S2B35,inline = T)
              updateRadioButtons(session,"S2B36", "Bolulder 36", choices = c("None","Zone","Top"),selected = userdata()$S2B36,inline = T)
              updateRadioButtons(session,"S2B37", "Bolulder 37", choices = c("None","Zone","Top"),selected = userdata()$S2B37,inline = T)
              updateRadioButtons(session,"S2B38", "Bolulder 38", choices = c("None","Zone","Top"),selected = userdata()$S2B38,inline = T)
              updateRadioButtons(session,"S2B39", "Bolulder 39", choices = c("None","Zone","Top"),selected = userdata()$S2B39,inline = T)
              updateRadioButtons(session,"S2B40", "Bolulder 40", choices = c("None","Zone","Top"),selected = userdata()$S2B40,inline = T)
            }else{
              if("Child" %in% settings()  ){
                updateRadioButtons(session,"S2B1", "Bolulder 01", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B1,inline = T)
                updateRadioButtons(session,"S2B2", "Bolulder 02", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B2,inline = T)
                updateRadioButtons(session,"S2B3", "Bolulder 03", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B3,inline = T)
                updateRadioButtons(session,"S2B4", "Bolulder 04", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B4,inline = T)
                updateRadioButtons(session,"S2B5", "Bolulder 05", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B5,inline = T)
                updateRadioButtons(session,"S2B6", "Bolulder 06", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B6,inline = T)
                updateRadioButtons(session,"S2B7", "Bolulder 07", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B7,inline = T)
                updateRadioButtons(session,"S2B8", "Bolulder 08", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B8,inline = T)
                updateRadioButtons(session,"S2B9", "Bolulder 09", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B9,inline = T)
                updateRadioButtons(session,"S2B10", "Bolulder 10", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B10,inline = T)
                updateRadioButtons(session,"S2B11", "Bolulder 11", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B11,inline = T)
                updateRadioButtons(session,"S2B12", "Bolulder 12", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B12,inline = T)
                updateRadioButtons(session,"S2B13", "Bolulder 13", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B13,inline = T)
                updateRadioButtons(session,"S2B14", "Bolulder 14", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B14,inline = T)
                updateRadioButtons(session,"S2B15", "Bolulder 15", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B15,inline = T)
                updateRadioButtons(session,"S2B16", "Bolulder 16", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B16,inline = T)
                updateRadioButtons(session,"S2B17", "Bolulder 17", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B17,inline = T)
                updateRadioButtons(session,"S2B18", "Bolulder 18", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B18,inline = T)
                updateRadioButtons(session,"S2B19", "Bolulder 19", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B19,inline = T)
                updateRadioButtons(session,"S2B20", "Bolulder 20", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B20,inline = T)
                updateRadioButtons(session,"S2B21", "Bolulder 21", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B21,inline = T)
                updateRadioButtons(session,"S2B22", "Bolulder 22", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B22,inline = T)
                updateRadioButtons(session,"S2B23", "Bolulder 23", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B23,inline = T)
                updateRadioButtons(session,"S2B24", "Bolulder 24", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B24,inline = T)
                updateRadioButtons(session,"S2B25", "Bolulder 25", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B25,inline = T)
                updateRadioButtons(session,"S2B26", "Bolulder 26", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B26,inline = T)
                updateRadioButtons(session,"S2B27", "Bolulder 27", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B27,inline = T)
                updateRadioButtons(session,"S2B28", "Bolulder 28", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B28,inline = T)
                updateRadioButtons(session,"S2B29", "Bolulder 29", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B29,inline = T)
                updateRadioButtons(session,"S2B30", "Bolulder 30", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B30,inline = T)
                updateRadioButtons(session,"S2B31", "Bolulder 31", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B31,inline = T)
                updateRadioButtons(session,"S2B32", "Bolulder 32", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B32,inline = T)
                updateRadioButtons(session,"S2B33", "Bolulder 33", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B33,inline = T)
                updateRadioButtons(session,"S2B34", "Bolulder 34", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B34,inline = T)
                updateRadioButtons(session,"S2B35", "Bolulder 35", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B35,inline = T)
                updateRadioButtons(session,"S2B36", "Bolulder 36", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B36,inline = T)
                updateRadioButtons(session,"S2B37", "Bolulder 37", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B37,inline = T)
                updateRadioButtons(session,"S2B38", "Bolulder 38", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B38,inline = T)
                updateRadioButtons(session,"S2B39", "Bolulder 39", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B39,inline = T)
                updateRadioButtons(session,"S2B40", "Bolulder 40", choices = c("None","Zone","Top","Zone flash","Top flash"),selected = userdata()$S2B40,inline = T)
              }
            }
            
        })
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$summary <- DT::renderDataTable({
            input$submit
            user0<-credentials()$info[[1]][1]
            table<-if(is.null(user0)){loadData()}else{
              loadData() %>% filter(user==user0)
            }
            #table<- loadData() %>% filter(user==user0)
            if(dim(table)[1]==0){
                    table<- data.frame(summary=c("Top","Zone"),result=c(0,0))
                }else{
                    cn<- (t(settings()[1,-1]))
                    table<-
                      loadData() %>% filter(user==user0)  %>% 
                        select(all_of(as.character(cn))) %>% t() %>% as.data.frame() %>%
                        group_by(V1) %>% summarise(n=n())
                      colnames(table) <- c("summary","result")
                    
                } 
            table
        })
        
        output$settings <- DT::renderDataTable({
            input$submit
            loadData_Admin() 
        })
        
        output$responses <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            DT::datatable( loadData() %>% select(user,name,sex,cat,season1,all_of(as.character(cn))),
                       extensions = "Buttons", 
                       options = list(paging = TRUE,
                                      #scrollX=TRUE, 
                                      searching = TRUE,
                                      ordering = TRUE,
                                      dom = 'Blfrtip',
                                      buttons = c('copy', 'csv', 'excel'),
                                      pageLength=10, 
                                      lengthMenu=c(10,50,100) )
            )
            
        })
        
        output$points1 <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            
            data2<-loadData() %>% 
              mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
                select(all_of(as.character(cn))) 
            tops<- as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums(na.rm = T)
            zones<- as.data.frame(ifelse(data2=="Top",1,
                                        ifelse(data2=="Zone",1,0))) %>% colSums(na.rm = T)
            tops<-1000/ifelse(tops==0,1,tops)
            zones<-500/ifelse(zones==0,1,zones)
            points<-rbind("Top"=tops,"Zone"=zones)
            cols<- colnames(points)
            DT::datatable(
                points,
                extensions = "Buttons", 
                options = list(paging = TRUE,
                               #scrollX=TRUE, 
                               searching = TRUE,
                               ordering = TRUE,
                               dom = 'Blfrtip',
                               buttons = c('copy', 'csv', 'excel'),
                               pageLength=5, 
                               lengthMenu=c(3,5,10) ) )%>%
              DT::formatRound(columns=cols, digits=1)
            
        })
        output$points2 <- DT::renderDataTable({
          input$submit
          cn<- (t(settings()[1,-1]))
          
          data2<-loadData() %>% mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
            select(all_of(as.character(cn))) 
          tops<-as.data.frame(ifelse(data2=="Top",1,
                                     ifelse(data2=="Top flash",1,0))) %>% colSums(na.rm = T)
          zones<-as.data.frame(ifelse(data2=="Top",1,
                                      ifelse(data2=="Zone",1,
                                             ifelse(data2=="Zone flash",1,
                                                    ifelse(data2=="Top flash",1,0))))) %>% colSums(na.rm = T)
          tops<-1000/ifelse(tops==0,1,tops)
          zones<-500/ifelse(zones==0,1,zones)
          topflash<-tops*(1.1)
          zoneflash<-zones*(1.1)
          points<-rbind("Top"=tops,"Zone"=zones,"Top flash"=topflash,"Zone flash"=zoneflash)
          cols<-colnames(points)
          DT::datatable(
            points,
            extensions = "Buttons", 
            options = list(paging = TRUE,
                           #scrollX=TRUE, 
                           searching = TRUE,
                           ordering = TRUE,
                           dom = 'Blfrtip',
                           buttons = c('copy', 'csv', 'excel'),
                           pageLength=5, 
                           lengthMenu=c(3,5,10) ) )%>%
            DT::formatRound(columns=cols, digits=1)
          
        })
        output$results1 <- DT::renderDataTable({
            input$submit
            cn<- (t(settings()[1,-1]))
            
            data3<- loadData() %>% 
                select(user,name,sex,cat,season1,all_of(as.character(cn)))%>% mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2)
            data2<-loadData() %>% mutate(cat2=cat) %>%
              filter(!as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
              select(all_of(as.character(cn)))
            tops<-as.data.frame(ifelse(data2=="Top",1,0)) %>% colSums(na.rm = T)
            zones<-as.data.frame(ifelse(data2=="Top",1,
                                        ifelse(data2=="Zone",1,0))) %>% colSums(na.rm = T)
            tops<-1000/ifelse(tops==0,1,tops)
            zones<-500/ifelse(zones==0,1,zones)
            points<-rbind("Top"=tops,"Zone"=zones)
            data4 <- as.data.frame(t(ifelse(t(data2)=="Top",tops,
                                            ifelse(t(data2)=="Zone",zones,0)))) 
            data4<- cbind((data3 %>% select(user,name,sex,cat,season1)),
                          #SUM=(data4 %>% rowSums()),
                          (data4 %>% rowwise %>%
                                 mutate(!! "SUM_top10" := sum(head(sort(c_across(starts_with("S2B")), decreasing = TRUE), 10))) %>% 
                                 ungroup %>% select(SUM_top10)),
                          data4 ) %>% arrange(sex,cat,desc(SUM_top10))
            cols<- colnames(data4)[-c(1:5)]
            DT::datatable(data4,
                          extensions = "Buttons", 
                          options = list(paging = TRUE,
                                         #scrollX=TRUE, 
                                         searching = TRUE,
                                         ordering = TRUE,
                                         dom = 'Blfrtip',
                                         buttons = c('copy', 'csv', 'excel'),
                                         pageLength=10, 
                                         lengthMenu=c(10,50,100) ))  %>% 
                DT::formatStyle( 'SUM_top10', backgroundColor = 'yellow' )%>%
              DT::formatRound(columns=cols, digits=1)
        })
        
        output$results2 <- DT::renderDataTable({
          input$submit
          cn<- (t(settings()[1,-1]))
          
          data3<- loadData() %>% 
            select(user,name,sex,cat,season1,all_of(as.character(cn)))%>%mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2)
          data2<-loadData() %>% mutate(cat2=cat) %>%
            filter(as.character(cat2) %in% c("U10","U12","U14","U16")) %>% select(-cat2) %>%
            select(all_of(as.character(cn)))
          tops<-as.data.frame(ifelse(data2=="Top",1,
                                     ifelse(data2=="Top flash",1,0))) %>% colSums(na.rm = T)
          zones<-as.data.frame(ifelse(data2=="Top",1,
                                      ifelse(data2=="Zone",1,
                                             ifelse(data2=="Zone flash",1,
                                                    ifelse(data2=="Top flash",1,0))))) %>% colSums(na.rm = T)
          tops<-1000/ifelse(tops==0,1,tops)
          zones<-500/ifelse(zones==0,1,zones)
          topflash<-tops*(1.1)
          zoneflash<-zones*(1.1)
          points<-rbind("Top"=tops,"Zone"=zones,"Top flash"=topflash,"Zone flash"=zoneflash)
          data4 <- as.data.frame(t(ifelse(t(data2)=="Top",tops,
                                          ifelse(t(data2)=="Zone",zones,
                                                 ifelse(t(data2)=="Top flash",topflash,
                                                        ifelse(t(data2)=="Zone flash",zoneflash,0))))))
          data4<- cbind((data3 %>% select(user,name,sex,cat,season1)),
                        #SUM=(data4 %>% rowSums()),
                        (data4 %>% rowwise %>%
                           mutate(!! "SUM_top10" := sum(head(sort(c_across(starts_with("S2B")), decreasing = TRUE), 10))) %>% 
                           ungroup %>% select(SUM_top10)),
                        data4 ) %>% arrange(sex,cat,desc(SUM_top10))
          cols<- colnames(data4)[-c(1:5)]
          DT::datatable(data4,
                        extensions = "Buttons", 
                        options = list(paging = TRUE,
                                       #scrollX=TRUE, 
                                       searching = TRUE,
                                       ordering = TRUE,
                                       dom = 'Blfrtip',
                                       buttons = c('copy', 'csv', 'excel'),
                                       pageLength=10, 
                                       lengthMenu=c(10,50,100) ))  %>% 
            DT::formatStyle( 'SUM_top10', backgroundColor = 'yellow' )%>%
            DT::formatRound(columns=cols, digits=1)
        })
        
        output$user_table <- renderTable({
            # use req to only render results when credentials()$user_auth is TRUE
            req(credentials()$user_auth)
            credentials()$info[1,c(1,3,4)]
        })
    }
    

# Run the application 
shinyApp(ui = ui, server = server)



