# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#### TITLE: Twitter ShinyApp (ui + server) ####
#### CREATION DATE: 23-10-2018             ####
#### LAST MODIFICATION: 04-04-2019         ####

#### Reddit ShinyApp functions ####
#
# This is a Shiny web application. 
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(shiny)
require(Matrix)
require(rjson)
require(jsonlite)
require(httr)
require(rvest)
require(tidyverse)
require(shinyjs)
require(DT)
require(openxlsx)
require(quanteda)
require(rtweet)
require(stringr)
require(tibble)
require(rtweet)
require(shinythemes)
require(ggplot2)
require(quanteda)


#### FUNCTIONS ####

search_tweets=function(app,
         consumer_key,
         consumer_secret,
         access_token,
         access_secret,
         query="",
         n=100,
         no_rt=F,
         lang="en",
         geocode = "",
         type="recent") {
    
    token = rtweet::create_token(
        app = app,
        consumer_key = consumer_key,
        consumer_secret = consumer_secret,
        access_token = access_token,
        access_secret = access_secret)
    
    n=as.integer(n)
    query=as.character(query)
    geocode=as.character(geocode)
    
    if(no_rt!=0 & no_rt!=F & no_rt!="F" & no_rt!="FALSE" & no_rt!="false" & no_rt!="f" & no_rt!="0" & no_rt!=T & no_rt!="T" & no_rt!="TRUE"  & no_rt!="true" & no_rt!="t" & no_rt!="1" & no_rt!=1){message("ERROR: please enter a boolean value for no_rt parameter")}
    if(no_rt=="F" | no_rt=="no" |no_rt=="n" | no_rt=="FALSE" |no_rt=="false" | no_rt=="f" | no_rt=="0"){no_rt=F}
    if(no_rt=="T" | no_rt=="y" | no_rt=="yes" |no_rt=="TRUE" |no_rt=="true" | no_rt=="t" | no_rt=="1"){no_rt=T}
    if(query==""|is.na(query)){
        query=NULL 
    }
    
    if(geocode==""|is.na(geocode)){
        geocode=NULL   
    }
    if(lang==""|is.na(lang)){
        lang=NULL   
    }
    if(!is.null(lang)){
        if(!(lang %in%  rtweet::langs$alpha)){
            stop("please enter a valid lananguage code") 
        }
    }
    if(n>18000){
        n=18000
    }
    results= rtweet::search_tweets(q=query,
                                       n = n,
                                       type = type,
                                       include_rts = !no_rt,
                                       lang = lang,
                                       geocode = geocode,
                                       max_id = NULL,
                                       parse = TRUE,
                                       token = token,
                                       retryonratelimit = FALSE,
                                       verbose = TRUE
        )
    return(results)
}

#Process text

process_text=function(twitter_dataset, h = TRUE, f = TRUE, u = TRUE){
    
    enc2utf8(as(twitter_dataset$text, "character")) 
    enc2utf8(as(twitter_dataset$description, "character"))  
    
    #creates column and eliminates all non ASCII characters 
    twitter_dataset$clean_text = iconv(twitter_dataset$text,  to = "ASCII", from = "UTF-8" ,sub=' ')
    
    
    #extract urls
    twitter_dataset$text_urls = regmatches(twitter_dataset$clean_text,gregexpr("(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,9}[^ [:punct:]])",twitter_dataset$clean_text,perl = TRUE))
    
    #remove urls
    twitter_dataset$clean_text = gsub("(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,9}[^ [:punct:]])"," ",twitter_dataset$clean_text,perl = TRUE)
    
    #extract user tags @[tag]
    twitter_dataset$text_user_tags = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([@]{1}[a-zA-Z0-9_]{1,})",twitter_dataset$clean_text,perl = TRUE)), tolower)
    
    #(optional) remove usertags tagging character (@)
    if(u == T){twitter_dataset$clean_text = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([@]{1})(?=[a-zA-Z0-9_]{1,})",twitter_dataset$clean_text,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract topic tags #[tag]
    twitter_dataset$text_hash_tags = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([#]{1}[a-zA-Z0-9_]{1,})",twitter_dataset$clean_text,perl = TRUE)), tolower)
    
    #(optional) remove hashtags tagging character (#)
    if(h == T){twitter_dataset$clean_text = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([#]{1})(?=[a-zA-Z0-9_]{1,})",twitter_dataset$clean_text,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract finance tags $[tag]
    twitter_dataset$text_fin_tags = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([$]{1}[A-Za-z]{2,8})",twitter_dataset$clean_text,perl = TRUE)), tolower)
    
    #(optional) remove fintags tagging character ($)
    if(f == T){twitter_dataset$clean_text = sapply(regmatches(twitter_dataset$clean_text,gregexpr("([$]{1})(?=[A-Za-z]{2,8})",twitter_dataset$clean_text,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract  standard character emoticones
    twitter_dataset$text_emoticones = regmatches(twitter_dataset$clean_text,gregexpr(pattern ="([>]{0,1}[:;=][']{0,1}[c\\-っ\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-っ\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)",twitter_dataset$clean_text,perl = TRUE))
    
    #remove emoticones
    twitter_dataset$clean_text= gsub(pattern ="([>]{0,1}[:;=][']{0,1}[c\\-っ\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-っ\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)"," ",twitter_dataset$clean_text,perl = TRUE)
    
    #remove all characters that are not alphanumeric or ponctuation
    twitter_dataset$clean_text = stringr::str_replace_all(twitter_dataset$clean_text,"[^[:punct:][:alnum:]]", " ")
    
    #clean multiple/trailing/leading spaces 
    twitter_dataset$clean_text = gsub("[[:space:]]{1,}", " ",twitter_dataset$clean_text, ignore.case = FALSE, perl = TRUE)
    twitter_dataset$clean_text = gsub("^\\s+|\\s+$", "",twitter_dataset$clean_text, ignore.case = FALSE, perl = TRUE)
    
    return(twitter_dataset)
}

# Processes description
process_description=function(twitter_dataset, h = TRUE, f = TRUE, u = TRUE){
    
    #creates column and eliminates all non ASCII characters
    twitter_dataset$clean_description = iconv(twitter_dataset$description,  to = "ASCII", from = "UTF-8" ,sub=' ')
    
    #extract urls
    twitter_dataset$description_urls = regmatches(twitter_dataset$clean_description,gregexpr("(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,9}[^ [:punct:]])",twitter_dataset$clean_description,perl = TRUE))
    
    #remove urls
    twitter_dataset$clean_description = gsub("(http[s]?:?\\/?\\/?t?\\.?c?o?\\/?[A-Za-z0-9\\.\\/]{0,9}[^ [:punct:]])"," ",twitter_dataset$clean_description,perl = TRUE)
    
    #extract user tags @[tag]
    twitter_dataset$description_user_tags = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([@]{1}[a-zA-Z0-9_]{1,})",twitter_dataset$clean_description,perl = TRUE)), tolower)
    
    #(optional) remove usertags tagging character (@)
    if(u == T){twitter_dataset$clean_description = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([@]{1})(?=[a-zA-Z0-9_]{1,})",twitter_dataset$clean_description,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract topic tags #[tag]
    twitter_dataset$description_hash_tags = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([#]{1}[a-zA-Z0-9_]{1,})",twitter_dataset$clean_description,perl = TRUE)), tolower)
    
    #(optional) remove hashtags tagging character (#)
    if(h == T){twitter_dataset$clean_description = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([#]{1})(?=[a-zA-Z0-9_]{1,})",twitter_dataset$clean_description,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract finance tags $[tag]
    twitter_dataset$description_fin_tags = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([$]{1}[A-Za-z]{2,8})",twitter_dataset$clean_description,perl = TRUE)), tolower)
    
    #(optional) remove fintags tagging character ($)
    if(f == T){twitter_dataset$clean_description = sapply(regmatches(twitter_dataset$clean_description,gregexpr("([$]{1})(?=[A-Za-z]{2,8})",twitter_dataset$clean_description,perl = TRUE),invert = TRUE),paste0, collapse = " ")}
    
    #extract  standard character emoticones
    twitter_dataset$description_emoticones = regmatches(twitter_dataset$clean_description,gregexpr(pattern ="([>]{0,1}[:;=][']{0,1}[c\\-っ\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-っ\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)",twitter_dataset$clean_description,perl = TRUE))
    
    #remove emoticones
    twitter_dataset$clean_description= gsub(pattern ="([>]{0,1}[:;=][']{0,1}[c\\-っ\\^]{0,1}[()\\]\\|][()\\]\\|]?)|(\\s[3>]?[;=X:8][']{0,1}[c\\-っ\\^]?[()\\]SsbpPDBOo0\\|][()\\]\\|]?\\s)"," ",twitter_dataset$clean_description,perl = TRUE)
    
    #remove all characters that are not alphanumeric or ponctuation
    twitter_dataset$clean_description = stringr::str_replace_all(twitter_dataset$clean_description,"[^[:punct:][:alnum:]]", " ")
    
    #clean multiple/trailing/leading spaces
    twitter_dataset$clean_description = gsub("[[:space:]]{1,}", " ",twitter_dataset$clean_description, ignore.case = FALSE, perl = TRUE)
    
    twitter_dataset$clean_description  = gsub("^\\s+|\\s+$", "",twitter_dataset$clean_description , ignore.case = FALSE, perl = TRUE)
    
    return(twitter_dataset)
}


#writer for json files
write_json = function(df, file, df_type = "rows", raw_type = "mongo", digits= NA){
    require(readr)
    require(jsonlite)
    df %>% 
        jsonlite::toJSON(dataframe = df_type, raw = raw_type, digits = digits, pretty = TRUE) %>%
        readr::write_lines(file)
}
# Define UI for application that draws a histogram
shinyApp(
    {
        fluidPage(
            useShinyjs(),
            theme = shinytheme("journal"),
            navbarPage(
                "Twitter Analysis Toolset By Carlo Santagiustina",
                id = "TW_search_tabs",
                tabPanel(
                    "Download",
                    fluidPage(
                        sidebarLayout(
                            ####1° level UI tab: authentication####
                            sidebarPanel(                            actionButton("TW_search_update", "Download Tweets from Search API", class = "btn-primary"),
                                                                     textInput(inputId = "TW_search_q",
                                                                               "Search terms:",
                                                                               "climate")
                                                                     ,
                                                                     # Text box
                                                                     textInput(inputId = "TW_search_l",
                                                                               "Language:",
                                                                               value = "en")
                                                                     ,
                                                                     sliderInput(
                                                                         inputId = "TW_search_n",
                                                                         "Number of Tweets:",
                                                                         min = 100,
                                                                         max = 18000,
                                                                         value = 100,
                                                                         step=100
                                                                     )
                                                                     ,
                                                                     textInput(inputId = "TW_search_geo",
                                                                               "Geo filtration:")
                                                                     ,
                                                                     checkboxInput(
                                                                         inputId =  "TW_search_no_rt",
                                                                         "Eliminate Retweets",
                                                                         value = F
                                                                     ),
                                                                     checkboxInput(
                                                                         inputId =  "TW_search_show_credentials",
                                                                         "Show authentication credentials",
                                                                         value = F
                                                                     ),
                                                                     conditionalPanel(
                                                                         condition = "input.TW_search_show_credentials == true"
                                                                         ,
                                                                         div(
                                                                             passwordInput(inputId = "TW_oauth_app",
                                                                                           "Twitter App Name:","PASTE HERE YOUR APP NAME")
                                                                             ,passwordInput(inputId = "TW_oauth_ck",
                                                                                            "Twitter Consumer Key:","PASTE HERE YOUR CONSUMER KEY")
                                                                             ,passwordInput(inputId = "TW_oauth_cs",
                                                                                            "Twitter Consumer Secret:","PASTE HERE YOUR CONSUMER SECRET")
                                                                             ,passwordInput(inputId = "TW_oauth_ac",
                                                                                            "Twitter Access Token:","PASTE HERE YOUR ACCESS TOKEN")
                                                                             ,passwordInput(inputId = "TW_oauth_as",
                                                                                            "Twitter Access Secret:","PASTE HERE YOUR ACESS SECRET")))
                            ),
                            mainPanel(
                                verbatimTextOutput("TW_search_observationsCount")
                                ,
                                br()
                                ,
                                div(style="display:inline-block",downloadButton("TW_search_downloadData", "Download CSV"),width=6),
                                div(style="display:inline-block",downloadButton("TW_search_downloadDataExcel", "Download Excel")),
                                div(style="display:inline-block",downloadButton("TW_search_downloadDataJSON", "Download JSON")),
                                br()
                                ,
                                DT::DTOutput("TW_search_rendered_observations")
                                
                            )
                        )
                    )
                )
                ####1° level UI tab: explorative analysis####
                ,
                tabPanel(
                    "Explorative Analysis",
                    fluidPage(navbarPage(
                        ####2° level UI tab: summary statistics ####
                        "Tweets Summary:",
                        tabPanel(              
                            ####3° level UI tab: stats by variable ####
                            "STATS BY VARIABLE",
                            useShinyjs()
                            ,
                            sidebarLayout(
                                # Sidebar with a slider and selection inputs
                                sidebarPanel(
                                    uiOutput('TW_search_summary_select')
                                )
                                ,
                                mainPanel(navbarPage(
                                    "OUTPUTS:",
                                    tabPanel("SUMMARY",
                                             strong("Summary Statistics"),
                                             br(),
                                             htmlOutput('TW_search_summary'),
                                             br(),
                                             strong("Most Frequently Observed Values"),
                                             br(),
                                             DT::DTOutput('TW_search_table')
                                    )
                                )
                                )
                            )
                        )
                        ,
                        ####3° level UI tab: time series ####
                        tabPanel("TIME SERIES",
                                 useShinyjs()
                                 ,
                                 sidebarLayout(
                                     # Sidebar with a slider and selection inputs
                                     sidebarPanel(
                                         textInput(inputId = "TW_search_timeseries_breaks","frequency of breaks for aggregation",value="mins"),
                                         numericInput("TW_search_timeseries_filter", "Remove groups with less than this number of observations", "10", step = 1),
                                         uiOutput(outputId="TW_search_timeseries_group_by")
                                         
                                     )
                                     ,
                                     mainPanel(navbarPage(
                                         "PLOTS:",
                                         tabPanel("N. TWEETS",
                                                  plotOutput("TW_search_timeseries")          
                                         ),
                                         tabPanel("N. TWEETS GROUPED BY",
                                                  plotOutput("TW_search_timeseries_grouped")          
                                         )
                                     )
                                     )
                                 )
                        )
                        ,
                        ####3° level UI tab: sources users urls ####
                        # tabPanel("ANALYSIS OF SOURCES, USERS AND URLS",
                        #          useShinyjs()
                        #          ,
                        #          sidebarLayout(
                        #              # Sidebar with a slider and selection inputs
                        #              sidebarPanel(
                        #                  
                        #              )
                        #              ,
                        #              mainPanel(navbarPage(
                        #                  "PLOTS:",
                        #                  tabPanel("ANALYSIS OF SOURCES AND URLS"
                        #                           # ,strong("Service used to upload tweet"),
                        #                           # br(),
                        #                           # DT::DTOutput('TW_search_source'),
                        #                           # br(),
                        #                           # strong("SMentioned URLs, domains, etc..."),
                        #                           # br(),
                        #                           # DT::DTOutput('TW_search_URLs')
                        #                  )
                        #              )
                        #              )
                        #          )
                        # )
                        # ,
                        ####3° level UI tab: KWIC ####
                        tabPanel("KWIC",
                                 useShinyjs()
                                 ,
                                 sidebarLayout(
                                     # Sidebar with a slider and selection inputs
                                     div(id = "TW_search_Sidebar2"
                                         , sidebarPanel(selectInput("TW_search_kwic_variable", div(h3("KWIC:"), h5("Variable")),
                                                                    choices =   c("Texts of Tweets [text]" = "text",
                                                                                  "Users Descriptions [description]" = "description"), selected = "text"),
                                                        textInput(inputId = "TW_search_kwic_pattern",
                                                                  h5("search keywords context in corpus (excluding retweets)")),
                                                        sliderInput(
                                                            inputId = "TW_search_kwic_window",
                                                            h3("(left/right) window size:"),
                                                            min = 1,
                                                            max = 20,
                                                            value = 10,
                                                            step=1
                                                        ),
                                                        checkboxInput(
                                                            inputId =  "TW_search_kwic_caseinsensitive",
                                                            "case insensitive pattern matching",
                                                            value = F
                                                        )
                                                        
                                         ))
                                     ,
                                     mainPanel(navbarPage(
                                         "KWIC Outputs:",tabPanel(
                                             "Context",
                                             DT::DTOutput("TW_search_kwicTable")),
                                         tabPanel(
                                             "Most Frequent Preceeding Pattern",DT::DTOutput("TW_search_kwicPreTable"))
                                         ,
                                         tabPanel(
                                             "Most Frequent Following Pattern", DT::DTOutput("TW_search_kwicPostTable"))
                                     )
                                     )
                                 )
                        )
                    )#bracket of "STATISTICS" tabsets
                    ))
                ,
                ####2° level UI tab: textual data processing [text and/or description]####
                tabPanel(
                    "Process Tweets",
                    fluidPage(
                        useShinyjs()
                        ,
                        sidebarLayout(
                            # Sidebar with a slider and selection inputs
                            div(id = "TW_search_Sidebar3"
                                ,
                                sidebarPanel(
                                    actionButton("TW_search_descriptionandtext_preprocess",label = "Process selected columns", class = "btn-primary"),
                                    checkboxInput("TW_search_process_text",strong("PROCESS AND CLEAN TEXT COLUMN"),value=TRUE),
                                    HTML("If ticked will process text column:<br /> <ul> - clean text from non ASCII characters <br> - extract URLs <br> - extract tagged terms (#,@, $) <br> - extract  emoticones <br></ul>")
                                    ,
                                    conditionalPanel(condition = "input.TW_search_process_text == true",
                                                     div(HTML(" and...<br>
                                         <ul>"),
                                                         checkboxInput("TW_search_text_hash_remove", " remove hash tag characters  (#)", value = TRUE, width = NULL),
                                                         checkboxInput("TW_search_text_fin_remove", " remove financial tag characters  ($)", value = TRUE, width = NULL),
                                                         checkboxInput("TW_search_text_user_remove", " remove user tag characters (@)", value = TRUE, width = NULL),
                                                         HTML("</ul>")
                                                     )
                                                     
                                    )
                                    ,
                                    br(),
                                    checkboxInput("TW_search_process_description",strong("PROCESS AND CLEAN DESCRIPTION COLUMN"),value=TRUE),
                                    HTML("If ticked will process description column:<br /> <ul> - clean text from special (non ASCII) characters <br> - extract URLs <br> - extract tagged terms (#,@, $) <br> - extract  emoticones <br></ul>"),
                                    conditionalPanel(condition = "input.TW_search_process_description == true",
                                                     
                                                     div(HTML(" and...<br>
                                         <ul>"),
                                                         checkboxInput("TW_search_description_hash_remove", " remove hash tag characters (#)",value = TRUE, width = NULL),
                                                         checkboxInput("TW_search_description_fin_remove", " remove financial tag characters ($)",value = TRUE, width = NULL),
                                                         checkboxInput("TW_search_description_user_remove", " remove user tag characters (@)",value = TRUE, width = NULL),
                                                         HTML("</ul>")))
                                    
                                )
                            )
                            ,
                            mainPanel("New columns called 'clean_text' and 'clean_description' contain respectively processed Tweets' texts and descriptions",
                                      br()
                                      ,div(style="display:inline-block",downloadButton("TW_search_downloadDataProcessed", "Download CSV"),width=6),
                                      div(style="display:inline-block",downloadButton("TW_search_downloadDataProcessedExcel", "Download Excel")),
                                      div(style="display:inline-block",downloadButton("TW_search_downloadDataProcessedJSON", "Download JSON")),
                                      br()
                                      ,
                                DT::DTOutput("TW_search_rendered_preprocessed")
                            )
                        )
                    )
                )
                ,
                ####2° level UI tab: tweet text analysis ####
                tabPanel(
                    "Text Analysis",
                    fluidPage(
                        useShinyjs()
                        ,
                        navbarPage(
                            "Analysis of:",
                            tabPanel(
                                "Hashtags",
                                mainPanel(
                                    DT::DTOutput("TW_search_text_hash_tags_top100"),
                                    br(),
                                    plotOutput("TW_search_text_hashtags_cooc_network"),
                                    br(),
                                    plotOutput("TW_search_text_hashtags_wordcloud")
                                )),
                            tabPanel(
                                "Usertags",
                                mainPanel(
                                    DT::DTOutput("TW_search_text_user_tags_top100"),
                                    br(),
                                    plotOutput("TW_search_text_usertags_cooc_network"),
                                    br(),
                                    plotOutput("TW_search_text_usertags_wordcloud")))
                        )
                    )
                )        ,
                ####2° level UI tab: user description analysis ####
                tabPanel(
                    "Description Analysis",
                    fluidPage(
                        useShinyjs()
                        ,
                        navbarPage(
                            "Analysis of:",
                            tabPanel(
                                "Hashtags",
                                mainPanel(
                                    DT::DTOutput("TW_search_description_hash_tags_top100"),
                                    br(),
                                    plotOutput("TW_search_description_hashtags_cooc_network"),
                                    br(),
                                    plotOutput("TW_search_description_hashtags_wordcloud")
                                )),
                            tabPanel(
                                "Usertags",
                                mainPanel(
                                    DT::DTOutput("TW_search_description_user_tags_top100"),
                                    br(),
                                    plotOutput("TW_search_description_usertags_cooc_network"),
                                    br(),
                                    plotOutput("TW_search_description_usertags_wordcloud")))
                        )
                    )
                )
                #end 2° level tabs
            )
        )
        
    },
    function(input, output, session) {
        
        #####options at initialisation####
        
        ##disable specific commands and options
        {
            disable("TW_search_downloadData")
            disable("TW_search_downloadDataJSON")
            disable("TW_search_downloadDataExcel")
        }
        ##disable specific tabsets
        {
            
        }
        ##observe specific javascript events  
        {  
            # observeEvent(input$TW_search_toggleSidebar0, {
            #       shinyjs::toggle(id = "TW_search_Sidebar0")
            #   })
            #   
            #   
            #   observeEvent(input$TW_search_toggleSidebar, {
            #       shinyjs::toggle(id = "TW_search_Sidebar")
            #   })
        }
        
        # observe({
        #     hide(selector = "#TW_search_tabs li a[data-value=\"Load Tweets\"]")
        # })
        # observe({
        #     hide(selector = "#TW_search_tabs li a[data-value=\"Process Tweets\"]")
        # })
        
        
        
        
        
        ####search for tweets#####
        TW_search_observations = eventReactive(input$TW_search_update, {
            withProgress({
                setProgress(message = "Gathering tweets...")
                search_tweets(app = input$TW_oauth_app,
                                            consumer_key = input$TW_oauth_ck,
                                            consumer_secret = input$TW_oauth_cs,
                                            access_token = input$TW_oauth_ac,
                                            access_secret=input$TW_oauth_as,
                                            query=input$TW_search_q,
                                            n=input$TW_search_n,
                                            no_rt=input$TW_search_no_rt,
                                            lang=input$TW_search_l,
                                            geocode = input$TW_search_geo,
                                            type="recent")
            })
        })
        
        output$TW_search_rendered_observations = DT::renderDT(
            as.data.frame(TW_search_observations()),
            options = list(
                lengthChange = FALSE,
                searching = FALSE,
                pageLength = 15
            )
        )
        
        output$TW_search_observationsCount = shiny::renderText({
            TW_search_df = TW_search_observations()
            enable("TW_search_downloadData")
            enable("TW_search_downloadDataJSON")
            enable("TW_search_downloadDataExcel")
            paste("Number of Tweets Found: ", as.character(nrow(TW_search_df)))
        })
        ####download tweets####
        ####csv####
        output$TW_search_downloadData <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data ",
                      input$TW_search_q,
                      
                      " ",
                      Sys.time(),
                      ".csv",
                      sep = "")
            },
            content = function(file) {
                rtweet::write_as_csv(TW_search_observations(), file_name=file, na = "", prepend_ids = TRUE, fileEncoding = "UTF-8")                     # put Data() into the download file
            }
        )
        ####excel####
        output$TW_search_downloadDataExcel <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data ",
                      input$TW_search_q,
                      
                      " ",
                      Sys.time(),
                      ".xlsx",
                      sep = "")
            },
            content = function(file) {
                openxlsx::write.xlsx(rtweet::flatten(TW_search_observations()), file=file, sheetName="raw tweets", colNames = TRUE, rowNames=FALSE, borders = "surrounding", colWidths="auto", overwrite=FALSE, keepNA=FALSE, zoom=50, creator="CarloWebDataApp")                     # put Data() into the download file
            }
        )
        ####json####
        output$TW_search_downloadDataJSON <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data ",
                      input$TW_search_q,
                      
                      " ",
                      Sys.time(),
                      ".json",
                      sep = "")
            },
            content = function(file) {
                write_json(TW_search_observations(), file=file)                     # put Data() into the download file
            })
        
        #### preliminary analysis ####
        ##interactive UI elements
        {
            output$TW_search_summary_select <- renderUI({
                df <- TW_search_observations()
                selectInput("TW_search_summary_variable", "Variable:",choices=names(df))
            })
            
            output$TW_search_timeseries_group_by <- renderUI({
                df=TW_search_observations()
                selectInput(inputId = "TW_search_timeseries_variable", label = "Group by variable",choices = names(df),selected = "is_retweet")})
            
            # TW_search_summary_numeric = reactive({
            # classes=sapply(TW_search_observations(), class)
            # which(classes=="numeric")
            # }
            # )
        }
        ####render summary#### 
        output$TW_search_table = DT::renderDT({
            TW_search_observations() %>% group_by(UQ(as.name(input$TW_search_summary_variable))) %>% select(UQ(as.name(input$TW_search_summary_variable))) %>% summarize(Count=n()) %>% 
                mutate(Percent = round((Count/sum(Count)*100))) %>%
                arrange(desc(Count))
        },
        options = list(
            lengthChange = FALSE,
            searching = TRUE,
            pageLength = 15
        )
        ) 
        
        
        output$TW_search_timeseries = renderPlot({
            df = TW_search_observations() 
            df %>% filter(n() >= input$TW_search_timeseries_filter) %>%
                rtweet::ts_plot(input$TW_search_timeseries_breaks) +
                ggplot2::theme_gray(base_family = 'serif') +
                ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), axis.text.x = element_text(angle = 90, hjust = 2)) +
                ggplot2::scale_x_datetime(date_breaks = input$TW_search_timeseries_breaks) +
                ggplot2::labs(
                    x = NULL, y = NULL,
                    title = paste("Time series of tweet counts", sep = ""),
                    subtitle = paste("Tweet counts aggregated at",input$TW_search_timeseries_breaks,"frequency", sep = " "),
                    caption = "\nSource: Data collected from Twitter's Search API"
                )
            
        }
        )
        
        output$TW_search_timeseries_grouped = renderPlot({
            df = TW_search_observations() 
            df %>% 
                group_by(UQ(as.name(input$TW_search_timeseries_variable)))  %>%
                filter(n() >= input$TW_search_timeseries_filter) %>%
                rtweet::ts_plot(input$TW_search_timeseries_breaks) +
                ggplot2::theme_gray(base_family = 'serif') +
                ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"), axis.text.x = element_text(angle = 90, hjust = 2)) +
                ggplot2::scale_x_datetime(date_breaks = input$TW_search_timeseries_breaks) +
                ggplot2::labs(
                    x = NULL, y = NULL,
                    title = paste("Time series of tweet counts", sep = ""),
                    subtitle = paste("Tweet counts aggregated at",input$TW_search_timeseries_breaks,"frequency and grouped by",input$TW_search_timeseries_variable, sep = " "),
                    caption = "\nSource: Data collected from Twitter's Search API"
                )
            
        }
        )
        
        output$TW_search_summary = renderPrint(summary(TW_search_observations()  %>% select(UQ(as.name(input$TW_search_summary_variable))) %>% unnest()))
        
        
        # output$TW_search_summary_plot <- renderPlot({
        #     if(!is.numeric(TW_search_observations()[[,input$TW_search_summary_variable]]) & !is.integer(TW_search_observations()[[,input$TW_search_summary_variable]])){return(NULL)}
        #     df <- TW_search_observations()
        #     df <- df[[,input$TW_search_summary_variable]]
        #     hist(df)
        # })        
        
        
        ####kwic#### 
        TW_search_kwic =  reactive(
            TW_search_observations() %>% filter(is_retweet == FALSE) %>% 
                quanteda::corpus(docid_field= "status_id" , text_field = input$TW_search_kwic_variable) %>%
                quanteda::tokens(what="word",  remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, remove_separators = FALSE, remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = TRUE, concatenator = " ", quanteda_options("verbose"), include_docvars = TRUE, ngrams=1) %>%
                kwic(pattern=phrase(input$TW_search_kwic_pattern),valuetype = "glob", case_insensitive = input$TW_search_kwic_caseinsensitive, window=input$TW_search_kwic_window)
        )
        
        TW_search_byusers_tweets = reactive({
            df=TW_search_observations()
            df %>%
                group_by(screen_name) %>%
                summarise(UQ(as.name(input$TW_search_kwic_variable)):= paste0(sprintf('%s',input$TW_search_kwic_variable, screen_name), collapse = " /n "))
            
        }
        )
        
        output$TW_search_byusers_tweets_xray =  renderPlot({
            df2 = TW_search_byusers_tweets()
            df2 %>%
                quanteda::corpus(docid_field= "status_id" , text_field = input$TW_search_kwic_variable) %>%
                quanteda::tokens(what="word",  remove_punct = TRUE, remove_numbers=TRUE, remove_symbols = TRUE, remove_separators = FALSE, remove_twitter = FALSE, remove_hyphens = FALSE, remove_url = TRUE, concatenator = " ", quanteda_options("verbose"), include_docvars = TRUE, ngrams=1) %>%
                kwic(pattern=phrase(input$TW_search_kwic_pattern),valuetype = "glob", case_insensitive = input$TW_search_kwic_caseinsensitive, window=input$TW_search_kwic_window)  %>%
                quanteda::textplot_xray()
        })
        
        output$TW_search_kwicTable <- DT::renderDT({
            TW_search_kwic() 
        },
        options = list(searching = FALSE)
        )
        
        output$TW_search_kwicPreTable <- DT::renderDT({
            TW_search_kwic() %>% count(pre, sort = TRUE)  
        },
        options = list(searching = TRUE)
        )
        
        output$TW_search_kwicPostTable <- DT::renderDT({
            TW_search_kwic() %>% count(post, sort = TRUE)
        },
        options = list(searching = TRUE)
        )
        #### processing text and/or description####
        TW_search_observations_processed = eventReactive(input$TW_search_descriptionandtext_preprocess,
                                                         withProgress({
                                                             setProgress(message = "processing tweets [text and/or description columns]...")
                                                             df=TW_search_observations()
                                                             #process text column
                                                             if(input$TW_search_process_text){
                                                                 df = process_text(twitter_dataset = df, h = input$TW_search_text_hash_remove, f = input$TW_search_text_fin_remove, u = input$TW_search_text_user_remove)}
                                                             #process description column
                                                             if(input$TW_search_process_description){
                                                                 df = process_description(twitter_dataset = df, h = input$TW_search_description_hash_remove, f = input$TW_search_description_fin_remove, u = input$TW_search_description_user_remove)
                                                             }
                                                             df
                                                         }))
        
        
        output$TW_search_rendered_preprocessed = DT::renderDT(
            as.data.frame(TW_search_observations_processed()),
            options = list(
                lengthChange = FALSE,
                searching = FALSE,
                pageLength = 15
            )
        )
        ####download processed tweets####
        ####csv####
        output$TW_search_downloadDataProcessed <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data_processed ",
                      input$TW_search_q,
                      
                      " ",
                      Sys.time(),
                      ".csv",
                      sep = "")
            },
            content = function(file) {
                rtweet::write_as_csv(TW_search_observations_processed(), file_name=file, na = "", prepend_ids = TRUE, fileEncoding = "UTF-8")                     # put Data() into the download file
            }
        )
        ####excel####
        output$TW_search_downloadDataProcessedExcel <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data_processed ",
                      input$TW_search_q,
                      
                      " ",
                      Sys.time(),
                      ".xlsx",
                      sep = "")
            },
            content = function(file) {
                openxlsx::write.xlsx(rtweet::flatten(TW_search_observations_processed()), file=file, sheetName="processed tweets", colNames = TRUE, rowNames=FALSE, borders = "surrounding", colWidths="auto", overwrite=FALSE, keepNA=FALSE, zoom=50, creator="CarloSantagiustinaWebDataApp")                     # put Data() into the download file
            }
        )
        ####json####
        output$TW_search_downloadDataProcessedJSON <- downloadHandler(
            # Create the download file name
            filename  = function() {
                paste("twitter_search_data_processed ",
                      input$TW_search_q,
                      " ",
                      Sys.time(),
                      ".json",
                      sep = "")
            },
            content = function(file) {
                write_json(TW_search_observations_processed(), file=file)                     # put Data() into the download file
            })
        
        ####text tags analysis####
        #hashtags (#)
        TW_search_text_hashtags_dfm = reactive(
            if(input$TW_search_process_text){                                     
                TweetsAll = TW_search_observations_processed()
                all_hash_tags = unique(unlist(TweetsAll$text_hash_tags))#unlist hashtags   
                text_hash_tags_n_unique = sapply(sapply(TweetsAll$text_hash_tags,unique), length)#compute number of unique hastags per document/tweet
                text_hash_tags_rows = unlist(mapply(rep, times = text_hash_tags_n_unique, x = 1:nrow(TweetsAll)), recursive = FALSE)#document (row) identifiers  
                text_hash_tags_columns = unlist(sapply(TweetsAll$text_hash_tags, function(x){which(all_hash_tags %in% x)}), recursive = FALSE)#hash tags (columns) identifiers
                text_hash_tags_dfm = quanteda::as.dfm(
                    Matrix::sparseMatrix(
                        text_hash_tags_rows, text_hash_tags_columns, dims=c(nrow(TweetsAll), length(all_hash_tags)), dimnames = list(TweetsAll$id,all_hash_tags)
                    )
                )
            } else{print("the column named text hasn't yet been processed, please process it to visualize the text analysis outputs")}
        )
        TW_search_text_hashtags_top100=reactive({
            top100=as.data.frame(quanteda::topfeatures(TW_search_text_hashtags_dfm(),n= 100))
            colnames(top100)=c("frequency")
            top100
        }
        )
        
        output$TW_search_text_hash_tags_top100 = DT::renderDT(
            TW_search_text_hashtags_top100(),
            options = list(
                lengthChange = FALSE,
                searching = TRUE,
                pageLength = 10
            )
        )
        
        
        TW_search_text_hashtags_cooc_matrix = reactive({
            text_hash_tags_fcm = quanteda::fcm(TW_search_text_hashtags_dfm())
            toptag_fcm = quanteda::fcm_select(text_hash_tags_fcm, pattern = rownames(TW_search_text_hashtags_top100()))
        })
        
        output$TW_search_text_hashtags_cooc_network = renderPlot({
            quanteda::textplot_network(TW_search_text_hashtags_cooc_matrix(),min_freq=2,omit_isolated=T, edge_alpha = 0.6, edge_size = 0.7)
        })
        
        output$TW_search_text_hashtags_wordcloud = renderPlot({
            quanteda::textplot_wordcloud(TW_search_text_hashtags_dfm(), min_count = 2, random_order = FALSE,rotation = .25,  color = RColorBrewer::brewer.pal(8,"Dark2"))
        })
        #usertags (@)
        TW_search_text_usertags_dfm = reactive(
            if(input$TW_search_process_text){                                     
                TweetsAll = TW_search_observations_processed()
                all_user_tags = unique(unlist(TweetsAll$text_user_tags))#unlist usertags   
                text_user_tags_n_unique = sapply(sapply(TweetsAll$text_user_tags,unique), length)#compute number of unique hastags per document/tweet
                text_user_tags_rows = unlist(mapply(rep, times = text_user_tags_n_unique, x = 1:nrow(TweetsAll)), recursive = FALSE)#document (row) identifiers  
                text_user_tags_columns = unlist(sapply(TweetsAll$text_user_tags, function(x){which(all_user_tags %in% x)}), recursive = FALSE)#user tags (columns) identifiers
                text_user_tags_dfm = quanteda::as.dfm(
                    Matrix::sparseMatrix(
                        text_user_tags_rows, text_user_tags_columns, dims=c(nrow(TweetsAll), length(all_user_tags)), dimnames = list(TweetsAll$id,all_user_tags)
                    )
                )
            }
        )
        
        
        TW_search_text_usertags_top100=reactive({
            top100=as.data.frame(quanteda::topfeatures(TW_search_text_usertags_dfm(),n= 100))
            colnames(top100)=c("frequency")
            top100
        })
        
        output$TW_search_text_user_tags_top100 = DT::renderDT(
            TW_search_text_usertags_top100(),
            options = list(
                lengthChange = FALSE,
                searching = TRUE,
                pageLength = 10
            )
        )
        
        TW_search_text_usertags_cooc_matrix = reactive({
            text_user_tags_fcm = quanteda::fcm(TW_search_text_usertags_dfm())
            toptag_fcm = quanteda::fcm_select(text_user_tags_fcm, pattern = rownames(TW_search_text_usertags_top100()))
        })
        
        output$TW_search_text_usertags_cooc_network = renderPlot({
            quanteda::textplot_network(TW_search_text_usertags_cooc_matrix(),min_freq=2,omit_isolated=T, edge_alpha = 0.6, edge_size = 0.7)
        })
        
        output$TW_search_text_usertags_wordcloud = renderPlot({
            quanteda::textplot_wordcloud(TW_search_text_usertags_dfm(), min_count = 2, random_order = FALSE,rotation = .25,  color = RColorBrewer::brewer.pal(8,"Dark2"))
        })
        
        ####description tags analysis####
        #hashtags (#)
        TW_search_description_hashtags_dfm = reactive(
            if(input$TW_search_process_description){                                     
                TweetsAll = TW_search_observations_processed()
                all_hash_tags = unique(unlist(TweetsAll$description_hash_tags))#unlist hashtags   
                description_hash_tags_n_unique = sapply(sapply(TweetsAll$description_hash_tags,unique), length)#compute number of unique hastags per document/tweet
                description_hash_tags_rows = unlist(mapply(rep, times = description_hash_tags_n_unique, x = 1:nrow(TweetsAll)), recursive = FALSE)#document (row) identifiers  
                description_hash_tags_columns = unlist(sapply(TweetsAll$description_hash_tags, function(x){which(all_hash_tags %in% x)}), recursive = FALSE)#hash tags (columns) identifiers
                description_hash_tags_dfm = quanteda::as.dfm(
                    Matrix::sparseMatrix(
                        description_hash_tags_rows, description_hash_tags_columns, dims=c(nrow(TweetsAll), length(all_hash_tags)), dimnames = list(TweetsAll$id,all_hash_tags)
                    )
                )
            } else{print("the column named description hasn't yet been processed, please process it to visualize the description analysis outputs")}
        )
        TW_search_description_hashtags_top100=reactive({
            top100=as.data.frame(quanteda::topfeatures(TW_search_description_hashtags_dfm(),n= 100))
            colnames(top100)=c("frequency")
            top100
        }
        )
        
        output$TW_search_description_hash_tags_top100 = DT::renderDT(
            TW_search_description_hashtags_top100(),
            options = list(
                lengthChange = FALSE,
                searching = TRUE,
                pageLength = 10
            )
        )
        
        
        TW_search_description_hashtags_cooc_matrix = reactive({
            description_hash_tags_fcm = quanteda::fcm(TW_search_description_hashtags_dfm())
            toptag_fcm = quanteda::fcm_select(description_hash_tags_fcm, pattern = rownames(TW_search_description_hashtags_top100()))
        })
        
        output$TW_search_description_hashtags_cooc_network = renderPlot({
            quanteda::textplot_network(TW_search_description_hashtags_cooc_matrix(),min_freq=2,omit_isolated=T, edge_alpha = 0.6, edge_size = 0.7)
        })
        
        output$TW_search_description_hashtags_wordcloud = renderPlot({
            quanteda::textplot_wordcloud(TW_search_description_hashtags_dfm(), min_count = 2, random_order = FALSE,rotation = .25,  color = RColorBrewer::brewer.pal(8,"Dark2"))
        })
        #usertags (@)
        TW_search_description_usertags_dfm = reactive(
            if(input$TW_search_process_description){                                     
                TweetsAll = TW_search_observations_processed()
                all_user_tags = unique(unlist(TweetsAll$description_user_tags))#unlist usertags   
                description_user_tags_n_unique = sapply(sapply(TweetsAll$description_user_tags,unique), length)#compute number of unique hastags per document/tweet
                description_user_tags_rows = unlist(mapply(rep, times = description_user_tags_n_unique, x = 1:nrow(TweetsAll)), recursive = FALSE)#document (row) identifiers  
                description_user_tags_columns = unlist(sapply(TweetsAll$description_user_tags, function(x){which(all_user_tags %in% x)}), recursive = FALSE)#user tags (columns) identifiers
                description_user_tags_dfm = quanteda::as.dfm(
                    Matrix::sparseMatrix(
                        description_user_tags_rows, description_user_tags_columns, dims=c(nrow(TweetsAll), length(all_user_tags)), dimnames = list(TweetsAll$id,all_user_tags)
                    )
                )
            }
        )
        
        
        TW_search_description_usertags_top100=reactive({
            top100=as.data.frame(quanteda::topfeatures(TW_search_description_usertags_dfm(),n= 100))
            colnames(top100)=c("frequency")
            top100
        })
        
        output$TW_search_description_user_tags_top100 = DT::renderDT(
            TW_search_description_usertags_top100(),
            options = list(
                lengthChange = FALSE,
                searching = TRUE,
                pageLength = 10
            )
        )
        
        TW_search_description_usertags_cooc_matrix = reactive({
            description_user_tags_fcm = quanteda::fcm(TW_search_description_usertags_dfm())
            toptag_fcm = quanteda::fcm_select(description_user_tags_fcm, pattern = rownames(TW_search_description_usertags_top100()))
        })
        
        output$TW_search_description_usertags_cooc_network = renderPlot({
            quanteda::textplot_network(TW_search_description_usertags_cooc_matrix(),min_freq=2,omit_isolated=T, edge_alpha = 0.6, edge_size = 0.7)
        })
        
        output$TW_search_description_usertags_wordcloud = renderPlot({
            quanteda::textplot_wordcloud(TW_search_description_usertags_dfm(), min_count = 2, random_order = FALSE,rotation = .25,  color = RColorBrewer::brewer.pal(8,"Dark2"))
        })
    }
    , options = list(height = 800)
)

# Run the application 
