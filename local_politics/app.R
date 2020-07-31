#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(htmltools)
library(plotly)
library(topicmodels)
library(tidytext)
library(broom)
library(DT)
library(purrr)
library(ggrepel) #text and label geoms for ggplot2
library(kableExtra)



# cleaned in an ETL script - TODO move the ETL into a chron job on cloud
in_dat <- readRDS('clustered_assignments.rds') %>% 
    mutate(topic = paste("Topic", 
                         topic-1)) 
topic_model <- readRDS('topic_model.rds')

# experiimentation --------
top_terms <- tidy(topic_model, 'beta') %>%
    group_by(topic) %>%
    arrange(topic, desc(beta)) %>%
    #get the top num_words PER topic
    slice(seq_len(20)) %>%
    arrange(topic, beta) %>%
    #row is required for the word_chart() function
    mutate(row = row_number()) %>%
    ungroup() %>%
    #add the word Topic to the topic labels
    mutate(topic = paste("Topic", topic, sep = " "))

# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme = shinytheme('darkly'),
    # Application title
    title = "What Are Boulderites Interested In??",
    tabsetPanel(
        tabPanel('Boulder City Council Emails'),
        tabPanel('Dashboard',
                 fluidPage(
                     sidebarLayout(
                         sidebarPanel(
                             selectizeInput('topics_filter', 'Choose Topic/s',
                                            choices = unique(in_dat$topic),
                                            multiple = FALSE),
                             plotlyOutput('topic_words')),
                         mainPanel(plotlyOutput('ts_density'))
                     ),
                     DTOutput('mailbox'),
                     uiOutput('email')
                 )
        ),
        tabPanel('Methods'),
        tabPanel('Contact')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    inDat = reactive({
        in_dat %>% filter(topic %in% input$topics_filter) 
    })
    
    output$topic_words = renderPlotly({
        tmp <- topic_model %>%
            tidy('beta') %>%
            mutate(topic = paste("Topic", topic-1)) %>%
            filter(topic %in% input$topics_filter) %>%
            arrange(desc(beta)) %>%
            head(10) %>%
            ggplot() +
            aes(x = term,
                y = beta) +
            geom_bar(stat = 'identity',
                     fill = 'blue') +
            coord_flip()

        ggplotly(tmp) %>%
            layout(paper_bgcolor = rgb(0,0,0,0),
                   plot_bgcolor =  rgb(0,0,0,0))
    })

    output$ts_density = renderPlotly({
        tmp <- inDat() %>%
            ggplot() +
            aes(x = ReceivedDate) +
            geom_density(alpha = .5,
                         fill = 'blue')

        ggplotly(tmp) %>%
            layout(paper_bgcolor = rgb(0,0,0,0),
                   plot_bgcolor =  rgb(0,0,0,0))
    }
    )
    
    output$mailbox = renderDT({
        inDat() %>% 
            select(Topic = topic, SentTo, ReceivedDate, EmailSubject) %>% 
            DT::datatable(selection = 'single') %>%
            formatStyle(
                columns = c(1:4),
                target = 'row',
                backgroundColor = rgb(0,0,0,0))
    })
    
    selectedEmail = reactive(inDat()[input$mailbox_rows_selected, ])
    
    output$email = renderUI(
        div(
            h3(selectedEmail() %>% pluck("EmailSubject", 1)),
            p(selectedEmail() %>% pluck("PlainTextBody", 1))
        )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
