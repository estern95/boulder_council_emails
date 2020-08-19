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
library(forcats)
library(htmltools)
library(plotly)
library(topicmodels)
library(tidytext)
library(broom)
library(dplyr)
library(DT)
library(purrr)
library(ggrepel) #text and label geoms for ggplot2
library(kableExtra)



# cleaned in an ETL script - TODO move the ETL into a chron job on cloud
in_dat <- readRDS('clustered_assignments.rds') %>% 
    filter(!is.na(topic)) %>% 
    mutate(topic = paste("Topic", 
                         topic)) %>% 
    arrange(desc(ReceivedDate))
topic_model <- readRDS('topic_model.rds')

# experimentation --------
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
    arrange(topic) %>% 
    mutate(topic = paste("Topic", topic, sep = " ") %>% as_factor()) 
   

# Define UI for application that draws a histogram
ui <- fluidPage(
    #theme = shinytheme('darkly'),
    # Application title
    title = "What Are Boulderites Interested In?",
    tabsetPanel(
        tabPanel('Boulder City Council Emails',
                 includeMarkdown('intro.md')),
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
                     fluidRow(
                         column(6, uiOutput('email')),
                         column(6, plotlyOutput('topic_designation'))
                     )
                 )
        ),
        tabPanel('Methods',
                 includeMarkdown('method.md')),
        tabPanel('Contact')
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    inDatTop = reactive({
        in_dat %>% 
            group_by(MessageIdentifier) %>% 
            filter(gamma == max(gamma)) %>% 
            ungroup() %>% 
            filter(topic %in% input$topics_filter) 
    })
    
    
    
    output$topic_words = renderPlotly({
        tmp <- topic_model %>%
            tidy('beta') %>%
            mutate(topic = paste("Topic", topic)) %>%
            filter(topic %in% input$topics_filter) %>%
            arrange(desc(beta)) %>%
            head(10) %>%
            mutate(term = as_factor(term) %>% fct_rev()) %>% 
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

        tmp <- inDatTop() %>%
            filter(topic %in% input$topics_filter) %>%
            ggplot() +
            aes(x = ReceivedDate) +
            geom_density(stat = 'count',
                         fill = 'blue')

        ggplotly(tmp) %>%
            layout(paper_bgcolor = rgb(0,0,0,0),
                   plot_bgcolor =  rgb(0,0,0,0))
    }
    )
    
    output$mailbox = renderDT({
        inDatTop() %>% 
            select(Topic = topic, SentTo, ReceivedDate, EmailSubject) %>% 
            DT::datatable(selection = 'single') %>%
            formatStyle(
                columns = c(1:4),
                target = 'row',
                backgroundColor = rgb(0,0,0,0))
    })
    
    selectedEmail = reactive(inDatTop()[input$mailbox_rows_selected, ])
    
    output$email = renderUI({
        if (nrow(selectedEmail()) == 0) {
            return(NULL)
        } else {
            div(
                h3(selectedEmail() %>% pluck("EmailSubject", 1)),
                p(selectedEmail() %>% pluck("PlainTextBody", 1))
            )
        }
    }
    )
    
    
    
    output$topic_designation = renderPlotly({
        
        if (nrow(selectedEmail()) == 0) {
            return(NULL)
        } else {
        id = selectedEmail() %>% pluck('MessageIdentifier', 1)
        
        tmp <- in_dat %>% 
            filter(MessageIdentifier == id) 
        
        tmp %>% 
            plot_ly() %>% 
            add_pie(values = ~gamma, labels = ~topic)
        } 
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
