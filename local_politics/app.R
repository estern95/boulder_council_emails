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

word_chart <- function(data, input, title) {
    data %>%
        #set y = 1 to just plot one variable and use word as the label
        ggplot(aes(as.factor(row), 1, label = input, fill = factor(topic) )) +
        #you want the words, not the points
        geom_point(color = "transparent") +
        #make sure the labels don't overlap
        geom_label_repel(nudge_x = .2,  
                         direction = "y",
                         box.padding = 0.1,
                         segment.color = "transparent",
                         size = 3) +
        facet_grid(~topic) +
        theme_dark() +
        theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
              #axis.title.x = element_text(size = 9),
              panel.grid = element_blank(), panel.background = element_blank(),
              panel.border = element_rect("lightgray", fill = NA),
              strip.text.x = element_text(size = 9)) +
        labs(x = NULL, y = NULL, title = title) 
}

top_terms_per_topic <- function(lda_model, num_words) {
    
    #tidy LDA object to get word, topic, and probability (beta)
    topics_tidy <- tidy(lda_model, matrix = "beta")
    
    
    top_terms <- topics_tidy %>%
        group_by(topic) %>%
        arrange(topic, desc(beta)) %>%
        #get the top num_words PER topic
        slice(seq_len(num_words)) %>%
        arrange(topic, beta) %>%
        #row is required for the word_chart() function
        mutate(row = row_number()) %>%
        ungroup() %>%
        #add the word Topic to the topic labels
        mutate(topic = paste("Topic", topic-1, sep = " "))
    #create a title to pass to word_chart
    title <- paste("LDA Top Terms")
    #call the word_chart function you built in prep work
    word_chart(top_terms, top_terms$term, title)
}

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
    theme = shinytheme('darkly'),
    # Application title
    title = "What Are Boulderites Interested In??",
    tabsetPanel(
        tabPanel('Boulder City Council Emails'),
        tabPanel('Dashboard',
                 fluidRow(
                     sidebarLayout(
                         sidebarPanel(
                             selectizeInput('topics_filter', 'Choose Topic/s',
                                            choices = unique(in_dat$topic),
                                            multiple = FALSE),
                             plotlyOutput('topic_words')),
                         mainPanel(plotlyOutput('ts_density'))
                     ),
                     dataTableOutput('mailbox'),
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
    
    output$mailbox = renderDataTable({
        inDat() %>% 
            select(Topic = topic, SentTo, SentCC, ReceivedDate, EmailSubject) %>% 
            datatable(selection = 'single') %>% 
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
            # h3(in_dat %>% pluck("EmailSubject", 1)),
            # p(in_dat %>% pluck("PlainTextBody", 1))
        )
        )
    
}

# Run the application 
shinyApp(ui = ui, server = server)