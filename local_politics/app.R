#
# Ideas,
# - change plot colors
# - Maybe add annotations for council meetings by scraping webpage?
# - make plotly filter by dragging time series plot
# - fix non ASCII charachters from showing up - may have to retrain 
# - Is seems like emails are being binned into multible top categories
# - I want a wow graphic that is eye catching - maybe showing network of topics?
# - Show 'all topics'. Add annotation about spikes (EG top word in spike)

library(shiny)
library(shinythemes)
library(forcats)
library(htmltools)
library(plotly)
library(topicmodels)
library(tidytext)
library(broom)
library(dplyr)
library(stringr)
library(DT)
library(purrr)
library(ggrepel) #text and label geoms for ggplot2
library(kableExtra)

plot_primary <- '#007bff'
plot_primary <- '#6610f2'


# cleaned in an ETL script - TODO move the ETL into a chron job on cloud
in_dat <- readRDS('clustered_assignments.rds') %>% 
    filter(!is.na(topic)) %>% 
    mutate(topic_lab = paste0("Topic ", 
                         str_pad(topic, 2, 'left', '0'),
                         ": ",
                         term)) %>% 
    arrange(desc(ReceivedDate))
topic_model <- readRDS('topic_model.rds')

beta_topics <- topic_model %>%
    tidy('beta') %>%
    left_join(in_dat %>% distinct(topic, topic_lab), by = 'topic') %>% 
    mutate(term = str_to_title(term))



# experimentation --------
# top_terms <- tidy(topic_model, 'beta') %>%
#     group_by(topic) %>%
#     arrange(topic, desc(beta)) %>%
#     #get the top num_words PER topic
#     slice(seq_len(20)) %>%
#     arrange(topic, beta) %>%
#     #row is required for the word_chart() function
#     mutate(row = row_number()) %>%
#     ungroup() %>%
#     #add the word Topic to the topic labels
#     arrange(topic) %>% 
#     mutate(topic = paste0("Topic ", 
#                           str_pad(topic, 2, 'left', '0'),
#                           ": ",
#                           str_to_title(term)))
   




# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme('united'),
    inverse = TRUE,
    # Application title
    title = "What Are Boulderites Interested In?",
    tabPanel('Boulder City Council Emails',
             includeMarkdown('intro.md')),
    tabPanel('Dashboard',
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectizeInput('topics_filter', 'Choose Topic/s',
                                        choices = unique(in_dat$topic_lab),
                                        multiple = TRUE),
                         plotlyOutput('topic_words'),
                         dateRangeInput('dates', 'Select Date:',
                                        start = '2018-01-01',
                                        end = max(in_dat$ReceivedDate))),
                     mainPanel(plotlyOutput('ts_density', height = '600px'))
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

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    inDatTop = reactive({
        in_dat %>% 
            group_by(MessageIdentifier) %>% 
            filter(gamma == max(gamma)) %>% 
            ungroup() %>% 
            filter(topic_lab %in% input$topics_filter) 
    })
    
    
    
    output$topic_words = renderPlotly({
        beta_topics %>% 
            filter(topic_lab %in% input$topics_filter) %>%
            arrange(desc(beta)) %>%
            head(10) %>%
            mutate(term = as_factor(term) %>% fct_rev()) %>% 
            plot_ly(orientation = 'h') %>% 
            add_bars(y = ~term,
                     color = ~topic_lab,
                     x = ~beta) %>%
            layout(paper_bgcolor = rgb(0,0,0,0),
                   plot_bgcolor =  rgb(0,0,0,0))
    })

    output$ts_density = renderPlotly({
        
        inDatTop() %>% 
            plot_ly(x = ~ReceivedDate) %>% 
            add_histogram(color = ~topic_lab) %>%
            layout(paper_bgcolor = rgb(0,0,0,0),
                   plot_bgcolor =  rgb(0,0,0,0))
    }
    )
    
    output$mailbox = renderDT({
        inDatTop() %>% 
            select(Topic = topic_lab, ReceivedDate, EmailSubject) %>% 
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
            add_pie(values = ~gamma, labels = ~topic_lab)
        } 
    })

    
}

# Run the application 
shinyApp(ui = ui, server = server)
