library(shiny)
library(rvest)
library(stringr)
library(purrr)
library(dplyr)
library(plotly)
library(DT)
library(tidytext)
library(lubridate)
library(httr)
library(jsonlite)
library(xml2)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      ".sidebar { height: calc(100vh - 100px); overflow-y: auto; }
       .main { height: calc(100vh - 100px); overflow: hidden; }
       .tab-content, .tab-pane { height: calc(100vh - 300px); overflow-y: auto; }
       .plotly { height: 100%; }"
    ))
  ),
  titlePanel(div("YouTube SuperChat Analyzer (V2.0)", style = "text-align: center;")),
  sidebarLayout(
    sidebarPanel(
      textInput("video_url", "YouTube Video URL", placeholder = "https://www.youtube.com/watch?v=..."),
      actionButton("enter_btn", "Fetch Data", class = "btn-primary"),
      actionButton("clear_btn", "Clear All"),
      hr(),
      radioButtons("currency", "Display Currency", choices = c("JPY", "USD"), selected = "JPY", inline = TRUE),
      uiOutput("filter_ui"),
      br(),
      uiOutput("download_ui")
    ),
    mainPanel(
      uiOutput("stream_info"),
      tabsetPanel(
        tabPanel("Cumulative Time Series", plotlyOutput("cum_plot", height = "100%")),
        tabPanel("Amount Distribution", plotlyOutput("dist_plot", height = "100%")),
        tabPanel("Top Donors", plotlyOutput("leaderboard_plot", height = "100%")),
        tabPanel("Avg vs Freq", plotlyOutput("scatter_plot", height = "100%")),
        tabPanel("Word Cloud", plotlyOutput("wordcloud_plot", height = "100%")),
        tabPanel("Raw Table", DT::dataTableOutput("superchats_table", height = "100%"))
      )
    )
  )
)

server <- function(input, output, session) {
  # get the JPY / USD exchange rate for the day, not live but updated once per day which is good enough for our purposes
  rate_data <- eventReactive(input$enter_btn, {
    req(input$video_url)
    res <- GET("https://open.er-api.com/v6/latest/USD")
    content(res, as = "parsed", simplifyDataFrame = TRUE)$rates[["JPY"]]
  })
  
  # stream title + thumbnail
  stream_info <- eventReactive(input$enter_btn, {
    req(input$video_url)
    page <- read_html(input$video_url)
    raw_title <- html_node(page, 'meta[property="og:title"]') %>% html_attr("content")
    clean_title <- str_replace_all(raw_title, "[^ -~]", "")
    thumb <- html_node(page, 'meta[property="og:image"]') %>% html_attr("content")
    list(title = clean_title, thumbnail = thumb)
  })
  
  # spaghetti for getting our superchat data
  superchats_data <- eventReactive(input$enter_btn, {
    req(input$video_url)
    vid_pattern <- "(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|embed/|v/|\\S*\\?(?:[^&\\s]*&)*v=))([\\w-]+)"
    video_id <- str_match(input$video_url, vid_pattern)[,2]
    base_url <- paste0("https://www.hololyzer.net/youtube/archive/superchat/", video_id)
    
    # in the event we've got a stream with so many superchats that it has multiple pages
    pages <- list(); p <- 1
    repeat {
      url <- if(p == 1) paste0(base_url, ".html") else paste0(base_url, "_", p, ".html")
      ok <- tryCatch({ pages[[p]] <- read_html(url); TRUE }, error = function(e) FALSE)
      if(!ok) break
      p <- p + 1
    }
    
    # this was by far the worst part of this whole ordeal
    bind_rows(map(pages, function(pg) {
      html_nodes(pg, "#chatarea .visible.supacha") %>%
        map_df(function(nd) {
          cells <- html_nodes(nd, "div.td")
          ut <- html_text(html_node(cells[2], "span.unixtime_timeLabel")) %>% str_squish() %>% as.numeric()
          ts <- as.POSIXct(ut/1e6, origin="1970-01-01", tz="UTC") %>% with_tz(Sys.timezone())
          orig <- html_text(html_node(cells[3], ".table-cell.align-left")) %>% str_squish()
          yen_txt <- html_text(html_node(cells[3], ".table-cell.align-right")) %>% str_remove_all("[^0-9]")
          yen <- as.integer(yen_txt); if(is.na(yen)) yen <- str_remove_all(orig, "[^0-9]") %>% as.integer()
          user <- html_text(cells[6]) %>% str_squish() %>% str_remove_all("\\(\\d+\\)$")
          
          raw_comment <- html_text(cells[7]) %>% str_squish()
          # Translate and retain all chats
          if(raw_comment == "(無言スパチャ)") comment <- "Wordless superchat"
          else if(length(html_nodes(cells[7], "img"))>0 && raw_comment == "") comment <- "Member emote"
          else comment <- raw_comment
          
          tibble(timestamp = ts, orig, yen, user, comment)
        })
    })) %>%
      arrange(timestamp) %>%
      mutate(index = row_number())
  })
  
  # stream info
  output$stream_info <- renderUI({
    req(stream_info())
    info <- stream_info()
    div(style = "display:flex; align-items:center; margin-bottom:20px;",
        img(src = info$thumbnail, height = '80px', style = "border-radius:4px;"),
        tags$h3(info$title, style = "margin-left:15px; font-weight:600;")
    )
  })
  
  # ui for filtering donos, users etc
  output$filter_ui <- renderUI({
    req(superchats_data(), rate_data(), input$currency)
    df <- superchats_data(); rate <- rate_data()
    max_val <- if(input$currency=="JPY") max(df$yen, na.rm=TRUE) else round(max(df$yen, na.rm=TRUE)/rate,2)
    step_amt <- if(input$currency=="JPY") 100 else 0.01
    tagList(
      sliderInput("min_amount", "Min Donation", min=0, max=max_val, value=0, step=step_amt),
      fluidRow(
        column(8,
               selectizeInput("select_user", "Select Users", choices=unique(df$user), multiple=TRUE,
                              options=list(plugins=list('remove_button'), placeholder='Select users...'))
        ),
        column(4, actionButton("clear_user", "Clear", style="margin-top:25px"))
      )
    )
  })
  
  observeEvent(input$clear_user, {
    updateSelectizeInput(session, "select_user", selected = character(0))
  })
  
  # for actually implementing the filters
  filtered <- reactive({
    req(superchats_data(), rate_data(), input$currency, input$min_amount)
    df <- superchats_data(); rate <- rate_data()
    df <- df %>% mutate(amount = if(input$currency=="JPY") yen else round(yen/rate,2)) %>% filter(amount>=input$min_amount)
    if(!is.null(input$select_user)&&length(input$select_user)>0) df <- df %>% filter(user %in% input$select_user)
    df
  })
  
  # for exporting to csv
  output$download_ui <- renderUI({
    req(superchats_data())
    downloadButton("download_csv", "Export CSV", class = "btn-success")
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("superchats_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )
  
  # our visualizations!
  output$cum_plot <- renderPlotly({ # CUMULATIVE
    df <- filtered() %>% mutate(cumulative=cumsum(amount))
    plot_ly(df,x=~timestamp,y=~cumulative,type='scatter',mode='lines')%>%
      layout(title='Cumulative Superchat Value',xaxis=list(title='Time'),yaxis=list(title=input$currency))
  })
  
  output$dist_plot <- renderPlotly({
    df <- filtered()
    plot_ly(df,x=~amount,type='histogram',nbinsx=30)%>%
      layout(title='Donation Distribution',xaxis=list(title=input$currency),yaxis=list(title='Count'))
  })
  
  output$leaderboard_plot <- renderPlotly({
    df <- filtered()%>%group_by(user)%>%summarise(total=sum(amount,na.rm=TRUE))%>%arrange(desc(total))%>%head(10)
    plot_ly(df,x=~reorder(user,total),y=~total,type='bar')%>%
      layout(title='Top 10 Donors',xaxis=list(title='User'),yaxis=list(title=input$currency))
  })
  
  output$scatter_plot <- renderPlotly({
    df <- filtered()%>%group_by(user)%>%summarise(avg=mean(amount),freq=n())
    plot_ly(df,x=~freq,y=~avg,text=~user,type='scatter',mode='markers')%>%
      layout(title='Average vs Frequency',xaxis=list(title='Count',dtick=1),yaxis=list(title=paste('Avg',input$currency)))
  })
  
  output$wordcloud_plot <- renderPlotly({
    words <- filtered()%>%filter(comment!='Member emote',comment!='Wordless superchat')%>%select(comment)%>%unnest_tokens(word,comment)%>%filter(!word%in%stop_words$word,word!='emote')%>%count(word,sort=TRUE)%>%head(50)%>%mutate(x=runif(n),y=runif(n),size=scales::rescale(n,c(10,50)))
    plot_ly(words,x=~x,y=~y,text=~word,textfont=list(size=~size),hoverinfo='text',mode='text')%>%
      layout(title='Word Cloud',xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
  })
  
  output$superchats_table <- renderDataTable({
    filtered()%>%select(timestamp,user,amount,comment)
  },options=list(pageLength=10,scrollX=TRUE))
  
  observeEvent(input$clear_btn, {
    session$reload()
  })
}

shinyApp(ui, server)
