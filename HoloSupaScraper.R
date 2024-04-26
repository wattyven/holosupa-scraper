library(shiny)
library(rvest)
library(tidyverse)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        body {
          display: flex;
          justify-content: center;
          margin: 0;
        }
        .container-fluid {
          max-width: 1200px;
          width: 100%;
          align-items: flex-start;
          padding-top: 20px;
        }
        .row {
          justify-content: center;
          width: 100%;
          display: flex;
          flex-direction: row;
          align-items: center;
        }
        .col-sm-6, .col-sm-8, .col-sm-12 {
          display: flex;
          flex-direction: column;
          align-items: center;
          max-width: 100%;
        }
        #overview_text {
          font-family: 'Arial', sans-serif;
          font-size: 16px;
          line-height: 1.5;
          color: #333;
          margin-bottom: 20px;
          text-align: center;
        }
      ")
    )
  ),
  titlePanel(
    div(
      "YouTube SuperChat Analyzer",
      style = "text-align: center;"
    )
  ),
  mainPanel(
    fluidRow(
      column(
        12, offset = 0,
        textInput(
          "video_url",
          "YouTube Video URL",
          placeholder = "https://www.youtube.com/watch?v=...",
          width = "100%"
        ),
        div(
          style = "display: flex; justify-content: space-between;",
          actionButton("enter_btn", "Enter", class = "btn-primary"),
          actionButton("clear_btn", "Clear")
        )
      )
    ),
    fluidRow(
      column(
        12, offset = 0,
        uiOutput("buttons_ui")
      )
    ),
    fluidRow(
      column(
        12, offset = 0,
        htmlOutput("overview_text")
      )
    ),
    fluidRow(
      column(
        12, offset = 0,
        tableOutput("superchats_table")
      )
    ),
    width = 12
  )
)

server <- function(input, output, session) {
  superchats_data <- eventReactive(input$enter_btn, {
    video_url <- input$video_url
    if (nchar(video_url) == 0) {
      return(NULL)
    }
    
    video_pattern <- "(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|embed/|v/|\\S*\\?(?:[^&\\s]*&)*v=))([\\w-]+)"
    video_id <- str_match(video_url, video_pattern)[,2]
    
    # Initialize the base URL
    base_url <- paste0("https://www.hololyzer.net/youtube/archive/superchat/",video_id)
    
    # Initialize an empty list to store the HTML content
    html_content <- list()
    
    # Initialize a counter for the page index
    page_index <- 1
    
    # Start a loop that will continue until a 404 error is encountered
    while(TRUE) {
      # Construct the URL for the current page
      if(page_index == 1) {
        url <- paste0(base_url, ".html")
      } else {
        url <- paste0(base_url, "_", page_index, ".html")
      }
      
      # Try to get the HTML content from the page
      result <- tryCatch({
        html_content[[page_index]] <- read_html(url)
        TRUE  # return TRUE if no error
      }, error = function(e) {
        FALSE  # return FALSE if error
      })
      
      # If an error occurred, break the loop
      if(!result) {
        break
      }
      
      # Increment the page index
      page_index <- page_index + 1
    }
    
    # Return the HTML content
    html_content
    
    # Create a new list, superchats, with the same length as html_content, with contents as NULL
    superchats <- vector("list", length(html_content))
    
    for (i in seq_along(html_content)) {
      superchats[[i]] <- html_nodes(html_content[[i]], "#chatarea .visible") %>%
        map_df(function(node) {
          value <- html_text(html_nodes(node, ".table-cell.align-left"))
          yen_value <- html_text(html_nodes(node, ".table-cell.align-right")) %>%
            str_remove_all("[^0-9.]") %>%
            as.integer()
          yen <- if_else(!is.na(yen_value), yen_value, str_remove_all(value, "[^0-9.]") %>% as.integer())
          user <- html_text(html_nodes(node, ".align-left"))
          comment <- if_else(
            !is.na(html_node(node, ".td.align-left.comment span")),
            html_text(html_node(node, ".td.align-left.comment span")),
            html_text(html_node(node, ".td.align-left.comment"))
          )
          list(value = value, yen = yen, user = user, comment = comment)
        }) %>%
        drop_na() %>%
        mutate(
          comment = if_else(
            row_number() %% 3 == 2,
            lead(if_else(row_number() %% 3 == 0, user, NA_character_)),
            comment
          )
        ) %>%
        filter(row_number() %% 3 == 2) %>%
        mutate(
          user = if_else(str_detect(user, "\\(\\d+\\)$"), str_remove(user, "\\(\\d+\\)$"), user),
          comment = if_else(comment == "(無言スパチャ)", "wordless superchat", comment),
          comment = if_else(comment == "", "Member emote", comment)
        ) %>%
        mutate(index = row_number())
    }
    
    # flatten the list
    superchats <- bind_rows(superchats) %>%
      mutate(index = row_number()) %>%
      select(index, everything())
    
    # Generate the quick stats overview
    chatter_data <- superchats %>%
      group_by(user) %>%
      summarise(superchats = n()) %>%
      left_join(superchats %>% group_by(user) %>% summarise(total_yen = sum(yen)), by = "user")
    
    total_superchats <- sum(chatter_data$total_yen)
    overview_statement <- paste0("The most generous superchatter was ", chatter_data$user[which.max(chatter_data$total_yen)], ", who contributed ", max(chatter_data$total_yen), " yen or ", round(max(chatter_data$total_yen) / total_superchats * 100, 2), "% of the total superchat value of ", total_superchats, " yen for the stream. The most frequent chatter was ", chatter_data$user[which.max(chatter_data$superchats)], " with ", max(chatter_data$superchats), " superchats, and the overall average superchat value was ", round(mean(chatter_data$total_yen)), " yen.")
    
    # Return a list containing both superchats and overview_statement
    list(overview_statement = overview_statement, superchats = superchats)
  })
  
  observeEvent(input$clear_btn, {
    updateTextInput(session, "video_url", value = "")
    output$superchats_table <- renderTable(NULL)
    output$overview_text <- renderPrint(NULL)
    output$buttons_ui <- renderUI({})
  })

  
  output$superchats_table <- renderTable({
    superchats_data_table <- superchats_data()
    output$buttons_ui <- renderUI({
      tagList(
        actionButton("overview_btn", "Quick Overview", class = "action-button"),
        downloadLink("download_csv", "Export to CSV")
      )
    })
    superchats_data_table$superchats
  })
  
  observeEvent(input$overview_btn, {
    superchats_data_table <- superchats_data()
    if (!is.null(superchats_data_table)) {
      output$overview_text <- renderText({
        superchats_data_table$overview_statement
      })
    }
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      video_id <- str_match(input$video_url, "(?:youtu\\.be/|youtube\\.com/(?:watch\\?v=|embed/|v/|\\S*\\?(?:[^&\\s]*&)*v=))([\\w-]+)")[, 2]
      paste0(video_id, ".csv")
    },
    content = function(file) {
      write.csv(superchats_data()$superchats, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)