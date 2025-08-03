library(shiny)
library(DT)
library(dplyr)
library(readr)
library(stringdist)
library(fuzzyjoin)
library(bslib)

# Load alias mapping file
alias_product <- read_csv("data/product_alias.csv")

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "cerulean",
    primary = "#FF5349",
    font_scale = 1,
    "headings-color" = "#29333C"
  ),
  title = "Kpler Product Normalization Tool",
  
  sidebar =
    list(
    fileInput("raw_file", "Upload 'raw_product.csv' file", accept = ".csv"),
    uiOutput("file_warning"),
    tags$hr(),
    downloadButton("download_cleaned", "Download mapped products", class = "btn-light"),
    br(),
    downloadButton("download_likely_valid", "Download fuzzyjoin products", class = "btn-light"),
    br(),
    downloadButton("download_unmatched", "Download unmapped products", class = "btn-light")
  ),

  # Contenu principal
  navs_tab_card(
    nav_panel("Mapped products", DTOutput("cleaned_table")),
    nav_panel("Fuzzy join products", DTOutput("likely_table")),
    nav_panel("Unmapped products", DTOutput("unmatched_table"))
  )
)

server <- function(input, output, session) {
  raw_data <- reactiveVal()
  cleaned_data <- reactiveVal()
  likely_valid_data <- reactiveVal()
  unmatched_data <- reactiveVal()
  file_error <- reactiveVal(NULL)

  observeEvent(input$raw_file, {
    req(input$raw_file)

    tryCatch({
      raw <- read_csv(input$raw_file$datapath)

      if (!all(c("raw_product") %in% colnames(raw))) {
        file_error("Invalid file format: required columns 'raw_product' not found.")
        raw_data(NULL)
        cleaned_data(NULL)
        likely_valid_data(NULL)
        unmatched_data(NULL)
        return(NULL)
      }

      file_error(NULL)
      raw_data(raw)
      
      # clean alias product and raw product
      alias_product_c <- alias_product |> 
        mutate(
        alias_id = as.character(alias_id),
        provider_id = as.character(provider_id),
        product_id = as.character(product_id)) |> 
        select(alias_id, alias, mapped_product, product_id) |> 
        distinct()
      
      raw_product_c <- raw |> 
        filter(!is.na(raw_product)) |> 
        distinct() |> 
        mutate(provider_id = as.character(provider_id)) |> 
        arrange(raw_product)
      
      
      # mapped products
      mapped_products <- raw_product_c |> left_join(alias_product_c, 
                             by = c("raw_product" = "alias")) |> 
        filter(!is.na(mapped_product))

      # unmapped products
      unmapped <- raw_product_c |> left_join(alias_product_c, 
                             by = c("raw_product" = "alias")) |> 
        filter(is.na(mapped_product))
      
      # categorize unmapped products in likely valid products
      likely_valid_products <- unmapped |>
        select(raw_product, provider_id) |>
        stringdist_left_join(
          alias_product_c, 
          by = c("raw_product" = "alias"), 
          method = "jw",
          max_dist = 0.2,
          distance_col = "distance") |> 
        arrange(raw_product, distance) |>
        group_by(raw_product, provider_id) |>
        slice(1) |> 
        ungroup() |>
        filter(!is.na(mapped_product)) |> 
        select(raw_product, alias, mapped_product, distance)

        # unmapped products
      unmapped_products <- unmapped |>
        select(raw_product, provider_id) |>
        stringdist_left_join(
          alias_product_c, 
          by = c("raw_product" = "alias"), 
          method = "jw",
          max_dist = 0.2,
          distance_col = "distance") |> 
        arrange(raw_product, distance) |>
        group_by(raw_product, provider_id) |>
        slice(1) |>
        ungroup() |>
        filter(is.na(mapped_product)) |>
        select(raw_product, alias, mapped_product)
        
      
      cleaned_data(mapped_products)
      likely_valid_data(likely_valid_products)
      unmatched_data(unmapped_products)

      
    }, error = function(e) {
      file_error("Error reading CSV file. Please check the file format.")
      raw_data(NULL)
      cleaned_data(NULL)
      likely_valid_data(NULL)
      unmatched_data(NULL)
    })
  })

  output$file_warning <- renderUI({
    if (!is.null(file_error())) {
      div(style = "color: red;", strong(file_error()))
    }
  })

  output$cleaned_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(),
      class = 'cell-border stripe',
      selection = "none",
      filter = "top")
  })

  output$likely_table <- renderDT({
    req(likely_valid_data())
    datatable(likely_valid_data(),
      class = 'cell-border stripe',
      selection = "none",
      filter = "top",
      editable = list(target = "row", disable = list(columns = c(3, 4, 5))), 
      rownames = FALSE) |>
      formatStyle('raw_product',  color = '#FF5349', fontWeight = 'bold')
  })
  
  observeEvent(input$likely_table_cell_edit, {
    info <- input$likely_table_cell_edit
    df <- likely_valid_data()
    df[info$row, info$col] <- info$value
    likely_valid_data(df)
  })
    
  output$unmatched_table <- renderDT({
    req(unmatched_data())
    datatable(unmatched_data(),
      class = 'cell-border stripe',
      selection = "none",
      filter = "top",
      editable = list(target = "row", disable = list(columns = c(3, 4))), 
      rownames = FALSE) |>
      formatStyle('raw_product',  color = '#FF5349', fontWeight = 'bold')
  })
  


  output$download_cleaned <- downloadHandler(
    filename = function() { "cleaned_products.csv" },
    content = function(file) {
      write_csv(cleaned_data(), file)
    }
  )

    output$download_likely_valid <- downloadHandler(
    filename = function() { "download_likely_valid_products.csv" },
    content = function(file) {
      write_csv(likely_valid_data(), file)
    }
  )
    
  output$download_unmatched <- downloadHandler(
    filename = function() { "unmatched_products.csv" },
    content = function(file) {
      write_csv(unmatched_data(), file)
    }
  )
}

shinyApp(ui, server)


