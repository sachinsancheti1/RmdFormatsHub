library(shiny)
library(rmarkdown)
library(shinyAce)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # For error handling and status feedback
  titlePanel("R Markdown to Multiple Formats"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload R Markdown File (.Rmd)", accept = ".Rmd"),
      
      # Display the uploaded file name for feedback
      textOutput("fileName"),
      
      # Theme picker (Beamer themes)
      selectInput("theme", "Select Beamer Theme",
                  choices = c("default", "AnnArbor", "Antibes", "Bergen", 
                              "Berkeley", "Berlin", "Boadilla", "CambridgeUS")),
      
      # Advanced formatting options
      numericInput("fontsize", "Font Size", 11),
      numericInput("margin", "Margin (cm)", 2),
      
      # Format selection
      checkboxGroupInput("formats", "Select Output Formats", 
                         choices = c("Beamer PDF" = "beamer",
                                     "Knitr PDF" = "knitr",
                                     "R HTML Presentation" = "html",
                                     "Word Document" = "word")),
      
      actionButton("convert", "Convert"),
      textOutput("status"),
      downloadButton("download", "Download Converted Files")
    ),
    mainPanel(
      aceEditor("markdownEditor", mode = "rmarkdown", height = "400px"),  # Live editor for editing RMarkdown
      textOutput("analytics")  # Display analytics
    )
  )
)

server <- function(input, output, session) {
  
  # Show the uploaded file name for confirmation
  output$fileName <- renderText({
    if (is.null(input$file)) {
      return("No file selected")
    } else {
      return(paste("Selected file:", input$file$name))
    }
  })
  
  # Read the uploaded file and display it in the live editor
  observeEvent(input$file, {
    req(input$file)
    
    # Read the contents of the uploaded RMarkdown file
    file_content <- readLines(input$file$datapath, warn = FALSE)
    
    # Update aceEditor with the file content
    updateAceEditor(session, "markdownEditor", value = paste(file_content, collapse = "\n"))
  })
  
  observeEvent(input$convert, {
    req(input$file)  # Ensure a file is uploaded
    file_path <- input$file$datapath
    formats <- input$formats
    
    # Basic error handling and user feedback
    if (is.null(input$file)) {
      output$status <- renderText("Please upload a valid R Markdown file.")
      return()
    }
    
    if (length(formats) == 0) {
      output$status <- renderText("Please select at least one output format.")
      return()
    }
    
    output$status <- renderText("Converting...")
    
    output_files <- list()  # Initialize list for storing file paths
    
    # Convert to Beamer Presentation with theme

    if ("beamer" %in% formats) {
      tryCatch({
        print("Rendering Beamer presentation...")
        output_file_beamer <- rmarkdown::render(file_path, 
                                                output_format = beamer_presentation(theme = input$theme, font_size = input$fontsize, keep_tex = TRUE))
        print(paste("Beamer file generated at:", output_file_beamer))
        output_files$beamer <- output_file_beamer
      }, error = function(e) {
        # Capture LaTeX-specific errors
        output$status <- renderText(paste("Error converting to Beamer: ", 
                                          "Ensure LaTeX (TinyTeX) is installed and working.", 
                                          e$message))
      })
    }
    
    # Convert to knitr PDF
    if ("knitr" %in% formats) {
      tryCatch({
        print("Rendering knitr PDF...")
        output_file_knitr <- rmarkdown::render(file_path, output_format = "pdf_document")
        print(paste("Knitr PDF file generated at:", output_file_knitr))
        output_files$knitr <- output_file_knitr
      }, error = function(e) {
        output$status <- renderText(paste("Error converting to Knitr PDF: ", e$message))
      })
    }
    
    # Convert to R HTML presentation (ioslides)
    if ("html" %in% formats) {
      tryCatch({
        print("Rendering HTML presentation...")
        output_file_html <- rmarkdown::render(file_path, output_format = "ioslides_presentation")
        print(paste("HTML file generated at:", output_file_html))
        output_files$html <- output_file_html
      }, error = function(e) {
        output$status <- renderText(paste("Error converting to HTML: ", e$message))
      })
    }
    
    # Convert to Word Document
    if ("word" %in% formats) {
      tryCatch({
        print("Rendering Word document...")
        output_file_word <- rmarkdown::render(file_path, output_format = "word_document")
        print(paste("Word file generated at:", output_file_word))
        output_files$word <- output_file_word
      }, error = function(e) {
        output$status <- renderText(paste("Error converting to Word Document: ", e$message))
      })
    }
    
    # Debugging: Print the generated file paths
    print("Generated file paths:")
    print(output_files)
    
    # Ensure files exist before attempting to zip
    valid_files <- unlist(output_files)
    valid_files <- valid_files[file.exists(valid_files)]  # Only include files that exist
    
    if (length(valid_files) > 0) {
      # Once done, notify the user
      output$status <- renderText("Conversion completed. Download your files below.")
      
      # Create download handler for converted files
      output$download <- downloadHandler(
        filename = function() { "converted_files.zip" },
        content = function(file) {
          zip(file, files = valid_files)
        }
      )
    } else {
      output$status <- renderText("No valid files were generated. Please try again.")
    }
    
    # Analytics tracking (simple counts for now)
    output$analytics <- renderText({
      paste("Converted file formats:", paste(formats, collapse = ", "), 
            "\nFont size:", input$fontsize, 
            "\nMargin size:", input$margin, "cm")
    })
  })
}

shinyApp(ui = ui, server = server)
