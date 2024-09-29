library(shiny)
library(rmarkdown)
library(shinyAce)
library(shinyjs)
library(zip)
library(fs)
library(yaml)  # To parse YAML frontmatter

ui <- fluidPage(
  useShinyjs(),
  titlePanel("R Markdown to Multiple Formats"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload R Markdown File (.Rmd)", accept = ".Rmd"),
      
      # Sidebar to configure themes and formats
      h4("Themes and Formats from YAML"),
      
      # Theme pickers for Beamer, PDF, HTML, Word
      selectInput("theme_beamer", "Beamer Theme", choices = c("default", "AnnArbor", "Antibes", "Bergen")),
      selectInput("theme_pdf", "PDF Theme", choices = c("default")),
      selectInput("theme_html", "HTML Theme", choices = c("default")),
      selectInput("theme_word", "Word Theme", choices = c("default")),
      
      checkboxGroupInput("formats", "Select Output Formats", 
                         choices = c("Beamer PDF" = "beamer",
                                     "Knitr PDF" = "knitr",
                                     "R HTML Presentation" = "html",
                                     "Word Document" = "word")),
      
      actionButton("convert", "Convert"),
      downloadButton("downloadRmd", "Download Updated Rmd"),
      textOutput("status"),
      downloadButton("download", "Download Converted Files")
    ),
    mainPanel(
      # Frontmatter editor
      h4("Frontmatter"),
      aceEditor("frontmatterEditor", mode = "yaml", height = "200px"),
      
      # Content editor
      h4("Body Content"),
      aceEditor("bodyEditor", mode = "rmarkdown", height = "400px"),
      
      # Log of console messages
      tags$pre(id = "consoleLog", style = "background-color: #f9f9f9; border: 1px solid #ddd; padding: 10px; max-height: 300px; overflow-y: scroll;"),
      
      # Progress notification
      hidden(div(id = "processingNotification", h4("Processing... Please wait"))),
      
      textOutput("analytics")
    )
  )
)

server <- function(input, output, session) {
  useShinyjs()
  
  # Function to update the log
  updateLog <- function(message) {
    shinyjs::html(id = "consoleLog", html = paste0(message, "\n"), add = TRUE)
  }
  
  # Variable to store the updated Rmd content
  updated_rmd <- reactiveVal(NULL)
  
  # Variable to store the original file name
  original_file_name <- reactiveVal("updated_file.Rmd")
  
  # Parse frontmatter and body content separately
  observeEvent(input$file, {
    req(input$file)
    
    # Read the contents of the uploaded RMarkdown file
    file_content <- readLines(input$file$datapath, warn = FALSE)
    rmd_content <- paste(file_content, collapse = "\n")
    
    # Split the frontmatter and body content
    frontmatter <- ""
    body <- rmd_content
    if (grepl("---", rmd_content)) {
      parts <- unlist(strsplit(rmd_content, "---\n", fixed = TRUE))
      if (length(parts) >= 3) {
        frontmatter <- paste(parts[2], collapse = "\n")
        body <- paste(parts[3:length(parts)], collapse = "---\n")
      }
    }
    
    # Parse the frontmatter as YAML
    parsed_yaml <- tryCatch({
      yaml.load(frontmatter)
    }, error = function(e) {
      updateLog("Error parsing YAML frontmatter.")
      return(NULL)
    })
    
    # If YAML is valid, update themes and formats in the sidebar
    if (!is.null(parsed_yaml) && !is.null(parsed_yaml$output)) {
      output_settings <- parsed_yaml$output
      if (!is.null(output_settings$beamer_presentation)) {
        updateSelectInput(session, "theme_beamer", selected = "default")
        updateCheckboxGroupInput(session, "formats", selected = c("beamer"))
      }
      if (!is.null(output_settings$pdf_document)) {
        updateSelectInput(session, "theme_pdf", selected = "default")
        updateCheckboxGroupInput(session, "formats", selected = c("knitr"))
      }
      if (!is.null(output_settings$ioslides_presentation)) {
        updateSelectInput(session, "theme_html", selected = "default")
        updateCheckboxGroupInput(session, "formats", selected = c("html"))
      }
      if (!is.null(output_settings$word_document)) {
        updateSelectInput(session, "theme_word", selected = "default")
        updateCheckboxGroupInput(session, "formats", selected = c("word"))
      }
    }
    
    # Update the Ace editors
    updateAceEditor(session, "frontmatterEditor", value = frontmatter)
    updateAceEditor(session, "bodyEditor", value = body)
    
    # Store the updated Rmd content for further use
    updated_rmd(rmd_content)
  })
  
  # Capture content updates from the frontmatter and body editors
  observe({
    frontmatter <- input$frontmatterEditor
    body <- input$bodyEditor
    updated_rmd(paste0("---\n", frontmatter, "---\n", body))
  })
  
  # Download the updated Rmd file with the original or improved name
  output$downloadRmd <- downloadHandler(
    filename = function() {
      paste0("updated_", original_file_name())  # Use a modified version of the original file name
    },
    content = function(file) {
      writeLines(updated_rmd(), file)
    }
  )
  
  # Conversion logic (similar to previous versions)
  observeEvent(input$convert, {
    req(updated_rmd())  # Ensure there's content to convert
    
    # Save the updated content to a temporary file for conversion
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(updated_rmd(), temp_rmd)
    
    formats <- input$formats
    temp_dir <- tempfile()  # Create a temporary directory for the converted files
    dir_create(temp_dir)    # Ensure the directory exists
    
    # Basic error handling and user feedback
    if (length(formats) == 0) {
      output$status <- renderText("Please select at least one output format.")
      return()
    }
    
    # Show the progress notification and clear the log
    shinyjs::show("processingNotification")
    shinyjs::html(id = "consoleLog", html = "", add = FALSE)  # Clear previous logs
    
    updateLog("Conversion started...")
    
    output$status <- renderText("Converting...")
    
    output_files <- list()  # Initialize list for storing file paths
    
    # Convert to Beamer Presentation with theme
    if ("beamer" %in% formats) {
      tryCatch({
        updateLog("Rendering Beamer presentation...")
        output_file_beamer <- rmarkdown::render(temp_rmd, 
                                                output_format = beamer_presentation(theme = input$theme_beamer, font_size = input$fontsize, keep_tex = TRUE),
                                                output_dir = temp_dir)  # Save directly to the temp directory
        output_files$beamer <- output_file_beamer
        updateLog(paste("Beamer file generated:", output_file_beamer))
      }, error = function(e) {
        updateLog(paste("Error converting to Beamer:", e$message))
      })
    }
    
    # Convert to knitr PDF
    if ("knitr" %in% formats) {
      tryCatch({
        updateLog("Rendering Knitr PDF...")
        output_file_knitr <- rmarkdown::render(temp_rmd, output_format = "pdf_document", output_dir = temp_dir)
        output_files$knitr <- output_file_knitr
        updateLog(paste("Knitr PDF file generated:", output_file_knitr))
      }, error = function(e) {
        updateLog(paste("Error converting to Knitr PDF:", e$message))
      })
    }
    
    # Convert to R HTML presentation (ioslides)
    if ("html" %in% formats) {
      tryCatch({
        updateLog("Rendering HTML presentation...")
        output_file_html <- rmarkdown::render(temp_rmd, output_format = "ioslides_presentation", output_dir = temp_dir)
        output_files$html <- output_file_html
        updateLog(paste("HTML file generated:", output_file_html))
      }, error = function(e) {
        updateLog(paste("Error converting to HTML:", e$message))
      })
    }
    
    # Convert to Word Document
    if ("word" %in% formats) {
      tryCatch({
        updateLog("Rendering Word document...")
        output_file_word <- rmarkdown::render(temp_rmd, output_format = "word_document", output_dir = temp_dir)
        output_files$word <- output_file_word
        updateLog(paste("Word document generated:", output_file_word))
      }, error = function(e) {
        updateLog(paste("Error converting to Word Document:", e$message))
      })
    }
    
    # Ensure files exist before attempting to zip
    valid_files <- unlist(output_files)
    valid_files <- valid_files[file.exists(valid_files)]  # Only include files that exist
    
    if (length(valid_files) > 0) {
      # Copy files to a flat temporary directory for zipping
      flat_dir <- tempfile()
      dir_create(flat_dir)
      
      # Copy the files to the flat directory with their base names only
      file_copy(valid_files, file.path(flat_dir, basename(valid_files)), overwrite = TRUE)
      
      # Add timestamp to the filename
      timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      
      updateLog("All conversions completed.")
      
      # Once done, notify the user and hide the progress notification
      shinyjs::hide("processingNotification")
      output$status <- renderText("Conversion completed. Download your files below.")
      
      # Create download handler for converted files with a timestamped zip filename
      output$download <- downloadHandler(
        filename = function() { paste0("converted_", original_file_name(), "_", timestamp, ".zip") },
        content = function(file) {
          # Use zip::zipr to zip without deep folder structures
          zipr(file, files = dir_ls(flat_dir, recurse = FALSE), root = flat_dir)
        }
      )
    } else {
      shinyjs::hide("processingNotification")
      output$status <- renderText("No valid files were generated. Please try again.")
    }
  })
}

shinyApp(ui = ui, server = server)
