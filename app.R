library(shiny)
library(rmarkdown)
library(shinyAce)
library(shinyjs)
library(zip)
library(fs)
library(yaml)

ui <- fluidPage(
  tags$head(
    # Include Google Fonts and custom CSS
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Noto+Sans:wght@400;700&display=swap"),
    
    # Inline CSS for customizing the UI appearance
    tags$style(HTML("
      body {
        font-family: 'Noto Sans', sans-serif;
        background-color: #f9f3eb; /* light peach shade */
        color: #4d4d4d;
      }
      h1, h4 {
        color: #4d4d4d;
      }
      #file {
        margin-top: 15px;
      }
      .shiny-input-container {
        margin-bottom: 20px;
      }
      .form-group {
        margin-bottom: 15px;
      }
      #convert {
        background-color: #e89f71; /* peach shade */
        border: none;
        color: white;
        padding: 10px 20px;
        font-size: 16px;
        font-weight: bold;
        border-radius: 8px;
        cursor: pointer;
        transition: background-color 0.3s ease;
      }
      #convert:hover {
        background-color: #d88b5f; /* darker peach */
      }
      .btn {
        border-radius: 8px;
        background-color: #8fbc8f; /* light green */
        color: white;
        padding: 8px 15px;
        font-weight: bold;
        transition: background-color 0.3s ease;
      }
      .btn:hover {
        background-color: #7aa87a; /* darker green */
      }
      .well {
        background-color: #e3f1e1; /* light green shade */
        border: 1px solid #c7e1c7;
      }
    "))
  ),
  useShinyjs(),
  titlePanel("R Markdown to Multiple Formats"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload R Markdown File (.Rmd)", accept = ".Rmd"),
      
      # Beamer Presentation Options
      h4("Beamer Presentation Options"),
      wellPanel(
        # Beamer theme selection with color themes, slide themes, and font themes
        selectInput("theme_beamer", "Beamer Theme", 
                    choices = c("default", "AnnArbor", "Antibes", "Bergen", "Berkeley", "Berlin")),
        
        selectInput("color_beamer", "Beamer Color Theme",
                    choices = c("default", "albatross", "beetle", "crane", "dolphin")),
        
        numericInput("slide_level", "Slide Level", value = 2, min = 1),
        
        selectInput("font_beamer", "Beamer Font Theme",
                    choices = c("default", "serif", "structurebold", "structureitalicserif"))
      ),
      
      # PDF Document Options
      h4("PDF Document Options"),
      wellPanel(
        selectInput("theme_pdf", "PDF Theme", choices = c("default", "article", "report", "book", "memoir"))
      ),
      
      # HTML Presentation Options
      h4("HTML Presentation Options"),
      wellPanel(
        selectInput("theme_html", "HTML Theme", choices = c("default"))
      ),
      
      # Word Document Options
      h4("Word Document Options"),
      wellPanel(
        selectInput("theme_word", "Word Theme", choices = c("default"))
      ),
      
      # Output Formats
      h4("Select Output Formats"),
      checkboxGroupInput("formats", "Choose output formats:", 
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
  
  # Function to update the frontmatter, ensuring all themes are present and correctly formatted
  # Function to update the frontmatter, ensuring all themes are present and correctly formatted
  updateFrontmatter <- function() {
    frontmatter <- input$frontmatterEditor
    
    # Parse the frontmatter as YAML
    parsed_yaml <- tryCatch({
      yaml.load(frontmatter)
    }, error = function(e) {
      updateLog("Error parsing YAML frontmatter.")
      return(NULL)
    })
    
    # Ensure parsed YAML is valid
    if (!is.null(parsed_yaml)) {
      
      # Check for missing output keys and add defaults if needed
      if (is.null(parsed_yaml$output)) {
        parsed_yaml$output <- list()  # Initialize output if missing
      }
      
      ### Beamer presentation ###
      # If Beamer theme is "default", use simple atomic format
      ### Beamer Presentation ###
      if (input$theme_beamer == "default") {
        parsed_yaml$output$beamer_presentation <- "default"
      } else {
        if (!is.list(parsed_yaml$output$beamer_presentation)) {
          parsed_yaml$output$beamer_presentation <- list()
        }
        parsed_yaml$output$beamer_presentation$theme <- input$theme_beamer
        parsed_yaml$output$beamer_presentation$colortheme <- input$color_beamer
        parsed_yaml$output$beamer_presentation$slide_level <- input$slide_level
        parsed_yaml$output$beamer_presentation$fonttheme <- input$font_beamer
      }
      
      ### PDF Document ###
      # If PDF theme is "default", use simple atomic format
      if (input$theme_pdf == "default") {
        parsed_yaml$output$pdf_document <- "default"
      } else {
        if (!is.list(parsed_yaml$output$pdf_document)) {
          parsed_yaml$output$pdf_document <- list()
        }
        # Update PDF theme
        parsed_yaml$output$pdf_document$theme <- input$theme_pdf
      }
      
      ### HTML (ioslides) presentation ###
      # If HTML theme is "default", use simple atomic format
      if (input$theme_html == "default") {
        parsed_yaml$output$ioslides_presentation <- "default"
      } else {
        if (!is.list(parsed_yaml$output$ioslides_presentation)) {
          parsed_yaml$output$ioslides_presentation <- list()
        }
        # Update HTML theme
        parsed_yaml$output$ioslides_presentation$theme <- input$theme_html
      }
      
      ### Word Document ###
      # If Word theme is "default", use simple atomic format
      if (input$theme_word == "default") {
        parsed_yaml$output$word_document <- "default"
      } else {
        if (!is.list(parsed_yaml$output$word_document)) {
          parsed_yaml$output$word_document <- list()
        }
        # Update Word theme
        parsed_yaml$output$word_document$theme <- input$theme_word
      }
      
      # Convert updated YAML back to string
      new_frontmatter <- as.yaml(parsed_yaml)
      updateAceEditor(session, "frontmatterEditor", value = new_frontmatter)
    }
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
        updateSelectInput(session, "theme_beamer", selected = output_settings$beamer_presentation)
        updateCheckboxGroupInput(session, "formats", selected = c("beamer"))
      }
      if (!is.null(output_settings$pdf_document)) {
        updateSelectInput(session, "theme_pdf", selected = output_settings$pdf_document)
        updateCheckboxGroupInput(session, "formats", selected = c("knitr"))
      }
      if (!is.null(output_settings$ioslides_presentation)) {
        updateSelectInput(session, "theme_html", selected = output_settings$ioslides_presentation)
        updateCheckboxGroupInput(session, "formats", selected = c("html"))
      }
      if (!is.null(output_settings$word_document)) {
        updateSelectInput(session, "theme_word", selected = output_settings$word_document)
        updateCheckboxGroupInput(session, "formats", selected = c("word"))
      }
    }
    
    # Update the Ace editors
    updateAceEditor(session, "frontmatterEditor", value = frontmatter)
    updateAceEditor(session, "bodyEditor", value = body)
    
    # Store the updated Rmd content for further use
    updated_rmd(rmd_content)
  })
  
  # Listen to theme changes and update frontmatter
  observeEvent(input$theme_beamer, { updateFrontmatter() })
  observeEvent(input$color_beamer, { updateFrontmatter() })
  observeEvent(input$slide_level, { updateFrontmatter() })
  observeEvent(input$theme_pdf, { updateFrontmatter() })
  observeEvent(input$theme_html, { updateFrontmatter() })
  observeEvent(input$theme_word, { updateFrontmatter() })
  
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
    updateLog(paste("Temporary Rmd file path:", temp_rmd))
    
    # Write the content to the temporary file
    tryCatch({
      writeLines(updated_rmd(), temp_rmd)
    }, error = function(e) {
      updateLog(paste("Error writing temporary Rmd file:", e$message))
      return(NULL)
    })
    
    # Check if the file was written successfully
    if (!file.exists(temp_rmd)) {
      updateLog("Error: Temporary Rmd file does not exist.")
      output$status <- renderText("Error: Unable to create the temporary Rmd file.")
      return()
    }
    
    formats <- input$formats
    temp_dir <- tempfile()  # Create a temporary directory for the converted files
    dir_create(temp_dir)    # Ensure the directory exists
    updateLog(paste("Temporary output directory:", temp_dir))
    
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
    
    # Convert to Beamer Presentation with theme, colortheme, slidetheme, and fonttheme
    if ("beamer" %in% formats) {
      tryCatch({
        updateLog("Rendering Beamer presentation...")
        output_file_beamer <- rmarkdown::render(temp_rmd, 
                                                output_format = beamer_presentation(
                                                  theme = input$theme_beamer, 
                                                  colortheme = input$color_beamer, 
                                                  slide_level = input$slide_level,
                                                  fonttheme = input$font_beamer, 
                                                  keep_tex = TRUE),
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
