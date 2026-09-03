library(shiny)
# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Default Shiny upload cap is 5MB; an .Rmd with embedded images/data can
# exceed that. Matches nginx's client_max_body_size (200M) in
# nginx.conf.template - nginx's own default (1MB) sits in front of this
# and would 413 anything larger before Shiny ever saw it, so both had to
# move together.
options(shiny.maxRequestSize = 200 * 1024^2)

# List of required libraries
required_packages <- c("rmarkdown", "shinyAce", "shinyjs", "zip", "fs", "yaml")

# Function to check and install missing libraries
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}

# Install and load required packages
install_if_missing(required_packages)

# Shows a clear, unmissable "please refresh" banner when the Shiny session
# disconnects (server restart, crash, idle timeout, etc.). Shiny's own
# default disconnect behavior is just a subtle page-dimming effect - easy to
# miss unless you already know what a Shiny disconnect looks like. No
# auto-reconnect attempt: a Railway-hosted session is not reliably
# resumable, so the honest answer is always "refresh," not "wait."
# Kept identical across every app - copy verbatim, don't diverge.
disconnect_overlay <- function() {
  tagList(
    tags$head(tags$style(HTML("
      #shiny-disconnect-overlay {
        display: none;
        position: fixed;
        top: 0; left: 0; right: 0; bottom: 0;
        background: rgba(20, 20, 20, 0.75);
        z-index: 2147483647;
        align-items: center;
        justify-content: center;
      }
      #shiny-disconnect-overlay .box {
        background: #fff;
        border-radius: 8px;
        padding: 28px 32px;
        max-width: 380px;
        text-align: center;
        box-shadow: 0 4px 24px rgba(0,0,0,0.3);
        font-family: -apple-system, Segoe UI, Roboto, Arial, sans-serif;
      }
      #shiny-disconnect-overlay .title {
        font-size: 18px;
        font-weight: 600;
        color: #b02a2a;
        margin-bottom: 8px;
      }
      #shiny-disconnect-overlay .msg {
        font-size: 14px;
        color: #333;
        margin-bottom: 18px;
        line-height: 1.4;
      }
      #shiny-disconnect-overlay button {
        background: #2c7be5;
        color: #fff;
        border: none;
        border-radius: 5px;
        padding: 10px 22px;
        font-size: 14px;
        cursor: pointer;
      }
      #shiny-disconnect-overlay button:hover { background: #1a63c4; }
    ")),
    tags$script(HTML("
      $(document).on('shiny:disconnected', function() {
        document.getElementById('shiny-disconnect-overlay').style.display = 'flex';
      });
    "))),
    tags$div(id = "shiny-disconnect-overlay",
      tags$div(class = "box",
        tags$div(class = "title", "Connection lost"),
        tags$div(class = "msg",
          "This session has disconnected from the server. Your work in this ",
          "session can't be recovered — please refresh the page to start a new one."),
        tags$button("Refresh page", onclick = "location.reload()")
      )
    )
  )
}

# Visual styling pulled from the Vitrag (vitrag-6) design system - navy +
# teal palette, sharp/square corners (radius 0 throughout, not rounded),
# uppercase tracking-wide labels/buttons, Source Sans Pro. Layered on top of
# Shiny's default Bootstrap 3 via a plain CSS override block rather than a
# Shiny theming package, since only the visual language (not the
# React/Tailwind component structure) is being reused here. Font sizes were
# audited against vitrag-6's actual globals.css after an initial pass ran
# too small (13px tabs/12px labels vs. vitrag's real 14px minimums) - kept
# identical across every app's ui.R from here on, copy verbatim. Replaces
# this app's earlier one-off peach/green theme.
vitrag_theme <- function() {
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@400;600;700&display=swap"),
      tags$style(HTML("
        :root {
          --vblue: #384764;
          --vgreen: #00a99e;
          /* vitrag-6's source gives no hex for this one (only
             oklch(0.42 0.09 180)) - precisely computed via the culori
             library rather than eyeballed; an earlier guess (#00786f)
             was visibly off. */
          --vgreen-dark: #005c4f;
          --vorange: #ff7824;
          --vsection: #f2f5f9;
          /* vitrag's --border: oklch(0.91 0 0) and --muted-foreground:
             oklch(0.42 0.04 257), both precisely computed rather than
             guessed (form-control border was a hand-picked #c7ccd6
             before, unrelated to any real vitrag token). */
          --vborder: #e1e1e1;
          --vmuted: #3f4e63;
        }
        body {
          font-family: 'Source Sans 3', 'Source Sans Pro', system-ui, sans-serif;
          color: var(--vblue);
          background: #ffffff;
          font-size: 15px;
        }
        /* Bootstrap's default <small>/.help-block shrink to ~85% of a
           14px base (~12px) reads as genuinely too small once the base
           itself is 15px - fixed to a real, comfortable size instead of
           a relative shrink. */
        small, .help-block {
          font-size: 13px;
          color: var(--vmuted);
        }
        h1, h2, h3, h4, h5, legend {
          font-family: 'Source Sans 3', 'Source Sans Pro', system-ui, sans-serif;
          color: var(--vblue);
          font-weight: 600;
        }
        .container-fluid > h1:first-child {
          padding: 18px 0 8px;
          border-bottom: 3px solid var(--vgreen);
          margin-bottom: 4px;
        }
        a { color: var(--vblue); }
        a:hover { color: var(--vgreen-dark); }

        /* Tabs: bold uppercase labels, teal underline on the active tab -
           matches vitrag's .nav-link underline-indicator pattern. */
        .nav-tabs { border-bottom: 2px solid var(--vsection); }
        .nav-tabs > li > a {
          font-family: 'Source Sans 3', sans-serif;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.03em;
          font-size: 15px;
          padding: 12px 18px;
          color: var(--vblue);
          border-radius: 0;
          border: none;
          background: transparent;
        }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          color: var(--vblue);
          background: transparent;
          border: none;
          border-bottom: 3px solid var(--vgreen);
        }
        .nav-tabs > li > a:hover {
          background: var(--vsection);
          border: none;
          border-bottom: 3px solid var(--vgreen);
        }

        /* Sidebar: light section background, sharp corners, no shadow. */
        .well {
          background: var(--vsection);
          border: none;
          border-radius: 0;
          box-shadow: none;
        }

        /* Form fields: sharp corners, uppercase tracking-wide labels, teal focus ring. */
        label {
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.03em;
          font-size: 14px;
          color: var(--vblue);
        }
        .form-control {
          border-radius: 0;
          border: 1px solid var(--vborder);
          font-size: 15px;
          height: auto;
          padding: 8px 12px;
        }
        .form-control:focus {
          border-color: var(--vgreen);
          box-shadow: 0 0 0 1px var(--vgreen);
        }

        /* Buttons: sharp corners, bold uppercase, navy fill / teal hover -
           matches vitrag's .btn-vitrag. */
        .btn, .btn-default, .btn-primary {
          border-radius: 0;
          border: 2px solid var(--vblue);
          background: var(--vblue);
          color: #ffffff;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.03em;
          font-size: 15px;
          padding: 10px 24px;
          transition: all 0.15s ease;
        }
        .btn:hover, .btn-default:hover, .btn-primary:hover {
          background: var(--vgreen-dark);
          border-color: var(--vgreen-dark);
          color: #ffffff;
        }
        /* Positive/add actions get the teal instead of navy - still
           sharp-cornered/uppercase/bold like every other button. */
        .btn-success {
          border-radius: 0;
          border: 2px solid var(--vgreen-dark);
          background: var(--vgreen-dark);
          color: #ffffff;
          font-weight: 600;
          text-transform: uppercase;
          letter-spacing: 0.03em;
          font-size: 15px;
          padding: 10px 24px;
          transition: all 0.15s ease;
        }
        .btn-success:hover {
          background: var(--vblue);
          border-color: var(--vblue);
          color: #ffffff;
        }

        /* Radio/checkbox accent color. */
        input[type='radio'], input[type='checkbox'] { accent-color: var(--vgreen); }
      "))
    )
  )
}

ui <- fluidPage(
  disconnect_overlay(),
  vitrag_theme(),
  useShinyjs(),
  titlePanel("R Markdown to Multiple Formats"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload R Markdown File (.Rmd)", accept = ".Rmd"),

      # Beamer Presentation Options
      h4("Beamer Presentation Options"),
      wellPanel(
        # Beamer theme selection with color themes, slide themes, and font themes
        selectInput(
          "theme_beamer",
          "Beamer Theme",
          choices = c(
            "default",
            "AnnArbor",
            "Antibes",
            "Bergen",
            "Berkeley",
            "Berlin",
            "Boadilla",
            "CambridgeUS",
            "Copenhagen",
            "Darmstadt",
            "Dresden",
            "Frankfurt",
            "Goettingen",
            "Hannover",
            "Ilmenau",
            "JuanLesPins",
            "Luebeck",
            "Madrid",
            "Malmoe",
            "Marburg",
            "Montpellier",
            "PaloAlto",
            "Pittsburgh",
            "Rochester",
            "Singapore",
            "Szeged",
            "Warsaw"
          )
        ),

        selectInput(
          "color_beamer",
          "Beamer Color Theme",
          choices = c(
            "default",
            "albatross",
            "beetle",
            "crane",
            "dolphin",
            "fly",
            "seagull",
            "whale",
            "wolverine"
          )
        ),

        numericInput("slide_level", "Slide Level", value = 2, min = 1),

        selectInput(
          "font_beamer",
          "Beamer Font Theme",
          choices = c("default", "serif", "structurebold", "structureitalicserif")
        )
      ),

      # PDF Document Options
      h4("PDF Document Options"),
      wellPanel(selectInput(
        "theme_pdf",
        "PDF Theme",
        choices = c("default", "article", "report", "book", "memoir")
      )),

      # HTML Presentation Options
      h4("HTML Presentation Options"),
      wellPanel(selectInput(
        "theme_html", "HTML Theme", choices = c("default")
      )),

      # Word Document Options
      h4("Word Document Options"),
      wellPanel(selectInput(
        "theme_word", "Word Theme", choices = c("default")
      )),

      # Output Formats
      h4("Select Output Formats"),
      checkboxGroupInput(
        "formats",
        "Choose output formats:",
        choices = c(
          "Beamer PDF" = "beamer",
          "Knitr PDF" = "knitr",
          "R HTML Presentation" = "html",
          "Word Document" = "word"
        )
      ),

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
      hidden(div(id = "processingNotification", h4(
        "Processing... Please wait"
      ))),

      textOutput("analytics")
    )
  )
)

server <- function(input, output, session) {
  useShinyjs()

  # Function to update the log
  updateLog <- function(message) {
    shinyjs::html(id = "consoleLog",
                  html = paste0(message, "\n"),
                  add = TRUE)
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
        updateSelectInput(session,
                          "theme_beamer",
                          selected = output_settings$beamer_presentation)
        updateCheckboxGroupInput(session, "formats", selected = c("beamer"))
      }
      if (!is.null(output_settings$pdf_document)) {
        updateSelectInput(session, "theme_pdf", selected = output_settings$pdf_document)
        updateCheckboxGroupInput(session, "formats", selected = c("knitr"))
      }
      if (!is.null(output_settings$ioslides_presentation)) {
        updateSelectInput(session,
                          "theme_html",
                          selected = output_settings$ioslides_presentation)
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
  observeEvent(input$theme_beamer, {
    updateFrontmatter()
  })
  observeEvent(input$color_beamer, {
    updateFrontmatter()
  })
  observeEvent(input$slide_level, {
    updateFrontmatter()
  })
  observeEvent(input$theme_pdf, {
    updateFrontmatter()
  })
  observeEvent(input$theme_html, {
    updateFrontmatter()
  })
  observeEvent(input$theme_word, {
    updateFrontmatter()
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
  # Conversion logic (adjusted for unique file names)
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
        output_file_beamer <- rmarkdown::render(
          input = temp_rmd,
          output_format = beamer_presentation(
            theme = input$theme_beamer,
            colortheme = input$color_beamer,
            slide_level = input$slide_level,
            fonttheme = input$font_beamer
          ),
          output_file = file.path(
            temp_dir,
            paste0(
              "output_beamer_",
              tools::file_path_sans_ext(basename(temp_rmd)),
              ".pdf"
            )
          )
        )
        output_files$beamer <- output_file_beamer
        updateLog(paste("Beamer file generated:", output_file_beamer))
      }, error = function(e) {
        updateLog(paste("Error converting to Beamer:", e$message))
      })
    }

    # Convert to Knitr PDF
    if ("knitr" %in% formats) {
      tryCatch({
        updateLog("Rendering Knitr PDF...")
        output_file_knitr <- rmarkdown::render(
          input = temp_rmd,
          output_format = "pdf_document",
          output_file = file.path(
            temp_dir,
            paste0(
              "output_knitr_",
              tools::file_path_sans_ext(basename(temp_rmd)),
              ".pdf"
            )
          )
        )
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
        output_file_html <- rmarkdown::render(
          input = temp_rmd,
          output_format = "ioslides_presentation",
          output_file = file.path(
            temp_dir,
            paste0(
              "output_html_",
              tools::file_path_sans_ext(basename(temp_rmd)),
              ".html"
            )
          )
        )
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
        output_file_word <- rmarkdown::render(
          input = temp_rmd,
          output_format = "word_document",
          output_file = file.path(
            temp_dir,
            paste0(
              "output_word_",
              tools::file_path_sans_ext(basename(temp_rmd)),
              ".docx"
            )
          )
        )
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
        filename = function() {
          paste0("converted_",
                 original_file_name(),
                 "_",
                 timestamp,
                 ".zip")
        },
        content = function(file) {
          # Use zip::zipr to zip without deep folder structures
          zipr(file,
               files = dir_ls(flat_dir, recurse = FALSE),
               root = flat_dir)
        }
      )
    } else {
      shinyjs::hide("processingNotification")
      output$status <- renderText("No valid files were generated. Please try again.")
    }
  })


}

shinyApp(ui = ui, server = server)
