# rmd_core.R - the actual R Markdown -> {Beamer PDF, Knitr PDF, HTML
# presentation, Word doc} conversion logic, usable two ways:
#   1. sourced by app.R (the Shiny app's Convert button calls these
#      functions directly - no duplicated per-format render blocks)
#   2. run directly via `Rscript rmd_core.R convert <in.Rmd> <out.zip>
#      --formats ...` when a file is too large/sensitive to upload, or a
#      render failure needs local debugging the deployed container
#      doesn't give you (no R console, no pandoc/knitr log beyond what
#      the on-page console box shows).
# Same install needed either way: R + rmarkdown + zip, plus pandoc (and a
# LaTeX distribution for beamer/knitr PDF output) on PATH - see README.

suppressMessages({
  library(rmarkdown)
  library(zip)
})

FORMAT_LABELS <- c(
  beamer = "Beamer presentation",
  knitr = "Knitr PDF",
  html = "HTML presentation",
  word = "Word document"
)

# Renders one format from the (already frontmatter-configured) input Rmd.
# "beamer" passes an explicit output_format object (its 4 sub-options
# override anything already in the doc's YAML); "html"/"word" pass a plain
# format-name string, so rmarkdown pulls ioslides_presentation/
# word_document options straight from the .Rmd's own frontmatter - this
# matches exactly what the Shiny app's Convert button already did.
#
# "knitr" (pdf_document) is different: pdf_document has no "theme"
# argument, and no "documentclass" argument either - a LaTeX document
# class can only be set via the top-level "documentclass" YAML key. But
# that key is document-wide, not scoped to pdf_document - confirmed
# locally that writing it into the shared frontmatter breaks beamer's
# compile too (its explicit beamer_presentation() object does NOT shield
# it from a conflicting top-level documentclass already in the doc's own
# metadata: "! Undefined control sequence" from \begin{frame} etc, since
# the doc silently compiled as the wrong LaTeX class instead of beamer).
# So pdf_documentclass here is applied ONLY to this one render() call, via
# a pandoc --variable override, and is never written into the shared .Rmd.
render_one_format <- function(fmt, input_rmd, out_dir, beamer_theme, beamer_color,
                               beamer_slide_level, beamer_font,
                               pdf_documentclass = "default") {
  base <- tools::file_path_sans_ext(basename(input_rmd))
  switch(fmt,
    "beamer" = rmarkdown::render(
      input = input_rmd,
      output_format = beamer_presentation(
        theme = beamer_theme, colortheme = beamer_color,
        slide_level = beamer_slide_level, fonttheme = beamer_font
      ),
      output_file = file.path(out_dir, paste0("output_beamer_", base, ".pdf"))
    ),
    "knitr" = rmarkdown::render(
      input = input_rmd,
      output_format = pdf_document(
        pandoc_args = if (identical(pdf_documentclass, "default") || is.null(pdf_documentclass)) {
          NULL
        } else {
          c("--variable", paste0("documentclass=", pdf_documentclass))
        }
      ),
      output_file = file.path(out_dir, paste0("output_knitr_", base, ".pdf"))
    ),
    "html" = rmarkdown::render(
      input = input_rmd, output_format = "ioslides_presentation",
      output_file = file.path(out_dir, paste0("output_html_", base, ".html"))
    ),
    "word" = rmarkdown::render(
      input = input_rmd, output_format = "word_document",
      output_file = file.path(out_dir, paste0("output_word_", base, ".docx"))
    ),
    stop("Unknown format '", fmt, "' - use beamer, knitr, html, and/or word.")
  )
}

# Renders every requested format into out_dir. A single format failing
# (e.g. a LaTeX error in one section) doesn't abort the others - each is
# wrapped individually and logged via `log`, same as the Shiny app's
# Convert button always did. Only throws if NONE of the requested formats
# produced a file.
op_convert <- function(input_rmd, formats, out_dir,
                        beamer_theme = "default", beamer_color = "default",
                        beamer_slide_level = 2, beamer_font = "default",
                        pdf_documentclass = "default",
                        log = message) {
  if (length(formats) == 0) stop("Select at least one output format.")
  unknown <- setdiff(formats, names(FORMAT_LABELS))
  if (length(unknown) > 0) {
    stop("Unknown format(s): ", paste(unknown, collapse = ", "),
         " - use beamer, knitr, html, and/or word.")
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  output_files <- list()
  for (fmt in formats) {
    log(paste0("Rendering ", FORMAT_LABELS[[fmt]], "..."))
    tryCatch({
      f <- render_one_format(fmt, input_rmd, out_dir, beamer_theme, beamer_color,
                              beamer_slide_level, beamer_font, pdf_documentclass)
      output_files[[fmt]] <- f
      log(paste(FORMAT_LABELS[[fmt]], "generated:", f))
    }, error = function(e) {
      log(paste0("Error converting ", FORMAT_LABELS[[fmt]], ": ", conditionMessage(e)))
    })
  }

  valid_files <- unlist(output_files)
  valid_files <- valid_files[file.exists(valid_files)]
  if (length(valid_files) == 0) {
    stop("No valid files were generated - see the format-specific errors above.")
  }
  valid_files
}

# op_convert() + flatten-and-zip, matching the Shiny app's download step.
op_convert_zip <- function(input_rmd, formats, zip_path, ...) {
  render_dir <- tempfile()
  dir.create(render_dir, recursive = TRUE)
  on.exit(unlink(render_dir, recursive = TRUE, force = TRUE))

  valid_files <- op_convert(input_rmd, formats, render_dir, ...)

  flat_dir <- tempfile()
  dir.create(flat_dir, recursive = TRUE)
  on.exit(unlink(flat_dir, recursive = TRUE, force = TRUE), add = TRUE)
  file.copy(valid_files, file.path(flat_dir, basename(valid_files)), overwrite = TRUE)

  zip::zipr(zip_path, files = list.files(flat_dir, full.names = TRUE), root = flat_dir)
  invisible(zip_path)
}

# ---- CLI ----

.rmd_core_cli <- function(argv) {
  usage <- function() {
    cat(
      "Usage: Rscript rmd_core.R convert <in.Rmd> <out.zip> --formats F1,F2,...\n\n",
      "Formats (comma-separated, at least one required): beamer, knitr, html, word\n\n",
      "Beamer-only options (ignored for the other formats, which read their\n",
      "theme from the .Rmd's own YAML frontmatter, same as the Shiny app):\n",
      "  --beamer-theme T        default: default\n",
      "  --beamer-color T        default: default\n",
      "  --beamer-slide-level N  default: 2\n",
      "  --beamer-font T         default: default\n\n",
      "Knitr(PDF)-only option (a LaTeX document class - article/report/book/\n",
      "memoir; applied only to this one format's render, not written into\n",
      "the shared .Rmd, since it would otherwise break a beamer render in\n",
      "the same batch):\n",
      "  --pdf-documentclass T   default: default\n",
      sep = ""
    )
  }
  if (length(argv) == 0 || argv[1] != "convert") { usage(); quit(status = 1) }
  rest <- argv[-1]
  if (length(rest) < 2) { usage(); quit(status = 1) }
  input_rmd <- rest[1]; output_zip <- rest[2]
  flags <- rest[-(1:2)]

  get_flag <- function(name, default = NULL) {
    i <- match(paste0("--", name), flags)
    if (is.na(i) || i == length(flags)) default else flags[i + 1]
  }

  formats_arg <- get_flag("formats")
  if (is.null(formats_arg)) {
    cat("ERROR: --formats is required\n\n")
    usage()
    quit(status = 1)
  }
  formats <- trimws(strsplit(formats_arg, ",")[[1]])

  result <- tryCatch({
    op_convert_zip(
      input_rmd, formats, output_zip,
      beamer_theme = get_flag("beamer-theme", "default"),
      beamer_color = get_flag("beamer-color", "default"),
      beamer_slide_level = as.numeric(get_flag("beamer-slide-level", "2")),
      beamer_font = get_flag("beamer-font", "default"),
      pdf_documentclass = get_flag("pdf-documentclass", "default"),
      log = function(msg) cat(msg, "\n")
    )
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    quit(status = 1)
  })

  cat("Wrote:", result, "\n")
  invisible(NULL)
}

# Only actually run the CLI dispatcher when THIS file was the direct
# target of `Rscript rmd_core.R ...` - checking merely `!interactive()`
# would also be true when the deployed Shiny app (itself started
# non-interactively) sources this file, which would incorrectly trigger
# quit(status=1) from inside a running server and kill the whole process.
.rmd_core_invoked_file <- local({
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) NULL else sub("^--file=", "", file_arg[1])
})
if (!is.null(.rmd_core_invoked_file) && basename(.rmd_core_invoked_file) == "rmd_core.R") {
  .rmd_core_cli(commandArgs(trailingOnly = TRUE))
}
