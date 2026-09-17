# ============================================================================
# PCE-430 Sound Level Meter Explorer
# A teaching-oriented Shiny app for visualizing PCE-430 exports (.OCT/.SWN/.CSD)
#
# - "Single site" tab: all parameters for one site, with brief interpretation
#   notes next to each plot/table (collapsible "How to read this" boxes).
# - "Compare sites" tab: side-by-side comparison across any sites you've loaded.
#
# Setup (once):
#   install.packages(c("shiny", "bslib", "plotly", "DT"))
#
# Run:
#   shiny::runApp("app.R")
#
# Data: use "Add site" in the sidebar to upload one or more of that site's
# .OCT / .SWN / .CSD files under a site name. A site can have any subset of
# the three file types -- the app only shows what's available.
# ============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)

source("pce430_import.R")

# ---------------------------------------------------------------------------
# Interpretation notes (course-facing). One short sentence each, matching the
# definitions used when this app was scoped.
# ---------------------------------------------------------------------------
metric_tips <- list(
  LAeq   = "LAeq: the energy-average A-weighted level over the period -- the standard 'overall loudness' summary. Because dB is logarithmic, this is NOT the arithmetic mean of the raw readings (see note below).",
  L10    = "L10: the level exceeded 10% of the time -- reflects the louder, more intermittent moments (e.g. passing traffic).",
  L50    = "L50: the level exceeded 50% of the time -- a rough 'typical/median' level.",
  L90    = "L90: the level exceeded 90% of the time -- the standard proxy for background/ambient noise once transient loud events are excluded.",
  LAFmax = "LAFmax: the single loudest instantaneous A-weighted, Fast-response reading in the period.",
  LAFmin = "LAFmin: the single quietest instantaneous A-weighted, Fast-response reading in the period.",
  LAFsd  = "LAFsd: standard deviation of the A-weighted Fast level -- how variable/bursty the sound environment was, independent of its average level.",
  LAF    = "LAF / LBF / LCF / LZF: the instantaneous Fast-response level in each weighting at the end of this block (not a statistic over time).",
  LAsel  = "LAsel (Sound Exposure Level): total acoustic energy of an event compressed into an equivalent 1-second dose, in dB. Scales with both level and duration.",
  LAe    = "LAe: the same sound exposure as LAsel, but in LINEAR units (e.g. Pa²·h), not dB -- shown in scientific notation. Never average this column the way you would a dB column.",
  LCpeak = "LCpeak: the true (unaveraged) maximum C-weighted peak pressure level -- used for impulsive/impact noise.",
  OVLD   = "OVLD = 1 means the meter's input clipped/overloaded during that reading; treat flagged rows with caution or exclude them.",
  PAUSE  = "PAUSE flags whether logging was paused for that record -- a status flag, not an acoustic measurement.",
  SPLAF  = "SPL_AF: instantaneous A-weighted, Fast level -- the human-loudness-perception trace.",
  SPLBF  = "SPL_BF: instantaneous B-weighted level -- a largely obsolete weighting, included for completeness.",
  SPLCF  = "SPL_CF: instantaneous C-weighted, Fast level -- flatter response, more sensitive to low frequencies and peaks.",
  SPLZF  = "SPL_ZF: instantaneous unweighted (Z) broadband level -- the raw physical total that the octave bands in this row partition by frequency.",
  LEQ_A  = "LEQ(A): 1-second energy-average A-weighted level -- the per-second version of LAeq.",
  PEAK_C = "PEAK(C): the true C-weighted peak within that second -- used to catch sharp transients.",
  MAX_Z_F = "MAX(Z,F): the maximum unweighted, Fast-response level within that second."
)

db_average_note <- tags$p(
  style = "font-size:90%; color:#555;",
  tags$b("Why this matters: "),
  "dB is a logarithmic ratio, so decibel values cannot be averaged arithmetically ",
  "-- doing so is always biased low, and more so for spiky data. This app averages ",
  "level columns in the linear (energy) domain and converts back to dB, exactly ",
  "the way the meter itself computes LAeq. Percentiles (L10/L50/L90) are fine to ",
  "compare directly, since order statistics don't have this problem."
)

tip_box <- function(text, title = "How to read this") {
  tags$details(
    class = "tip-box",
    tags$summary(paste("ℹ️", title)),
    tags$div(style = "margin-top:6px; font-size:90%; color:#333;", text)
  )
}

metric_tip_table <- function(cols) {
  cols <- intersect(cols, names(metric_tips))
  if (length(cols) == 0) return(NULL)
  tags$ul(
    style = "font-size:88%; color:#333; padding-left:18px;",
    lapply(cols, function(cc) tags$li(tags$b(cc), ": ", metric_tips[[cc]]))
  )
}

# In SLM mode the PCE-430 writes the LN summary statistics to the .CSD file,
# but in 1/1OCT mode the .CSD is a per-minute octave-band log instead -- same
# extension, different columns. Check content, not extension.
csd_is_summary <- function(csd) {
  !is.null(csd) && all(c("LAeq", "L90", "LAFmax") %in% names(csd$data))
}

octave_band_hz <- function(band_cols) {
  # "X8Hz" -> 8, "X31_5Hz" -> 31.5
  raw <- sub("^X", "", band_cols)
  raw <- sub("Hz$", "", raw)
  as.numeric(gsub("_", ".", raw))
}

# small helper so the app still runs if about.md is briefly missing
# (kept dependency-free: no "markdown" package required)
includeMarkdown_safe <- function(path) {
  if (file.exists(path)) {
    tags$div(style = "max-width:800px; white-space:pre-wrap; line-height:1.5;",
             paste(readLines(path, warn = FALSE), collapse = "\n"))
  } else {
    tags$p("about.md not found.")
  }
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_sidebar(
  title = "PCE-430 Sound Level Explorer",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    .tip-box { background:#f4f8fb; border-left:4px solid #2c7fb8; padding:8px 12px; margin:10px 0 16px 0; border-radius:4px; }
    .tip-box summary { cursor:pointer; font-weight:600; color:#2c7fb8; }
    .site-list { font-size:90%; }
  "))),

  sidebar = sidebar(
    width = 320,
    h5("Add site data"),
    helpText("Upload the .OCT / .SWN / .CSD files for one site, give it a name, and add it. A site needs only the file types you actually have."),
    textInput("new_site_name", "Site name", placeholder = "e.g. Forest edge"),
    fileInput("new_site_files", "PCE-430 files for this site",
              multiple = TRUE, accept = c(".OCT", ".SWN", ".CSD", ".oct", ".swn", ".csd")),
    actionButton("add_site", "Add / update site", class = "btn-primary btn-sm w-100"),
    tags$hr(),
    actionButton("load_example", "Load bundled example (1 site)", class = "btn-outline-secondary btn-sm w-100"),
    tags$hr(),
    h5("Loaded sites"),
    uiOutput("site_list_ui")
  ),

  navset_tab(
    nav_panel(
      "Single site",
      fluidRow(
        column(6, selectInput("site_select", "Site", choices = NULL)),
      ),
      uiOutput("single_site_ui")
    ),
    nav_panel(
      "Compare sites",
      fluidRow(
        column(6, selectizeInput("compare_sites", "Sites to compare",
                                  choices = NULL, multiple = TRUE)),
        column(6, selectInput("compare_metric", "Broadband metric for comparison",
                               choices = c("LAeq (A-weighted energy average)" = "laeq",
                                           "L90 (background/ambient proxy)"   = "l90",
                                           "LAFmax (loudest moment)"          = "lafmax")))
      ),
      tip_box(paste(
        "Each row/second within a site-visit is NOT an independent statistical replicate",
        "-- it's one continuous, autocorrelated recording. For a real GLMM comparison between",
        "sites, treat each *session/visit* as one replicate (e.g. use its LAeq, L10/L50/L90, LAFmax",
        "the way the .CSD summary already reports them), rather than each second."
      )),
      h5("Overall level by site"),
      plotlyOutput("compare_bar", height = 320),
      tags$hr(),
      h5("Distribution of per-second levels by site"),
      tip_box("The box shows the median and interquartile range of 1-second readings; whiskers/outliers show extremes like LAFmax. A wide box = a variable/bursty site; a narrow box = a steady one (compare to LAFsd)."),
      plotlyOutput("compare_box", height = 380),
      tags$hr(),
      h5("Spectral profile comparison (octave bands)"),
      tip_box("Mean level per octave band (correct energetic average, not a naive dB mean) for each site with octave-band data. This is a site's acoustic 'fingerprint' across frequency -- useful for spotting e.g. traffic-dominated low-frequency sites vs. insect/bird-dominated high-frequency sites."),
      plotlyOutput("compare_spectrum", height = 400),
      tags$hr(),
      h5("Summary statistics table"),
      DTOutput("compare_table")
    ),
    nav_panel(
      "About / methods",
      includeMarkdown_safe("about.md")
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  sites <- reactiveValues(store = list())  # name -> list(oct=, swn=, csd=)

  refresh_site_choices <- function() {
    nms <- names(sites$store)
    updateSelectInput(session, "site_select", choices = nms, selected = if (length(nms)) nms[1] else NULL)
    updateSelectizeInput(session, "compare_sites", choices = nms, selected = nms, server = FALSE)
  }

  observeEvent(input$add_site, {
    req(input$new_site_name, input$new_site_files)
    nm <- trimws(input$new_site_name)
    if (nm == "") { showNotification("Please give the site a name.", type = "error"); return() }

    entry <- sites$store[[nm]]
    if (is.null(entry)) entry <- list(oct = NULL, swn = NULL, csd = NULL)

    fdf <- input$new_site_files
    for (i in seq_len(nrow(fdf))) {
      ext <- toupper(tools::file_ext(fdf$name[i]))
      parsed <- tryCatch(read_pce430(fdf$datapath[i]), error = function(e) {
        showNotification(paste("Could not parse", fdf$name[i], ":", conditionMessage(e)), type = "error")
        NULL
      })
      if (!is.null(parsed)) {
        entry[[tolower(ext)]] <- parsed
      }
    }
    sites$store[[nm]] <- entry
    refresh_site_choices()
    updateTextInput(session, "new_site_name", value = "")
    showNotification(paste("Site", shQuote(nm), "updated."), type = "message")
  })

  observeEvent(input$load_example, {
    ex_dir <- "data_samples"
    if (!dir.exists(ex_dir)) { showNotification("No bundled example data found.", type = "error"); return() }
    entry <- list(
      oct = tryCatch(read_pce430(file.path(ex_dir, "DATA0002.OCT")), error = function(e) NULL),
      swn = tryCatch(read_pce430(file.path(ex_dir, "DATA0086.SWN")), error = function(e) NULL),
      csd = tryCatch(read_pce430(file.path(ex_dir, "DATA0070.CSD")), error = function(e) NULL)
    )
    sites$store[["Example site"]] <- entry
    refresh_site_choices()
    showNotification("Loaded bundled example as 'Example site'.", type = "message")
  })

  output$site_list_ui <- renderUI({
    nms <- names(sites$store)
    if (length(nms) == 0) return(helpText("No sites loaded yet."))
    tags$div(class = "site-list",
      lapply(nms, function(nm) {
        types <- names(Filter(Negate(is.null), sites$store[[nm]]))
        fluidRow(
          column(8, tags$span(tags$b(nm), tags$br(), tags$small(paste(toupper(types), collapse = ", ")))),
          column(4, actionButton(paste0("rm_", make.names(nm)), "Remove", class = "btn-sm btn-outline-danger"))
        )
      })
    )
  })

  registered_rm_buttons <- new.env()  # tracks which "Remove" buttons already have an observer wired up

  observe({
    nms <- names(sites$store)
    for (nm in nms) {
      btn_id <- paste0("rm_", make.names(nm))
      if (!is.null(registered_rm_buttons[[btn_id]])) next  # already wired, skip
      registered_rm_buttons[[btn_id]] <- TRUE
      local({
        nm_local <- nm
        observeEvent(input[[btn_id]], {
          sites$store[[nm_local]] <- NULL
          refresh_site_choices()
        }, ignoreInit = TRUE)
      })
    }
  })

  current <- reactive({
    req(input$site_select)
    sites$store[[input$site_select]]
  })

  # ---------------- Single site tab ----------------
  output$single_site_ui <- renderUI({
    site <- current()
    req(site)
    tagList(
      if (!is.null(site$oct)) tagList(
        h5("Octave-band spectrogram (SPL_ZF, unweighted, per second)"),
        tip_box("Time on the x-axis, octave band (Hz) on the y-axis, colour = level in dB. Warm colours flag which frequencies dominate at each moment -- e.g. a steady warm band around 8 Hz suggests wind/low-frequency rumble, brief warm patches at higher bands suggest transient sources like birds, insects, or a passing vehicle."),
        plotlyOutput("oct_heatmap", height = 380),
        tags$hr(),
        h5("Broadband weighted levels over time"),
        checkboxGroupInput("oct_weightings", NULL,
                            choices = c("A" = "SPLAF", "B" = "SPLBF", "C" = "SPLCF", "Z" = "SPLZF"),
                            selected = c("SPLAF", "SPLCF", "SPLZF"), inline = TRUE),
        tip_box(HTML(paste0(
          "A (SPL_AF) is the standard perceptual-loudness trace; C (SPL_CF) is flatter and more sensitive to low frequencies/peaks; ",
          "Z (SPL_ZF) is the raw unweighted total. Compare A vs Z: a big gap usually means the sound is dominated by low frequencies ",
          "the ear discounts (e.g. wind, distant traffic rumble)."
        ))),
        plotlyOutput("oct_broadband", height = 320),
        tags$hr()
      ),
      if (!is.null(site$swn)) tagList(
        h5("Continuous broadband log (LEQ / PEAK / MAX)"),
        tip_box("LEQ(A) is the running 1-second energy-average level; PEAK(C) is the true peak within that second (good for catching sharp bangs/impacts); MAX(Z,F) is the loudest unweighted Fast reading in that second. A file like this is well suited to catching a discrete loud event."),
        plotlyOutput("swn_plot", height = 340),
        tags$hr()
      ),
      if (csd_is_summary(site$csd)) tagList(
        h5("Session summary statistics (.CSD)"),
        DTOutput("csd_table"),
        db_average_note,
        tags$hr()
      ) else if (!is.null(site$csd)) tagList(
        h5("Session summary statistics (.CSD)"),
        tags$p(style = "font-size:90%; color:#555;",
               "This site's .CSD file was exported with the meter in 1/1OCT mode, so it contains a per-minute octave-band log rather than the LAeq/L10/L50/L90 summary table. The octave-band plots above already cover this recording."),
        tags$hr()
      ),
      if (is.null(site$oct) && is.null(site$swn) && is.null(site$csd))
        tags$p("No data loaded for this site yet -- use the sidebar to add files.")
    )
  })

  output$oct_heatmap <- renderPlotly({
    site <- current(); req(site$oct)
    df <- site$oct$data
    band_cols <- site$oct$meta$octave_bands_hz
    freqs <- octave_band_hz(band_cols)
    ord <- order(freqs)
    band_cols <- band_cols[ord]; freqs <- freqs[ord]

    z <- as.matrix(df[, band_cols])
    plot_ly(x = df$datetime, y = freqs, z = t(z), type = "heatmap",
            colorscale = "YlOrRd", colorbar = list(title = "dB")) |>
      layout(yaxis = list(title = "Octave band centre frequency (Hz)", type = "log"),
             xaxis = list(title = "Time"))
  })

  output$oct_broadband <- renderPlotly({
    site <- current(); req(site$oct)
    df <- site$oct$data
    sel <- input$oct_weightings; req(length(sel) > 0)
    p <- plot_ly()
    for (cc in sel) {
      p <- add_trace(p, x = df$datetime, y = df[[cc]], type = "scatter", mode = "lines", name = cc)
    }
    p |> layout(yaxis = list(title = "dB"), xaxis = list(title = "Time"))
  })

  output$swn_plot <- renderPlotly({
    site <- current(); req(site$swn)
    df <- site$swn$data
    plot_ly(df, x = ~datetime) |>
      add_trace(y = ~LEQ_A, type = "scatter", mode = "lines", name = "LEQ (A)") |>
      add_trace(y = ~PEAK_C, type = "scatter", mode = "lines", name = "PEAK (C)") |>
      add_trace(y = ~MAX_Z_F, type = "scatter", mode = "lines", name = "MAX (Z,F)") |>
      layout(yaxis = list(title = "dB"), xaxis = list(title = "Time"))
  })

  output$csd_table <- renderDT({
    site <- current(); req(csd_is_summary(site$csd))
    row <- site$csd$data
    cols <- setdiff(names(row), c("Date", "Time", "datetime"))
    tbl <- data.frame(
      Metric = cols,
      Value = sapply(cols, function(cc) format(row[[cc]], digits = 4)),
      `What it means` = sapply(cols, function(cc) metric_tips[[cc]] %||% ""),
      check.names = FALSE
    )
    datatable(tbl, rownames = FALSE, options = list(dom = "t", pageLength = 25))
  })

  # ---------------- Compare sites tab ----------------
  compare_summary <- reactive({
    req(length(input$compare_sites) > 0)
    do.call(rbind, lapply(input$compare_sites, function(nm) {
      site <- sites$store[[nm]]
      laeq <- l90 <- lafmax <- NA_real_
      if (csd_is_summary(site$csd)) {
        laeq   <- site$csd$data$LAeq[1]
        l90    <- site$csd$data$L90[1]
        lafmax <- site$csd$data$LAFmax[1]
      } else if (!is.null(site$oct)) {
        laeq   <- energetic_mean_db(site$oct$data$SPLAF)
        l90    <- unname(quantile(site$oct$data$SPLAF, 0.10, na.rm = TRUE)) # exceeded 90% => 10th pct
        lafmax <- max(site$oct$data$SPLAF, na.rm = TRUE)
      } else if (!is.null(site$swn)) {
        laeq   <- energetic_mean_db(site$swn$data$LEQ_A)
        l90    <- unname(quantile(site$swn$data$LEQ_A, 0.10, na.rm = TRUE))
        lafmax <- max(site$swn$data$LEQ_A, na.rm = TRUE)
      } else if (!is.null(site$csd) && "SPLAF" %in% names(site$csd$data)) {
        # octave-mode .CSD is the only file for this site: use its per-minute log
        laeq   <- energetic_mean_db(site$csd$data$SPLAF)
        l90    <- unname(quantile(site$csd$data$SPLAF, 0.10, na.rm = TRUE))
        lafmax <- max(site$csd$data$SPLAF, na.rm = TRUE)
      }
      data.frame(Site = nm, LAeq = laeq, L90 = l90, LAFmax = lafmax)
    }))
  })

  output$compare_bar <- renderPlotly({
    df <- compare_summary(); req(nrow(df) > 0)
    ycol <- switch(input$compare_metric, laeq = "LAeq", l90 = "L90", lafmax = "LAFmax")
    plot_ly(df, x = ~Site, y = as.formula(paste0("~", ycol)), type = "bar") |>
      layout(yaxis = list(title = "dB"))
  })

  output$compare_box <- renderPlotly({
    req(length(input$compare_sites) > 0)
    p <- plot_ly(type = "box")
    for (nm in input$compare_sites) {
      site <- sites$store[[nm]]
      vals <- NULL
      if (!is.null(site$oct)) vals <- site$oct$data$SPLAF
      else if (!is.null(site$swn)) vals <- site$swn$data$LEQ_A
      if (!is.null(vals)) p <- add_trace(p, y = vals, name = nm, boxpoints = "outliers")
    }
    p |> layout(yaxis = list(title = "dB (per-second A-weighted level)"))
  })

  output$compare_spectrum <- renderPlotly({
    req(length(input$compare_sites) > 0)
    p <- plot_ly(type = "scatter", mode = "lines+markers")
    any_data <- FALSE
    for (nm in input$compare_sites) {
      site <- sites$store[[nm]]
      if (is.null(site$oct)) next
      any_data <- TRUE
      band_cols <- site$oct$meta$octave_bands_hz
      freqs <- octave_band_hz(band_cols)
      ord <- order(freqs)
      band_cols <- band_cols[ord]; freqs <- freqs[ord]
      levels <- sapply(band_cols, function(cc) energetic_mean_db(site$oct$data[[cc]]))
      p <- add_trace(p, x = freqs, y = levels, name = nm)
    }
    if (!any_data) return(plotly_empty(type = "scatter", mode = "lines") |>
                             layout(title = "No sites with octave-band (.OCT) data selected"))
    p |> layout(xaxis = list(title = "Octave band centre frequency (Hz)", type = "log"),
                yaxis = list(title = "Mean level (dB, energetic average)"))
  })

  output$compare_table <- renderDT({
    df <- compare_summary(); req(nrow(df) > 0)
    datatable(df, rownames = FALSE, options = list(dom = "t")) |>
      formatRound(columns = c("LAeq", "L90", "LAFmax"), digits = 1)
  })
}

shinyApp(ui, server)
