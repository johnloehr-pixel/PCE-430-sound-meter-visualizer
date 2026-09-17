# pce430_import.R
# ---------------------------------------------------------------------------
# Import functions for PCE-430 sound level meter export files (.OCT, .SWN, .CSD)
#
# All three file types share a common header block:
#   [Version] / [SN Inf] / [Logging On] / [Logger Step] / [GPS Inf] / [Setting]
# followed by a [Data] section: one tab-separated header line, then
# tab-separated data rows (Date as DDMMYYYY, Time as HHMMSS or HHMMSS.f).
#
# read_pce430() dispatches on file extension and returns a list:
#   list(meta = <named list of header metadata>, data = <data.frame>)
#
# IMPORTANT (dB units):
#   All "L*" columns are in decibels except LAe (linear sound exposure,
#   typically Pa^2*h or Pa^2*s -- appears in scientific notation, e.g. 2.461e-05).
#   Because dB is a log quantity, columns of dB values must NEVER be averaged
#   arithmetically. Use energetic_mean_db() below for any custom time
#   aggregation of level columns. Percentiles (L10/L50/L90) are safe to
#   average/compare directly since order statistics are transform-invariant.
# ---------------------------------------------------------------------------

#' Correct energetic (linear-domain) average of a vector of dB values
#' @param db numeric vector of decibel values
#' @return single dB value representing the energy-correct average
energetic_mean_db <- function(db) {
  db <- db[is.finite(db)]
  if (length(db) == 0) return(NA_real_)
  10 * log10(mean(10^(db / 10)))
}

#' Correct energetic sum in dB (e.g. reconstructing broadband from octave bands)
energetic_sum_db <- function(db) {
  db <- db[is.finite(db)]
  if (length(db) == 0) return(NA_real_)
  10 * log10(sum(10^(db / 10)))
}

# --- internal helpers -------------------------------------------------------

.pce_parse_header <- function(lines) {
  meta <- list()

  gi <- function(tag) {
    idx <- which(lines == tag)
    if (length(idx) == 0 || idx[1] >= length(lines)) return(NA_character_)
    lines[idx[1] + 1]
  }

  meta$version   <- gi("[Version]")
  sn_line        <- gi("[SN Inf]")
  meta$serial    <- sub("^SN:", "", sn_line)

  log_on         <- gi("[Logging On]")
  # e.g. "16-09-2026 09:12:15 DD-MM-YYYY"
  ts_str <- sub("\\s+DD-MM-YYYY.*$", "", log_on)
  meta$logging_start <- as.POSIXct(ts_str, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")

  meta$logger_step <- gi("[Logger Step]")

  setting_line <- gi("[Setting]")
  meta$setting_raw <- setting_line
  cal <- regmatches(setting_line, regexpr("Cal\\.Factor:[^\\s\\t]+", setting_line))
  meta$cal_factor <- if (length(cal) > 0) cal else NA_character_
  filt <- regmatches(setting_line, regexpr("(?<=Filter:)[A-Z]", setting_line, perl = TRUE))
  meta$filter <- if (length(filt) > 0) filt else NA_character_
  det <- regmatches(setting_line, regexpr("(?<=Detector:)[A-Z]", setting_line, perl = TRUE))
  meta$detector <- if (length(det) > 0) det else NA_character_

  meta
}

.pce_parse_datetime <- function(date_chr, time_chr) {
  # Date: DDMMYYYY (8 digits). Time: HHMMSS or HHMMSS.f
  d <- sprintf("%08s", date_chr)
  day   <- substr(d, 1, 2)
  month <- substr(d, 3, 4)
  year  <- substr(d, 5, 8)

  t <- trimws(time_chr)
  secs_frac <- sub("^[0-9]{6}", "", t)          # ".0" or ""
  hms <- substr(t, 1, 6)
  hh <- substr(hms, 1, 2); mm <- substr(hms, 3, 4); ss <- substr(hms, 5, 6)

  ts <- as.POSIXct(paste0(year, "-", month, "-", day, " ", hh, ":", mm, ":", ss),
                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  frac <- suppressWarnings(as.numeric(paste0("0", secs_frac)))
  frac[is.na(frac)] <- 0
  ts + frac
}

.pce_read_datablock <- function(lines) {
  data_idx <- which(lines == "[Data]")
  if (length(data_idx) == 0) stop("No [Data] section found.")
  header_line <- lines[data_idx + 1]
  col_names <- strsplit(header_line, "\t")[[1]]
  col_names <- trimws(col_names)
  col_names <- col_names[col_names != ""]

  data_lines <- lines[(data_idx + 2):length(lines)]
  data_lines <- data_lines[trimws(data_lines) != ""]

  rows <- lapply(data_lines, function(ln) {
    v <- strsplit(ln, "\t")[[1]]
    v <- trimws(v)
    length(v) <- length(col_names)  # pad/truncate defensively
    v
  })
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- make.names(col_names, unique = TRUE)
  df
}

.pce_clean_names <- function(nms) {
  nms <- gsub("\\s*-\\s*$", "", nms)     # trailing " -" e.g. "LEQ A -"
  nms <- gsub("[^A-Za-z0-9]+", "_", nms)
  nms <- gsub("_+$", "", nms)
  nms <- gsub("^_+", "", nms)
  nms
}

# --- public readers ----------------------------------------------------------

#' Read a PCE-430 octave-band (.OCT) file
read_pce430_oct <- function(path) {
  lines <- readLines(path, warn = FALSE)
  meta <- .pce_parse_header(lines)
  meta$file_type <- "OCT"
  df <- .pce_read_datablock(lines)
  names(df) <- .pce_clean_names(names(df))

  df$datetime <- .pce_parse_datetime(df$Date, df$Time)
  num_cols <- setdiff(names(df), c("Date", "Time", "datetime"))
  for (cc in num_cols) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  df$OVLD <- as.logical(df$OVLD)

  # tidy octave-band frequency labels, in Hz-ascending order
  band_cols <- grep("Hz$", names(df), value = TRUE)
  meta$octave_bands_hz <- band_cols

  list(meta = meta, data = df)
}

#' Read a PCE-430 continuous broadband (.SWN) file
read_pce430_swn <- function(path) {
  lines <- readLines(path, warn = FALSE)
  meta <- .pce_parse_header(lines)
  meta$file_type <- "SWN"
  df <- .pce_read_datablock(lines)
  names(df) <- .pce_clean_names(names(df))
  # Expected columns after cleaning: Date, Time, LEQ_A, PEAK_C, MAX_Z_F, OVLD

  df$datetime <- .pce_parse_datetime(df$Date, df$Time)
  num_cols <- setdiff(names(df), c("Date", "Time", "datetime"))
  for (cc in num_cols) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  df$OVLD <- as.logical(df$OVLD)

  list(meta = meta, data = df)
}

#' Read a PCE-430 statistics/summary (.CSD) file
read_pce430_csd <- function(path) {
  lines <- readLines(path, warn = FALSE)
  meta <- .pce_parse_header(lines)
  meta$file_type <- "CSD"
  df <- .pce_read_datablock(lines)
  names(df) <- .pce_clean_names(names(df))

  df$datetime <- .pce_parse_datetime(df$Date, df$Time)
  # LAe is a linear quantity (scientific notation) -- as.numeric handles this
  # fine, it just must never be treated as if it were on the dB scale.
  num_cols <- setdiff(names(df), c("Date", "Time", "datetime"))
  for (cc in num_cols) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  if ("OVLD" %in% names(df))  df$OVLD  <- as.logical(df$OVLD)
  if ("PAUSE" %in% names(df)) df$PAUSE <- as.logical(df$PAUSE)

  list(meta = meta, data = df)
}

#' Dispatch on file extension
read_pce430 <- function(path) {
  ext <- toupper(tools::file_ext(path))
  switch(ext,
    OCT = read_pce430_oct(path),
    SWN = read_pce430_swn(path),
    CSD = read_pce430_csd(path),
    stop("Unrecognised PCE-430 file extension: ", ext)
  )
}

# --- quick manual test when run directly ------------------------------------
if (sys.nframe() == 0) {
  for (f in list.files("data_samples", full.names = TRUE)) {
    cat("\n====", f, "====\n")
    res <- read_pce430(f)
    print(res$meta[c("file_type", "serial", "logging_start", "logger_step", "cal_factor")])
    str(res$data)
  }
}
