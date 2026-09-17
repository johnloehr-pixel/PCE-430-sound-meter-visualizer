# PCE-430 Sound Level Explorer

A Shiny app for exploring and comparing field recordings from the PCE-430 sound level meter. Built for teaching environmental acoustics: every plot carries a short collapsible "How to read this" note, and all averaging of decibel values is done in the linear (energy) domain, the same way the meter itself computes LAeq.

## What it does

The app reads the three export formats the PCE-430 produces:

| Format | Content |
|---|---|
| `.OCT` | Per-second octave-band log: SPL in A/B/C/Z weightings plus 12 octave bands from 8 Hz to 16 kHz |
| `.SWN` | Per-second broadband log: LEQ(A), PEAK(C), MAX(Z,Fast) |
| `.CSD` | Session summary statistics: LAeq, L10/L50/L90, LAFmax/min, LAFsd, LAsel/LAe, LCpeak |

You group files into named sites through the sidebar. A site can have any subset of the three formats, and the app shows only what it has.

**Single site tab** — octave-band spectrogram over time, broadband weighted levels (A/B/C/Z, selectable), the continuous LEQ/PEAK/MAX trace, and the session summary table with a plain-language explanation of each metric.

**Compare sites tab** — for any set of loaded sites: overall level as a bar chart (LAeq, L90, or LAFmax), boxplots of the per-second level distributions, mean octave-band spectra as a per-site frequency "fingerprint", and a summary table.

## Setup and run

```r
install.packages(c("shiny", "bslib", "plotly", "DT"))
shiny::runApp("app.R")
```

Or open the folder in RStudio and click "Run App". The **Load bundled example** button in the sidebar loads the sample files from `data_samples/` so the app has something to show immediately.

## Two things the app gets right that are easy to get wrong

**Decibel averaging.** dB is a logarithmic ratio, so averaging dB values arithmetically is always biased low, and more so for variable recordings. Wherever the app needs to summarise a level column (the spectral comparison, LAeq computed from a per-second log), it converts to linear energy, averages, and converts back (`energetic_mean_db()` in `pce430_import.R`). Percentiles such as L10/L50/L90 are order statistics and are safe to compare directly.

**Meter mode changes what a `.CSD` contains.** In SLM mode the `.CSD` export is the session summary table. In 1/1OCT mode it is instead a per-minute octave-band log of the same session the `.OCT` records per second. The app checks the columns actually present rather than trusting the extension, and falls back to computing LAeq/L90/LAFmax from the per-second data when no true summary is available.

## A statistical caution for site comparisons

Seconds within one recording are not independent replicates. A loud second is usually followed by another loud second. For a formal comparison between sites (for example with a GLMM), treat each session or site visit as one replicate, using its summary statistics as response variables, rather than feeding in every 1-second row. The in-app notes and `about.md` expand on this.

## Files

- `app.R` — the Shiny app (UI and server)
- `pce430_import.R` — parsers for the three export formats plus the energetic-mean helpers
- `data_samples/` — example PCE-430 exports, including a four-site set under `ECGS-024/`
- `about.md` — the app's "About / methods" tab
