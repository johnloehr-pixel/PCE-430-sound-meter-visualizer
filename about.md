PCE-430 Sound Level Explorer
=============================

What this app does
-------------------
It reads the three export formats produced by the PCE-430 sound level meter and
lets you explore them per site, or compare several sites side by side.

  .OCT  Octave-band log (1 row/second): SPL_AF/BF/CF/ZF broadband levels
        plus 12 octave bands from 8 Hz to 16 kHz.
  .SWN  Continuous broadband log (1 row/second): LEQ(A), PEAK(C), MAX(Z,Fast).
        Well suited to capturing discrete loud events.
  .CSD  Session summary statistics: LAeq, L10/L50/L90, LAFmax/min, LAFsd,
        LAsel/LAe, LCpeak -- one row per measurement session.

A "site" in this app is just a name you attach to a set of files. You can add
any subset of the three file types per site -- the app only shows what you
gave it.

A note on decibels (please read before drawing conclusions)
-------------------------------------------------------------
dB is a logarithmic ratio, not a linear unit. That means:

  - You cannot average dB values arithmetically and get the correct answer.
    Doing so is always biased low, and more so the more the values vary.
  - LAeq is specifically defined to get around this: it's an energy
    (linear-domain) average, converted back to dB. This app follows the same
    approach (see `energetic_mean_db()` in pce430_import.R) any time it needs
    to summarise dB values you haven't already been given a proper LAeq/L90
    for -- for example, the "Spectral profile comparison" plot averages each
    octave band correctly, not by naively averaging the dB column.
  - Percentiles (L10/L50/L90) do NOT have this problem -- order statistics
    are unaffected by the log transform, so it's fine to compare them
    directly across sites or time windows.
  - LAe is the odd one out: it's already a LINEAR quantity (e.g. Pa^2*h),
    shown in scientific notation, sitting right next to its dB twin LAsel.
    Never treat LAe as if it were on the same scale as the other columns.

A statistical note for the course: independence of observations
-------------------------------------------------------------------
Each second within one session is not an independent replicate -- it's one
continuous, highly autocorrelated recording (a loud second is usually
followed by another loud second). If you want to compare sites formally
(e.g. with a GLMM), the standard approach is to treat each *site-visit /
session* as one replicate, using its summary statistics (LAeq, L10/L50/L90,
LAFmax, LAFsd -- exactly what the .CSD file already reports) as your response
variables, rather than feeding every 1-second row in as if it were an
independent data point. If you do want to keep the full time-series
resolution for a more detailed model, a mixed model with an explicit
autocorrelation structure (e.g. `nlme::lme` or `mgcv::gamm`/`bam` with an
AR(1) correlation on the residuals) is the more defensible route.

Files in this project
----------------------
  app.R              the Shiny app (UI + server)
  pce430_import.R    file parsing + the energetic-mean helper functions
  data_samples/      three example files (one of each type) for the
                     "Load bundled example" button, so the app has something
                     to show immediately
  about.md           this page
