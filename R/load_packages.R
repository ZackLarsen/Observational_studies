##################################################
## Project: Observational Studies
## Script purpose: Load packages
## Date: January 16, 2021
## Author: Zack Larsen
##################################################


if (!require('pacman', character.only = TRUE)) {
  install.packages('pacman', dependencies = TRUE)
  library('pacman', character.only = TRUE)
}


p_load(
  # Package namespace collisions:
  conflicted,
  
  # Snippets:
  datapasta,
  
  # Matching
  MatchIt, cobalt, optmatch, rbounds, #genmatch,
  cem, Matching, Zelig, twang, quickmatch,
  stddiff,
  glm2, speedglm, pscl,
  mitools, margins, survey, survival,
  lmerTest,
  
  # Convenience
  docstring, broom, scales, lobstr, janitor, glue,
  sloop, vctrs, pryr, forcats,
  progress,
  tidyverse, magrittr, here,
  Hmisc,
  
  # EDA
  skimr, naniar, UpSetR, DataExplorer, funModeling, moderndive,
  
  # Data manipulation
  data.table, tidyr, widyr,
  
  # Data io
  haven, vroom, jsonlite, fst,
  
  # Timing
  tictoc, proffer,
  
  # Dataviz
  cowplot, ggthemes, esquisse,
  ggthemes, ggforce, ggpubr, ggsci, ggfortify, 
  plotly, rbokeh,
  treemapify,
  #gifski, transformr,
  #gganimate, # Careful - gganimate messes up the View() function
  patchwork, see, vioplot, ggridges, sm,
  extrafont,
  hrbrthemes,
  
  MASS, # for rlm
  boot, doParallel, mgcv, # for gam
  
  dtplyr,
  fs,
  
  # Parallelization
  furrr,
  
  # Pipeline
  #targets,
  
  AER,
  
  # Synthesizing and fitting data
  random, fitdistrplus, wakefield,
  #, synthpop,
  
  # Reports
  gt, tufte, tableone, kableExtra, captioner
  )


conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
#conflict_prefer("View", "utils")


# Show which packages have been loaded:
search()


sessionInfo()

