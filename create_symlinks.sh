#!/bin/bash

# find /Users/joey/neurotyp/NeurotypR/inst/rmarkdown/templates/neurotypr/skeleton -name '*.Rmd' -exec sh -c '
#   for f do
#     ln -s -- "$f" /Users/joey/neurotyp/NeurotypR/Rmd/"$(basename "$f")"
#   done' find-sh {} \;

find /Users/joey/neurotyp/NeurotypR/inst/rmarkdown/templates/neurotypr/skeleton/quarto -name '*.qmd' -exec sh -c '
  for f do
    ln -s -- "$f" /Users/joey/neurotyp/NeurotypR/Qmd/"$(basename "$f")"
  done' find-sh {} \;

# find /Users/joey/neurotyp/NeurotypR/inst/rmarkdown/templates/neurotypr/skeleton -name '*.R' -exec sh -c '
#   for f do
#     ln -s -- "$f" /Users/joey/neurotyp/NeurotypR/Rmd/"$(basename "$f")"
#   done' find-sh {} \;
