#!/bin/bash

find /Users/joey/pfc/NeurotypR/inst/rmarkdown/templates/neurotypr/skeleton -name '*.Rmd' -exec sh -c '
  for f do
    ln -s -- "$f" /Users/joey/pfc/NeurotypR/Rmd/"$(basename "$f")"
  done' find-sh {} \;