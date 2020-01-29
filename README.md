# Viz-WEPPCloud

This repo contains an experimental R shiny app that can visualize WEPPCloud outputs. 

At present it preprocesses the Hillslope file and is able to plot cumulative selected "variable" 
for a selected "watershed" and selected "simulation scenario" 

## Getting Started

These instructions will get you a copy of the "Viz-WEPPCloud" up and running on your local machine.

## How to Install

Currently this app is only hosted via Github. Make sure you have Rstudio installed and then install 
libraries needed to use this tool by running following command in your Rstudio console: 

```{r}
install.packages("shiny", "tidyverse)
```

Now you start visualizing the data using Viz-WEPPCloud package by running following command:

```{r}
shiny::runGitHub("Viz-WEPPCloud", "devalc")
```
