## ----knitr, echo=FALSE---------------------------------------------------
library("knitr")
knitr::opts_chunk$set(
    echo = FALSE,
    message = FALSE,
    warning = FALSE,
    fig.align = 'center',
    fig.pos = 'H',
    fig.path = "plots/",
    ## dev = c("png", "svg"),
    dev = "svg"
   ## ,
    ## dpi = 500
)

## ----init----------------------------------------------------------------
## source("../lib/EcoChartGenerator_libraries.R")

library(ggthemes)
library(ggplot2)
library(readxl)
library(zoo)
library(stats)
library(plyr)
library(dplyr)
library(tidyr)
library(xts)
library(reshape2)
library(rvg)
library(stringr)
library(extrafont)
library(lubridate)

library(grid)
library(gridExtra)
library(devEMF)

## - StopProgramIfLibraryIsMissing
## - theme_ECO
## - manage_axis
source("../lib/ECO_ggplot_theme.r")

## - read_eo_sheet
## - clean_df
## - eco_format_ggplot
source("../lib/helper.R")


## ----param---------------------------------------------------------------
param <- yaml::yaml.load_file("../demo-2/param.yml")
lang <- "EN"

param_labelY <- manage_axis_title(param$labelY, param$YPosition)

## ported from ../lib/ECO_ggplot_theme.r
eco_default <- yaml::yaml.load_file("../inst/extdata/eco_default.yml")


## ----data----------------------------------------------------------------
sheets <- c("A", "Q", "M", "D")

DATA <- lapply(sheets,
               function(x) read_eo_sheet(sheet = paste0("DATA_", x),
                                         file = "../data-raw/_TEST_CHN_DEF.xlsx"
                                         ))
names(DATA) <- sheets

prz_data_a  <- prz_data_q <- prz_data_m <- data.frame(NULL)


## Merge A/Q/M/D data in one data.frame
data <- Eco_Merge_AQMD_DataFrames(
  param$chart_type,
  DATA$A,
  DATA$Q,
  DATA$M,
  DATA$D,
  prz_data_a,
  prz_data_q,
  prz_data_m
)


## ----chart---------------------------------------------------------------
## Define xTicks labels, minor/major ticks
xTicks <- Eco_GetXTicks(param_y_min = param$y_min,
                        param_y_max = param$y_max,
                        dates = data$Date,
                        year = param$x_break)

## manual addition
xTicks$XTICKS_LABELS[5] <- "2017-01-01"


## Bar/Stacked Bar preparation
stackedbar_order <- tribble(
  ~variable, ~type,
  "Goods trade", "bar",
  "Service trade", "bar",
  "Primary income", "bar",
  "Secondary income", "bar",
  "Current balance", "line")

stackedbar_data <-
  data %>%
  gather(key = variable, value = value, -Date) %>%
  filter(!is.na(variable)) %>%
  left_join(stackedbar_order, by = "variable")

stackedbar_data$variable <- factor(stackedbar_data$variable, levels = stackedbar_order$variable)

if (interactive()==TRUE) { graphics.off() }

my_chart <-
  ggplot(data = stackedbar_data) +
  ## Series "Goods trade","Service trade","Primary income","Secondary income" (Stacked-bar)
  geom_bar(
    data = function(x) { x[x$type %in% c("bar"), ] },
    mapping = aes(x = Date,
                  y = value,
                  fill = variable,
                  group = rev(variable)),
    stat = "identity",
    position = "stack",
    color = "black",
    size = 0.3
  ) +
  ## Series 5: Current balance (Color=black, Geom=line, Line type=solid)
  geom_line(
    data = function(x) { x[x$type %in% c("line"), ] },
    aes(x = Date, y = value, color = factor(1)),
    size = eco_default$line_width,
    linetype = "solid"
  )  +
  ## Set series colors
  scale_color_manual(labels = c(param[[lang]]$series_labels[5]),
                     values = c(eco_default$color_values[11])) +
  scale_fill_manual(labels = c(param[[lang]]$series_labels[1:4]),
                    values = c(eco_default$color_values[c(1, 4, 3, 8)]))

my_chart2 <-
  eco_format_ggplot(p = my_chart,
                    breaks = xTicks$XTICKS_LABELS,
                    ticksOnX = "YES") +
  xTicks$XTICKS_MINOR() +
  xTicks$XTICKS_MAJOR()


## ----plot, dev.args=list(bg='transparent')-------------------------------
print(my_chart2)


