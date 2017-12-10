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
param <- yaml::yaml.load_file("../demo/param.yml")
lang <- "EN"

param_secondAxis_Label <- manage_axis_title(param[[lang]]$secondAxis_Label, "right")
param_labelY <- manage_axis_title(param[[lang]]$labelY, param$YPosition)

## ported from ../lib/ECO_ggplot_theme.r
eco_default <- yaml::yaml.load_file("../inst/extdata/eco_default.yml")

if (!Sys.info()[["sysname"]] == "Linux") {
  suppressMessages(extrafont::loadfonts(device = "win")) }

suppressMessages(extrafont::loadfonts(device = "pdf"))
suppressMessages(extrafont::loadfonts(device = "postscript"))


## ----data----------------------------------------------------------------
sheets <- c("A", "Q", "M", "D")
dat_empty <- data.frame(Date = "1900-01-01")

DATA <- lapply(sheets,
               function(x) read_eo_sheet(sheet = paste0("DATA_", x),
                                         file = "../data-raw/_test_EST_EO102.xlsx",
                                         dat_empty = dat_empty))
names(DATA) <- sheets

## Merge A/Q/M/D data in one data.frame
merge_cols   <- c("Date")
data_m <-
  dat_empty %>%
  full_join(DATA$A, by = merge_cols) %>%
  full_join(DATA$Q, by = merge_cols) %>%
  full_join(DATA$M, by = merge_cols) %>%
  full_join(DATA$D, by = merge_cols) %>%
  mutate(Date = as.Date(Date)) %>%
  arrange(Date) %>%
  .[-1, ]

## change shape from wide to long
data_ml <-
  data_m %>%
  mutate(esi_scaled = `Economic sentiment indicator` * 0.5 - 45) %>%
  gather(key = variable, value = value, -Date, na.rm = TRUE)

## legend labels for join with plot data
df_varlabel <- data.frame(variable = c("GDP growth", "esi_scaled"),
                          varlabel = c(paste0("<-- ", param[[lang]]$series_1_label),
                                       paste0(param[[lang]]$series_2_label, " -->")))


## ----chart---------------------------------------------------------------
ticksize <- (param$y_max - param$y_min) * 2.5 / 100

## minor_tick_seq <- seq(min(data_ml$Date), max(data_ml$Date), "3 month")
major_tick_seq <- seq(min(data_ml$Date), max(data_ml$Date) + lubridate::years(1), "1 year")

midyeardata <- as.Date(paste0(lubridate::year(data_ml$Date), "-07-01"),
                       format = "%Y-%m-%d")
label_tick_seq <- seq(min(midyeardata), max(midyeardata), "3 year")

if (interactive() == TRUE) {graphics.off()}

p <- data_ml %>%
  inner_join(df_varlabel) %>%
  ggplot(aes(x = Date, y = value, color = varlabel)) +
  geom_line(size = eco_default$line_width, linetype = "solid") +
  ## Set series colors
  scale_color_manual(values = eco_default$color_values[3:4])

my_chart2 <-
  eco_format_ggplot(p = p,
                    y2formula = "~ . * 2 + 90",
                    y2lab = param_secondAxis_Label) +
  annotate(geom = "segment",
           y = param$y_min,
           yend = param$y_min + ticksize,
           x = major_tick_seq,
           xend = major_tick_seq,
           size = 0.1)


## ----plot, dev.args=list(bg='transparent')-------------------------------
print(my_chart2)


## ----export, eval=FALSE--------------------------------------------------
## ## Replace my_chart1 by the name of your first chart
## gA <- ggplotGrob(my_chart2)
## ## Replace my_chart2 by the name of your second chart
## gB <- ggplotGrob(my_chart2)
## g = cbind(gA, gB, size = "last")
## 
## devEMF::emf("graphs.emf", width = 16.4/2.54, height = 6.6/2.54)
## 
## grid.arrange(g, top = textGrob(" Estonia ", x = 0.5, hjust = 1, gp = gpar(fontface = "bold", fontsize = 15)), bottom = textGrob("1. my notes", x = 0, hjust = 0, gp = gpar(fontsize = 9)))
## #grid.arrange(g)
## dev.off()
## 

