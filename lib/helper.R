
read_eo_sheet <- function(sheet = stop("'sheet' must be provided"),
                          file = stop("'file' must be provided"),
                          dat_empty = data.frame(NULL)) {
  dat_orig <- tryCatch(as.data.frame(read_excel(path = file, sheet = sheet)),
                  error = function(err) { data.frame(NULL) })
  dat_clean <- clean_df(dat_orig)
  dat_out <- if(nrow(dat_clean) == 0) dat_empty else dat_clean
  return(dat_out)
}


## Remove empty columns and empty values (it can happen depending on the excel contents)
## data <- rbind.data.frame(
##   c("<NA>", "<NA>", "<NA>"),
##   DATA$M)
## data <- cbind(data,
##               rep("<NA>", nrow(data)),
##               rep("<NA>", nrow(data)))
## names(data) <- c(names(data)[1:3], "", NA)
## head(data)
clean_df <- function(data = stop("'data' must be provided")) {
  if ("Date" %in% colnames(data) & class(data$Date[1])=="character") {
    data_filter <- data %>% subset(Date != "<NA>")
  } else {
    data_filter <- data
  }

  data_out <-
    data_filter %>%
    .[,colnames(.)[colnames(.)!= "" & is.na(colnames(.)) == FALSE]]

  return(data_out)
}


eco_format_ggplot <- function(p, y2formula="~.") {
  p_out <- p +
    expand_limits(y = c(param$y_min, param$y_max)) +
    ##
    ## Reference Line (horizontal)
    ## You can change the color and linetype like this
    ## geom_hline(yintercept = c(10,15), color = "red", linetype = "dashed")
    geom_hline(yintercept = c(0), color = "black") +
    ##
    ## Set X ticks
    scale_x_date(breaks = label_tick_seq,
                 date_labels = "%Y",
                 expand = c(0,0)) +
    ##
    ## Set Y ticks
    scale_y_continuous(position = param$YPosition,
                       breaks = seq(from = param$y_min,
                                    to = param$y_max,
                                    by = param$y_break),
                       ## Second Y axis
                       sec.axis = sec_axis(trans = as.formula(y2formula),
                                           name = param_secondAxis_Label,
                                           breaks = seq(param$secondAxis_Min,
                                                        param$secondAxis_Max,
                                                        param$secondAxis_Break)),
                       expand = c(0, 0)) +
    ##
    ## Add title, subtiles, x/y labels
    labs(x = "",
         y = param_labelY,
         title = param[[lang]]$title,
         subtitle = param[[lang]]$subtitle) +
    ## The 0.001 fix a bug on double axis display (for "DEU Labour market")
    coord_cartesian(ylim = c(param$y_min - 0.001 * abs(param$y_min),
                             param$y_max))  +
    ## Apply ECO theme
    theme_ECO(legendposition = c(param[[lang]]$legendX,
                                 param[[lang]]$legendY),
              legenddirection = param[[lang]]$legendDir,
              panelontop = param$panel_ontop,
              rotateXLabel = "NO",
              ticksOnX = "NO")+
    annotate(geom = "segment", y = param$y_min, yend = param$y_min + ticksize,
             x = major_tick_seq, xend = major_tick_seq, size = 0.1)

  return(p_out)
}


Eco_Merge_AQMD_DataFrames <- function(chart_type, xls_data_a, xls_data_q, xls_data_m, xls_data_d, prz_data_a, prz_data_q, prz_data_m) {

  if (chart_type =='Time series') {
    # Merge A/Q/M/D data in one data.frame ')
    merge_cols   <- c("Date")
    data         <- data.frame(Date="1900-01-01") #  (This value will be removed after)
    if (exists('xls_data_a')) {    if (is.na(colnames(xls_data_a)[1])==FALSE & colnames(xls_data_a)[1] != ""){data <- merge(data, xls_data_a, by=merge_cols, all.x=TRUE, all.y=TRUE)} }
    if (exists('xls_data_q')) {    if (is.na(colnames(xls_data_q)[1])==FALSE & colnames(xls_data_q)[1] != ""){data <- merge(data, xls_data_q, by=merge_cols, all.x=TRUE, all.y=TRUE)}}
    if (exists('xls_data_m')) {    if (is.na(colnames(xls_data_m)[1])==FALSE & colnames(xls_data_m)[1] != ""){data <- merge(data, xls_data_m, by=merge_cols, all.x=TRUE, all.y=TRUE)}}
    if (exists('xls_data_d')) {    if (is.na(colnames(xls_data_d)[1])==FALSE & colnames(xls_data_d)[1] != ""){data <- merge(data, xls_data_d, by=merge_cols, all.x=TRUE, all.y=TRUE)}}

    if (exists('prz_data_a')) {    if (is.na(colnames(prz_data_a)[1])==FALSE & colnames(prz_data_a)[1] != ""){data <- merge(data, prz_data_a, by=merge_cols, all.x=TRUE, all.y=TRUE)} }
    if (exists('prz_data_q')) {    if (is.na(colnames(prz_data_q)[1])==FALSE & colnames(prz_data_q)[1] != ""){data <- merge(data, prz_data_q, by=merge_cols, all.x=TRUE, all.y=TRUE)} }
    if (exists('prz_data_m')) {    if (is.na(colnames(prz_data_m)[1])==FALSE & colnames(prz_data_m)[1] != ""){data <- merge(data, prz_data_m, by=merge_cols, all.x=TRUE, all.y=TRUE)} }

    #data$Date    <- as.Date(data$Date)
    cols<-colnames(data)
    data         <- data[-1,] # Remove the first row (1900-01-01)
    data         <- data.frame(lapply(data,function(x) {gsub('nd',NA,x)})) # Replace 'nd' by NA
    data         <- data.frame(lapply(data,function(x) {gsub('ND',NA,x)})) # Replace 'ND' by NA
  }
  if (chart_type =='Category') {
    # Merge A/Q/M/D data in one data.frame
    merge_cols   <- c("X")
    data         <- data.frame(X="_") #  (This value will be removed after)
    if (exists('xls_data_a')) {if (is.na(colnames(xls_data_a)[1])==FALSE & colnames(xls_data_a)[1] != ""){data <- merge(data, xls_data_a, by=merge_cols, all.x=TRUE, all.y=TRUE)}}
    if (exists('xls_data_q')) {if (is.na(colnames(xls_data_q)[1])==FALSE & colnames(xls_data_q)[1] != ""){data <- merge(data, xls_data_q, by=merge_cols, all.x=TRUE, all.y=TRUE)}}
    if (exists('xls_data_m')) {if (is.na(colnames(xls_data_m)[1])==FALSE & colnames(xls_data_m)[1] != ""){data <- merge(data, xls_data_m, by=merge_cols, all.x=TRUE, all.y=TRUE)}}
    if (exists('xls_data_d')) {if (is.na(colnames(xls_data_d)[1])==FALSE & colnames(xls_data_d)[1] != ""){data <- merge(data, xls_data_d, by=merge_cols, all.x=TRUE, all.y=TRUE)}}

    if (exists('prz_data_a')) {if (is.na(colnames(prz_data_a)[1])==FALSE & colnames(prz_data_a)[1] != ""){      data <- merge(data, prz_data_a, by=merge_cols, all.x=TRUE, all.y=TRUE)}      }
    if (exists('prz_data_q')) {if (is.na(colnames(prz_data_q)[1])==FALSE & colnames(prz_data_q)[1] != ""){      data <- merge(data, prz_data_q, by=merge_cols, all.x=TRUE, all.y=TRUE)}      }
    if (exists('prz_data_m')) {if (is.na(colnames(prz_data_m)[1])==FALSE & colnames(prz_data_m)[1] != ""){      data <- merge(data, prz_data_m, by=merge_cols, all.x=TRUE, all.y=TRUE)}      }
    cols<-colnames(data)

    data         <- data[-1,] # Remove the first row (1900-01-01)
    data         <- data.frame(lapply(data,function(x) {gsub('nd',NA,x)})) # Replace 'nd' by NA
    data         <- data.frame(lapply(data,function(x) {gsub('ND',NA,x)})) # Replace 'ND' by NA
  }

  # Convert Factor columns to numeric
  co <- as.list(colnames(data)[!colnames(data) %in% as.character(colnames(data)[1])])
  for (i in 1:length(co)) {
    data[as.character(co[i])]<-lapply(data[as.character(co[i])], function(x) as.numeric(as.character(x)))
  }

   if (chart_type =='Time series') {
     data$Date <- as.Date(data$Date)
     data      <- data[order(data$Date),] # Order by Date
   }
  colnames(data)<-cols
  return(data)
}
