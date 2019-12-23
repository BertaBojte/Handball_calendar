library(shiny)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(rvest)
library(data.table)
library(stringr)
library(lubridate)
library(googlecalendar)
library(dplyr)
library(googlesheets)
library(shinycssloaders)

# source("Functions.R")

#### General functions ####
firstup <- function(x) {
  s <- strsplit(x, " ")[[1]]
  t <- paste(toupper(substring(s, 1,1)), substring(s, 2),
             sep = "", collapse = " ")
  return(t)
}

#### Google Sheet ####

### Authentication for GoogleSheet ### ----
auth <- function() {
  # authentication
  gs_ls()
  # read workbook
  meccsek <-  gs_title("NAME_OF_THE_WORKBOOK")
  return(meccsek)
}


### Data download from a google sheet that contains match history ### ----
AdatLetoltes <- function() {
  match_h <- na.omit(data.table(
    gs_read(
      literal = F ,
      ss = meccsek,
      ws = "NAME_OF_THE_WORKSHEET",
      range = cell_cols(1:5),
      col_names = T,
      col_types = cols(
        channel = col_character(),
        match = col_character(),
        strt = col_character(),
        end = col_character(),
        color = col_character()
      )
    )
  ))
  return(match_h)
}

### Clean the date from the match history ###
date_cleaning <- function(match_h_dt){
  
  for (i in 1:length(match_h_dt$strt)) {
    
    match_h_dt$strt_dt[i] = as.character(
                              ymd_hms(paste(strsplit(
                                match_h_dt$strt[i], "T")[1],
                                gsub('Z', '', strsplit(match_h_dt$strt[i], "T")[2]))) + 7200)
    
    match_h_dt$end_dt[i] = as.character(
                            ymd_hms(paste(strsplit(
                              match_h_dt$end[i], "T")[1],
                              gsub('Z', '', strsplit(match_h_dt$end[i], "T")[2]))) + 7200)
  }
    return(match_h_dt)
}

### Check the next match withing 3 days, and save it to show on the website
next_match_create <- function(match_h_dt){
  
  # create text for the next match textbox
  next_match_dt <- match_h_dt[strt_dt >= now()+7200][1]
  next_match <- ""
  if (is.na(next_match_dt$channel[1])) {
    next_match <- "A közeljövőben nem lesz meccs :("
  } else {
    next_match <-
      paste(
        next_match_dt$strt_dt,
        "<br>",
        gsub("ÉLŐ Kézilabda", "", next_match_dt$match), 
        "<br>", 
        firstup(tolower(next_match_dt$channel))
      )
  }
  return(next_match)
}

### Scraping the data of the provided website ### ----
## Create a data.table from the provided URL,
## containing the text for the programm and the time of it
scrape_data <- function(m_url)
{
  # read the HTML code into a variable
  my_html <- read_html(m_url)
  
  # search for the nodes you need and add it to a data.table
  dt_text <-
    data.table(html_nodes(x = my_html, css = 'SOME_CSS_PROPERTY')
               %>% html_text())
  dt_time <-
    data.table(html_nodes(x = my_html, css = 'SOME_CSS_PROPERTY')
               %>% html_attr(name = 'content'))
  
  # create one data.table from the text and time and rename the columns
  dt <- cbind(dt_text, dt_time)
  colnames(dt) <- c('text_o', 'time_o')
  return(dt)
}

### Clean the text - manage Hungarian special characters ###
txt_clean <- function(txt){

  txt = stringi::stri_unescape_unicode(txt)
  txt <- gsub(pattern = " tÃ", replacement = "", x = txt, fixed = TRUE)

  txt <- trimws(gsub(": ", "", 
                     gsub('[[:digit:]]', "",
                          gsub('[\n\t]', '',enc2utf8(txt),fixed = TRUE))), 
                "both")
  txt1 <- enc2native(txt)
  
  txt1 <- gsub(pattern = "Ã\u0089LÅ\u0090",replacement =  "ÉLŐ ",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ãº",replacement =  "ú",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ã©",replacement =  "é",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Å\u0091",replacement =  "ő",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ã¡",replacement =  "á",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ã³",replacement =  "ó",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ã-",replacement =  "í",x =  txt1, fixed = TRUE)
  
  txt1 <- gsub(pattern = "A<U+0089>LA",replacement =  "ÉLŐ ",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "Ao",replacement =  "ú",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "A©",replacement =  "é",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "A<U+0091>",replacement =  "ő",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "A!",replacement =  "á",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "A3",replacement =  "ó",x =  txt1, fixed = TRUE)
  txt1 <- gsub(pattern = "A¶",replacement =  "ö",x =  txt1, fixed = TRUE)

    return(txt1)
}

### Data Cleaning  ### ----
## cleanes the scraped data by
## - cleaning up the text from formatting, extra spaces, numbers, etc
## - creating the needed date format for the calendar
## - flagging the records that contain handball ("Kézilabda")
## - adding the channel info
clean_data <- function(dt, channel)
{
  for (i in 1:length(dt$text_o))
  {
    dt$text_cl1[i] <- txt_clean(dt$text_o[i])
    
    dt$text_cl[i] <- trimws(gsub(": ", "", gsub(
      '[[:digit:]]', "",
      gsub('[\n\t]', '',
           dt$text_cl1[i])
    )), "both")
    
    dt$time[i] <- as.character(as_datetime(dt$time_o[i]))
    
    dt$time_e[i] <- as.character(as_datetime(dt$time_o[i]) + 7200)
    
    dt$cont_Kezi[i] <- grepl("Kézilabda", dt$text_cl[i])
    
    dt$date_strt[i] <- strsplit(dt$time[i], " ")[[1]][1]
    
    dt$time_strt[i] <-
      coalesce(strsplit(dt$time[i], " ")[[1]][2], "00:00:00")
    
    dt$strt[i] <-
      str_remove_all(paste(dt$date_strt[i], "T", dt$time_strt[i], "Z"), " ")
    
    dt$date_end[i] <- strsplit(dt$time_e[i], " ")[[1]][1]
    
    dt$time_end[i] <-
      coalesce(strsplit(dt$time_e[i], " ")[[1]][2], "00:00:00")
    
    dt$end[i] <-
      str_remove_all(paste(dt$date_end[i], "T", dt$time_end[i], "Z"), " ")
    
    dt$match[i] <- trimws(gsub("", "", dt$text_cl[i]), "both")
    
    dt$cont_meccs[i] <- grepl(" - ", dt$match[i])
    
    dt$cont_studio[i] <- grepl("Stúdió", dt$match[i])
    
    dt$cont_elo[i] <- grepl("ÉLŐ", dt$match[i])
    
    dt$cont_vb[i] <- grepl("gbajnoks", dt$match[i])
    
    dt$channel[i] <- channel$channel
    
    dt$color[i] <- channel$color
  }
  return(unique(dt))
}

### Data Filtering ### ----
## filtering the cleaned data to contain only those records that are handball matches
# - dt: datatable containing the matches
# - col_filt: list of columns to use as filters
# - col_keep: list of columns to keep in the end file
# - col_order: column name to use for ordering the table
filter_data <- function(dt)
{
  dt_1 <- data.table(dt)
  dt_1 <- unique(dt_1[cont_Kezi == TRUE]) 
  
  dt_2 <- data.table(dt_1)
  
  dt_2 <- dt_2[cont_meccs == TRUE]
  
  dt_2_1 <- dt_2[cont_vb == TRUE]
  
  dt_2 <- dt_2[cont_elo == TRUE]
  dt_2 <- dt_2[cont_studio == FALSE]
  
  dt_2_2 <- rbind(dt_2_1, dt_2)
  
  dt_2_2 <- dt_2_2[hms(time_strt) >= hms('06:00:00')
               & hms(time_strt) <= hms('23:00:00')]  # time_strt
  
  dt_3 <- unique(dt_2_2[, .(channel, match, strt, end, color)])   # channel, match, strt, end, color
  return(dt_3[order(strt)])
}

#### Google Calendar ####

### Authentication for Google Calendar ### ----
## Authentication for the calendar owner, and then selecting the calendar to use by name
load_calendar <- function(g_key, g_secret, cal_id)
{
  options(googlecalendar.client_key = g_key,
          googlecalendar.client_secret = g_secret)
  gc_auth(
    new_user = FALSE,
    key = getOption("googlecalendar.client_key"),
    secret = getOption("googlecalendar.client_secret"),
    cache = T
  )
  cal <- gc_id(cal_id)
  print(enc2utf8("naptár kész"))
  return(cal)
}


# adding the events from the cleaned, filtered dataframe to the selected calendar
add_to_cal <- function(dt_inp, cal)
{
  print("add to cal start")
    for (i in 1:length(dt_inp$match))
    {
      gc_event_new(
        x = cal,
        start = list(dateTime = dt_inp$strt[i]),
        end = list(dateTime = dt_inp$end[i]),
        summary = dt_inp$match[i],
        location = dt_inp$channel[i],
        colorId = dt_inp$color[i]
      )
    }
}

## Update the calendar with the new matches
run_update <- function(
  m_url,
  key,
  secret,
  cal_id,
  chan_inf,
  channel_information,
  txt_fin
){
  
  dt_raw <- scrape_data(m_url)
  dt_clean <- clean_data(dt_raw, chan_inf)
  dt_handb <- filter_data(dt_clean)

    cal <- load_calendar(key, secret, cal_id)
  
  if (length(dt_handb$match) > 0) {
    add_to_cal(dt_handb, cal)
    gs_add_row(ss = meccsek, ws = "matches", input = dt_handb, verbose = T)
  }
  else {
    txt_fin = "Az alábbi csatornán <br> nem lesz meccs <br> a következő 5 napban: <br>"
  }
    return(paste(txt_fin, channel_information))
  }




#### SERVER CODE #### 




# getting the data from GoogleSheets
meccsek <- auth()
match_history <- AdatLetoltes()

match_history <- date_cleaning(match_history)

# create a list of channels - these broadcast handball matches - and add color-coding for the calendar
chan_inf <- data.table(channel = c("SPORT1", "M4_SPORT", "SPORT2", "DUNA", 
                                   "DUNAWORLD", "SPILER_TV", "SPILER2_TV"), 
                       color_cde = c(5,4,6,2,3,1,7))


#### Shiny Server ####
shinyServer(function(input, output) {
  
  # Output the next match
  output$nextMatch <- renderText(next_match_create(match_history))
  
  # Update the selected channel by the selected date
  output$RUNTIME_TEXT <- eventReactive(input$btn_runUpdate, 
                                run_update(
                                  "THE_URL_FOR_THE_MATCHES",
                                  "GOOGLE_CALENDAR_KEY",
                                  "GOOGLE_CALENDAR_SECRET",
                                  "CALENDAR_ID",
                                  chan_inf[channel == input$inp_chan],
                                  chan_inf[channel == input$inp_chan]$channel,
                                  "Frissítés kész:" )
                                )
  
  # Output the URL (includes selected channel and date) used for update - for checking purposes
  output$URL_FOR_CHECK <- renderText("THE_CREATED_URL_FOR_THE_UPDATE")

})

