###  Covid 19 data from John Hopkins data set
library(dplyr)
library(plotly)
library (RCurl)

# rm(list = ls())

###   colors
cl <- colors(TRUE)

# cl[136:234] are shades of gray
# seq(136,234)[!seq(136,234) %in% seq(136, 234 , by = 20)]
cl <- cl[c(2:135, seq(136, 234 , by = 4), 235:502)]
##  length(cl)  # 427
## seq(1,427, by =5)  # 86 elements
cl <- cl[seq(1,427, by =5)]

cl <- cl[c(2,7,8,9,10,12,14,15,18,19,20,22,23,24,25,26,29,32,33,35,36,38)]
cl <- c("antiquewhite4", "deepskyblue4", "brown4", "gold",
        "gray24", "indianred2", "darkseagreen", "cadetblue4",
        "firebrick4", "burlywood4",   "darkorchid2",
  "hotpink2", "chocolate", "cornflowerblue", "darkgoldenrod1",
  "darkgreen", "deeppink3", "darkslateblue",
  "dodgerblue4", "gray85", "green2", "khaki3")
##### some constants for plotly plots
vals <- schema(F)$traces$scatter$attributes$marker$symbol$values
vals <- grep("-", vals, value = T)
vals_open <- grep("open", vals, value = T) #, invert = T)
vals_open_dot <- grep("dot", vals_open,
                      value = T) #, invert = T)

# xax_label <- list(  title = "Date")
lty_jc <- c(1,2,1)
col_jc <- c("lightblue", "darkgreen", "red")
lty_ploty <- list(list(),
                  list(dash = 'dot'),
                  list())

######### creation of list of certain countries with population
pays_jc <- c("Algeria", "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Canada", "China", "Denmark", "Egypt", "Finland", "France", "Germany", "Greece", "Iceland", "India", "Indonesia", "Ireland", "Israel", "Italy", "Japan", "Korea, South", "Mexico", "Morocco", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", "Russia", "South Africa", "Spain", "Sweden", "Taiwan*", "Thailand", "Tunisia", "Turkey", "United Kingdom", "US")
getpop_jc <- c(43851044, 45195774, 25499884, 8993780, 11589623, 212559417, 37742154, 1408526449, 5792202, 102334404, 5538627, 65238332, 83042200, 10435529, 341243, 1391749135, 272805349, 4937786, 8621523, 60480000, 126476461, 51258472, 128932753, 36801592, 17134872, 4822233, 5410781, 37846611, 10196709, 145934462, 59308690, 46750386, 10083783, 23816775, 69756812, 11818619, 84339067, 67886011, 331002651)

pays_pop <- cbind.data.frame(pays_jc, getpop_jc,
                             stringsAsFactors = F)
names(pays_pop) <- c("pays", "pop")
names(getpop_jc) <- pays_pop$pays

####### check source and update date/time
url_address <- "https://github.com/CSSEGISandData/COVID%2D19/tree/master/csse_covid_19_data/csse_covid_19_time_series"

con <-  url(sub("%2D", "-", url_address)) # opens connection from URL
htmlCode <-  readLines(con)
close(con)
# date_field <- htmlCode[grep("      <span class=\"mr-1\">Latest commit</span>",
#                             #value = TRUE,
#                             htmlCode)+ 4]
htmlCode[grep("</relative-time>", htmlCode)] # 714
date_field <- htmlCode[grep("</relative-time>",
                            #value = TRUE,
                            htmlCode)]

update_datetime <- sub("T", " ", sub("        <relative-time datetime=\"", "",
                       sub("Z\" class=\"no-wrap\".+","", date_field)))
update_datetime <- strptime(update_datetime, "%Y-%m-%d %H:%M:%S")
update_datetime <- as.character(format(update_datetime,
                                       value = TRUE,  "%d %b %Y à %H:%M:%S"))

rm(htmlCode, con, date_field)

############## Import of data from the website
##############

####### import deaths
covid_deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                         stringsAsFactors = FALSE)
pays <- sort(unique(covid_deaths[,2]))

####### import confirmed cases
covid_confirmedCases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                                 stringsAsFactors = FALSE)
pays_confirmed <- sort(unique(covid_confirmedCases[,2]))

####### import recovered
covid_recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                            stringsAsFactors = FALSE)
pays_recovered <- sort(unique(covid_recovered[,2]))

#### check list of countries is the same everywhere

# remove all lines of covid_deaths and covid_confirmedCases
# that do not include notOK_country

if (!identical(pays, pays_recovered) || !identical(pays, pays_confirmed)) {
  covid_deaths <- filter(covid_deaths,
                         !Country.Region %in% notOK_country)
  covid_recovered <- filter(covid_recovered,
                            !Country.Region %in% notOK_country)
  pays <- sort(unique(covid_deaths[,2]))
  pays_recovered <- sort(unique(pays_recovered[,2]))

  identical(pays, pays_recovered)
  identical(pays, pays_confirmed)
  print("DONE")
}


##### initialize main dataframe covid_country
dates <- sub("X","",names(covid_deaths))[-(1:4)]
dates <- as.POSIXct( strptime(dates, "%m.%d.%y"))  # POSIXlt

covid_country <-  data.frame(dates,
                             row.names = seq_along(dates)) # POSIXct
names(covid_country) <- "dates"

########### created full dataframe for cumutive data
for (countr in 1:length(pays)) {

  country_chosen <- pays[countr] # "France"

  ######## process confirmed cases file
  covid_confirmed_country <- as_tibble(covid_confirmedCases[covid_confirmedCases$Country.Region == country_chosen,])
  covid_confirmed_country <- select(covid_confirmed_country,
                                    -Country.Region,
                                    -Province.State, -Lat, -Long)
  covid_confirmed_country <- colSums(covid_confirmed_country)
  df_cas_confirmes <- data.frame(dates,
                                 covid_confirmed_country,
                                 row.names = seq_along(dates))
  names(df_cas_confirmes) <- c("dates",
                               paste0(country_chosen,
                                      "_cas_confirmés"))

  covid_country <- full_join(covid_country, df_cas_confirmes, by = "dates")

  ######## process recovered file
  covid_recovered_country <- as_tibble(covid_recovered[covid_recovered$Country.Region == country_chosen,])
  covid_recovered_country <- select(covid_recovered_country,
                                    -Country.Region,
                                    -Province.State, -Lat, -Long)
  covid_recovered_country <- colSums(covid_recovered_country)
  df_remissions <- data.frame(dates,
                              covid_recovered_country,
                              row.names = seq_along(dates))
  names(df_remissions) <- c("dates",
                            paste0(country_chosen,"_rémissions"))
  covid_country <- full_join(covid_country, df_remissions, by = "dates")

  ###### process deaths file
  covid_deaths_country <- as_tibble(covid_deaths[covid_deaths$Country.Region == country_chosen,])
  covid_deaths_country <- select(covid_deaths_country,
                                 -Country.Region,
                                 -Province.State, -Lat, -Long)
  covid_deaths_country <- colSums(covid_deaths_country)

  df_deces <- data.frame(dates,
                         covid_deaths_country,
                         row.names = seq_along(dates))
  names(df_deces) <- c("dates", paste0(country_chosen,"_décès"))

  covid_country <- full_join(covid_country, df_deces, by = "dates")

}

rm(df_cas_confirmes, df_deces, df_remissions,
   covid_deaths, covid_recovered, covid_confirmedCases,
   pays_recovered, pays_confirmed, country_chosen, countr)

################
####### create dataframe with new cases per day covid_country_new
covid_country_new <- covid_country
covid_new_temp <- rbind(covid_country_new[1,],
                        covid_country_new)

covid_country_new <- rbind(covid_country_new,
                           tail(covid_country_new,1))
covid_country_new[,-1] <- covid_country_new[,-1] - covid_new_temp[,-1]

covid_country_new <- head(covid_country_new,-1)
rm(covid_new_temp)

