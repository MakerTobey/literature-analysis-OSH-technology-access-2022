## Load libraries
  library(tidyverse)
  library(ggplot2)
  library(scales)
  library(dplyr)
  library(readxl)
  library(ggrepel)
  library(treemapify)
  library(bibliometrix)

### Prepare overall country statistics graphs
## Load data 
  getwd() # which working directory am I in?
  # setwd("/home/documents...")
  
  # load Scimagojr data cientific publication by counrty
  Hfactor <- read_excel("data/scimagojr country rank 1996-2020 all.xlsx")
  # adjust some country labels
  Hfactor[Hfactor$Country == "Haïti", "Country"] <- "Haiti"
  Hfactor[Hfactor$Country == "Russian Federation", "Country"] <- "Russia"
  Hfactor[Hfactor$Country == "Macedonia", "Country"] <- "North Macedonia"
  Hfactor[Hfactor$Country == "Côte d’Ivoire", "Country"] <- "Côte d'Ivoire"
  Hfactor[Hfactor$Country == "Viet Nam", "Country"] <- "Vietnam"
  # remove unused columns
  Hfactor <- select(Hfactor, -c(Region))
  
  #load UN datasets containing global regions and population and merge them
  #load UN methods dataset specifying UN sub-regions in STGs https://unstats.un.org/unsd/methodology/m49/overview/
  UNMethods <- read_delim("data/UNSD — Methodology.csv", delim = ",") %>%
                rename(Country = `Country or Area`)
  #load UN methods dataset specifying UN sub-regions - for key countries with missing data for the year 2019, the UN projection data (medium) was added manually
  UNPopulation2019 <- read_delim("data/UNdata_Export_20220415_224316842_extended.csv", delim = ",") %>%
                      rename(Country = `Country or Area`, Population  = Value)
  # select only total population and all sexes, and filter columns that could be confusing later and duplicate entries
  UNPopulation2019_total <- UNPopulation2019[UNPopulation2019$Sex == "Both Sexes" &  UNPopulation2019$Area == "Total",]
  UNPopulation2019_total <- select(UNPopulation2019_total, -c(Year, Area, Sex))
  UNPopulation2019_total <- UNPopulation2019_total[!duplicated(UNPopulation2019_total$Country), ]
  # join the two UN data sets before adjusting nomenclature to the other data sets
  UNData <- left_join(UNMethods, UNPopulation2019_total, by = "Country") 
  # adjust some country labels to make them shorter and compatible with Scimagojr data
  UNData[UNData$Country == "United States of America", "Country"] <- "United States"
  UNData[UNData$Country == "United Kingdom of Great Britain and Northern Ireland", "Country"] <- "United Kingdom"
  UNData[UNData$Country == "Venezuela (Bolivarian Republic of)", "Country"] <- "Venezuela"
  UNData[UNData$Country == "Bolivia (Plurinational State of)", "Country"] <- "Bolivia"
  UNData[UNData$Country == "Republic of Korea", "Country"] <- "South Korea"
  UNData[UNData$Country == "Democratic People's Republic of Korea", "Country"] <- "North Korea"
  UNData[UNData$Country == "Czechia", "Country"] <- "Czech Republic"
  UNData[UNData$Country == "Iran (Islamic Republic of)", "Country"] <- "Iran"
  UNData[UNData$Country == "United Republic of Tanzania", "Country"] <- "Tanzania"
  UNData[UNData$Country == "State of Palestine", "Country"] <- "Palestine"
  UNData[UNData$Country == "Republic of Moldova", "Country"] <- "Moldova"
  UNData[UNData$Country == "Democratic Republic of the Congo", "Country"] <- "Democratic Republic Congo"
  UNData[UNData$Country == "Cabo Verde", "Country"] <- "Cape Verde"
  UNData[UNData$Country == "Côte d’Ivoire", "Country"] <- "Côte d'Ivoire"
  UNData[UNData$Country ==  "Russian Federation", "Country"] <- "Russia"
  UNData[UNData$Country ==  "Viet Nam", "Country"] <- "Vietnam"
  UNData[UNData$Country ==  "Lao People's Democratic Republic", "Country"] <- "Laos"
  UNData[UNData$`M49 Code` == " Hong Kong Special Administrative Region", "Country"] <- "Hong Kong"
  UNData[UNData$`M49 Code` == " Macao Special Administrative Region", "Country"] <- "Macao"
  # remove unused columns
  UNData <- select(UNData, -c(`Value Footnotes`, `Source Year`, `Record Type`, Reliability))
  UNData <- select(UNData, -c(`Global Code`, `ISO-alpha2 Code`, `Global Name`))
  
  # load dataset on economic complexity https://atlas.cid.harvard.edu/rankings
  CountryComplexity <- read_delim("data/Country Complexity Rankings 1995 - 2019.csv", delim = ",")
  # adjust some country labels to make them shorter and compatible with Scimagojr data
  CountryComplexity[CountryComplexity$Country == "United States of America", "Country"] <- "United States"
  CountryComplexity[CountryComplexity$Country == "Democratic Republic of the Congo", "Country"] <- "Democratic Republic Congo"
  CountryComplexity[CountryComplexity$Country == "Republic of the Congo", "Country"] <- "Congo"
  CountryComplexity[CountryComplexity$Country == "Czechia", "Country"] <- "Czech Republic"
  #delete row not present in h-factor
  CountryComplexity = CountryComplexity[!CountryComplexity$Country == "Eswatini",]
  # remove unused columns
  CountryComplexity <- select(CountryComplexity, c(Country, `ECI 2019`))
  
  # load data of researcher density from Worldbank - World Development Indicators https://data.worldbank.org/indicator/SP.POP.SCIE.RD.P6
  Researchers <- read_delim("data/API_SP.POP.SCIE.RD.P6_DS2_en_csv_v2_3932151_extended.csv", delim = ",") %>%
    rename(`ISO-alpha3 Code` = `Country Code`) #rename country code column according to UN table
    # the extended file contains the values of 13 extra countries based on UNECSO data for total nr HC scientists divided by population size
  # make data tidy (individual year observations into individual rows)
  Researchers <- Researchers %>% 
    pivot_longer(as.character(c(1960:2020)), names_to = "year", values_to = "nr researchers per m")
  # delete empty observations and select the latest available timepoint for each country
  Researchers <- Researchers[!(is.na(Researchers$`nr researchers per m`) | Researchers$`nr researchers per m`==""), ]
  Researchers <- Researchers[!duplicated(Researchers$`ISO-alpha3 Code`, fromLast = TRUE), ]
  # remove unused columns
  Researchers <- select(Researchers, -c(X66, `Country Name`, year, `Indicator Code`))
  
  
## Merge different datasets
  #merge the data of the UN Methods table for all countries where h-factors exist:
  df <- right_join(UNData, Hfactor, by = "Country") 
  #merge the data of the other tables for courtries where economic complexity data is available:
  df <- right_join(df, CountryComplexity, by = "Country")
  #merge number of scientists to the data of the other tables
  df <- inner_join(df, Researchers, by = "ISO-alpha3 Code")

  
## save dataframe and reload in future session
  save(df, file = "df.RData")
  write.csv(df, file = "df.csv")
  #load the file data/df.RData
  load(".../data/df.RData")
  
  
## Plot data
  # ggplot dotplot on combined datasets with nr scientists
  ggplot(filter(df), aes(x = `ECI 2019`, y = `nr researchers per m`)) + #
    geom_point(aes(size = `H index`, fill = `Region Name`), shape = 21,  alpha = 0.9) +
    #scale_fill_brewer(palette = "Set3") +
    geom_text_repel(aes(label = `ISO-alpha3 Code`, color=factor(`Region Name`), size=`H index`), hjust=-0.4, vjust=0.5) + #check_overlap = TRUE, size = 2
    scale_size(range=c(0.5,3)) +
    labs(title = "Countries scientific output related to their economic & scientist ecocystem",
         x = "Country economic complexity index (ECI 2019)",
         y = "Researchers in R&D per million people",
         size = "H-factor",
         fill = "Region") +
    guides(fill = guide_legend(override.aes = list(size=3))) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "right", 
          legend.title = element_blank())
  
  # ggplot area-fractions - Total publications per Region
    ggplot(df, aes(area = `Citable documents`, fill = `Region Name`, subgroup = `Region Name`, label = Country)) +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "black") +
      geom_treemap_subgroup_text(fontface = "bold", colour = "#f0f0f0", alpha = 0.7, place = "bottomleft") +
      geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Citable documents per Country",
           x = NULL, 
           y = NULL, 
           fill = NULL) +
      theme(legend.position = "none")   


    
### Prepare plot documents published using OSH tools ### REVISION - now for papers only mentioning the tools (Raspberry Pi and Arduino) in the methods section
## Load data
  # data import countries
  dfcountry2 <- read.delim("data/osh publications/RPi+Arduino-countries-analyze-filtered.txt") %>%
    rename(Country = `Countries.Regions`)
    dfcountry2 <- rename(dfcountry2, Count = `Record.Count`)
    dfcountry2 <- rename(dfcountry2, Percent = `X..of.13.285`)
    dfcountry2 <- slice(dfcountry2, 1:(n() - 1))     # delete last row with info on empty data
  
  # the above web of science dataset does not contain the information about which papers mention
    #the open tools in their method section, i therefore now import the whole WoS data export of
    #these publications to then compare them again another dataset that contains the method section
    #information but not the country information
  # load with bibliometrix
  wosdf <- convert2df("data/osh publications/OSH_savedrecs.txt", dbsource = "isi", format="plaintext")
  
  # load scite data with arduino or raspberry pi search in method section only:
  OSHinMethods1 <- read_delim("data/osh publications/scite-raspberry pi-2022-09-21.csv", delim = ",")
  OSHinMethods2 <- read_delim("data/osh publications/scite-arduino-2022-09-21.csv", delim = ",")
  OSHinMethods3 <- read_delim("data/osh publications/scite-arduino-2022-09-21-2.csv", delim = ",")
  OSHinMethods <- rbind(OSHinMethods1, OSHinMethods2, OSHinMethods3)

  # select rows of bibliometrix dataset with DOI match
  # alternative for joined table: OSHjoineddfDOI <- inner_join(wosdf, OSHinMethods, by = c("DI"="doi")) 
  dfOSHmatchedDOI <- wosdf[wosdf$DI %in% OSHinMethods$doi,]
  
  # how many WoS entries are missing their DOI (DI)?
  sum(is.na(wosdf$DI))
  # almost half! So we merge the others based on the title (TI/title).
  # for merging, select only those rows without DOI:
  wosdfnodoi <- wosdf[is.na(wosdf$DI),]
  
  #find intersection of WoS dataset (more restricted publication venues) and Scite dataset (mention in method section only)
  # it turns out some database entries are without titles -> remove them
  sum(is.na(OSHinMethods$title))
  OSHinMethodst <- OSHinMethods[!is.na(OSHinMethods$title),]
  # convert titles to lower case to avoid case based mis-matching
  wosdfnodoi$TI <- tolower(wosdfnodoi$TI)
  OSHinMethodst$title <- tolower(OSHinMethodst$title)
  # remove doublicated titles (e.g. pre-prints) 
  sum(duplicated(wosdfnodoi$TI))
  wosdfnodoi <- wosdfnodoi[!duplicated(wosdfnodoi$TI),]
  sum(duplicated(OSHinMethodst$title))
  OSHinMethodst <- OSHinMethodst[!duplicated(OSHinMethodst$title),]
  # extract rows with (exactly) matching titles between datasets (fuzzyjoin did not work with bibliometrix)
  dfOSHmatchedtitle <- wosdfnodoi[wosdfnodoi$TI %in% OSHinMethodst$title,]

  # merge both datasets - those matched by DOI and those matched by title:
  dfOSHmethodsWOS <- rbind(dfOSHmatchedDOI, dfOSHmatchedtitle)
  
## data processing
  # use bibliometrix analysis to get country frequency:
  resultsbibOSHmethods <- biblioAnalysis(dfOSHmethodsWOS)
  plot(x=resultsbibOSHmethods, k=10, pause=F)
  countryranking <- resultsbibOSHmethods$Countries
  countryranking <- as.data.frame(countryranking) 
  #convert table to dataframe
  countryranking <- as.data.frame(countryranking, row.names = NULL, optional = FALSE,
  cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
  check.names = !optional, stringsAsFactors = FALSE)
  #rename
  countryranking <- rename(countryranking, Country = `Tab`)
  countryranking <- rename(countryranking, Count = `Freq`)
  dfcountry <- countryranking # use previous df name to ease code integration
    
  # adjust some country labels to match modified UN set used earlier
    dfcountry[dfcountry$Country == "USA", "Country"] <- "United States"
    dfcountry[dfcountry$Country == "TRINIDAD TOBAGO", "Country"] <- "Trinidad and Tobago"
    dfcountry[dfcountry$Country == "REP CONGO", "Country"] <- "Congo"
    dfcountry[dfcountry$Country == "DEM REP CONGO", "Country"] <- "Democratic Republic Congo"
    dfcountry[dfcountry$Country == "COTE IVOIRE", "Country"] <- "Côte d'Ivoire"
    dfcountry[dfcountry$Country == "PEOPLES R CHINA", "Country"] <- "China"
    dfcountry[dfcountry$Country == "U ARAB EMIRATES", "Country"] <- "United Arab Emirates"
    dfcountry[dfcountry$Country == "BOSNIA", "Country"] <- "Bosnia and Herzegovina"
    dfcountry[dfcountry$Country == "BRUNEI", "Country"] <- "Brunei Darussalam" # not elegant naming in UN set
    dfcountry[dfcountry$Country == "SYRIA", "Country"] <- "Syrian Arab Republic" # not elegant naming in UN set
    dfcountry[dfcountry$Country == "MACEDONIA", "Country"] <- "North Macedonia"
    dfcountry[dfcountry$Country == "CENT AFR REPUBL", "Country"] <- "Central African Republic"
    dfcountry[dfcountry$Country == "KOREA", "Country"] <- "South Korea"
    dfcountry[dfcountry$Country == "ST LUCIA", "Country"] <- "Saint Lucia"
    dfcountry[dfcountry$Country == "ST HELENA", "Country"] <- "Saint Helena"

    save(dfcountry, file = "dfcountryOSHmethodsWOS.RData")
    write.csv(dfcountry, file = "dfcountryOSHmethodsWOS.csv") 
       
    # execute earlier code again to load and format the UNData dataset (before it was reduced to joined data points with other datasets in the earlier df)
    # merge the data of document countries to the UNDAta set to obtain nice and corent county names
    #require(fuzzyjoin) # merge while ignoring the capitalization of the web of science dataset
    dfUNcountry <- regex_left_join(dfcountry, UNData, by = "Country", ignore_case =TRUE)
    dfUNcountry <- dfUNcountry %>%
      filter(!row_number() %in% c(93)) # KOSOVO  is no country in UN set
    
## safe dataframe for reuse
    save(dfUNcountry, file = "dfUNcountryOSHmethodsWOS.RData")
    write.csv(dfUNcountry, file = "dfUNcountryOSHmethodsWOS.csv")
    
    
## Plot publication countries using countries from above dataset
    #ggplot area-fractions - Arduino AND RPi publications per Region 
    ggplot(dfUNcountry, aes(area = `Count`, fill = `Region Name`, subgroup = `Region Name`, label = `Country.y`)) +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "black") +
      geom_treemap_subgroup_text(fontface = "bold", colour = "#f0f0f0", alpha = 0.7, place = "bottomleft") +
      geom_treemap_text(colour = "white", place = "centre", reflow = TRUE) +
      scale_fill_brewer(palette = "Set2") +
      labs(title = "Documents per Country",
           x = NULL, 
           y = NULL, 
           fill = NULL) +
      theme(legend.position = "none")
    
