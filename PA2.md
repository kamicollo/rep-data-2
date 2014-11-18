# Reproducible Research: Assignment 2
Aurimas R.  

##Synopsis


##Data Processing

###Loading data

The data for this analysis is based on U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database, with data spanning from 1950 to 2011. It was aquired in 1 file and loaded for analysis.


```r
data <- read.csv(bzfile('repdata-data-StormData.csv.bz2'), stringsAsFactors=FALSE)
```

The data comprises the following attributes:


```r
names(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

###Processing numerical data

In this paper, we will mainly focus on the numerical attributes `INJURIES`, `FATALITIES`, `CROPDMG` (crop damage) and `PROPDMG` (property damage). While no pre-processing is required for injury or fatality data, the crop and property damage data has its multipliers reported separately, in `CROPDMGEXP` and `PROPDMGEXP` fields, with some additional issues. In particular, this is the data we found in the dataset.


```r
table(data$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
table(data$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

To tackle this, the following approach was followed: 

- all "h"/k"/"m"/"b" were assumed to represent hundreds, thousands, millions and billions respectively - all numeric values in the `_EXP` fields were assumed to use scientific notation (e.g. 10^N). In case the value field is zero, it was assumed that it should have been 1 instead (e.g. the damage is as indicated by the multiplier). This assumption was made after manually inspecting the data.
- In all other cases, the multiplier was converted to 1 (i.e. value assumed to be in dollars). 

The following function was used to perform the processing


```r
normalize_numbers <- function(numbers, multipliers) {
    ##let's normalize case
    multipliers <- toupper(multipliers)
    
    ##conversion to billions
    ix <- which(multipliers=="B")
    numbers[ix] <- numbers[ix] * 1000000000
    ##conversion to millions
    ix <- which(multipliers=="M")
    numbers[ix] <- numbers[ix] * 1000000
    ##conversion to thousands
    ix <- which(multipliers=="K")
    numbers[ix] <- numbers[ix] * 1000
    ##conversion to millions
    ix <- which(multipliers=="H")
    numbers[ix] <- numbers[ix] * 100
    
    ##cleanup of multipliers to leave only numeric ones
    multipliers[which(multipliers %in% list("M", "B", "H", "K", 0, "-"))] <- ""
    multipliers[which(!(multipliers %in% 1:9))] <- ""
    
    ##for cases where there is a value, multiply it with the multiplier
    ## (assumed scientific notation)
    ix <- which(numbers != 0 & multipliers %in% 1:9)
    numbers[ix] <- numbers[ix] * 10^as.numeric(multipliers[ix])
    
    ## for all cases where there is no value, replace value with multiplier
    ## (assumed scientific notation)
    ix <- which(numbers == 0 & multipliers %in% 1:9)
    numbers[ix] <- 10^as.numeric(multipliers[ix])
    
    ##return numbers
    numbers
}

data$Property.Damage <- normalize_numbers(data$PROPDMG, data$PROPDMGEXP)
data$Crop.Damage <- normalize_numbers(data$CROPDMG, data$CROPDMGEXP)
```

To ensure that at least the key event data is correct, we manually reviewed top 10 events by property damage value and compared the damage value indicated to the any mentions of damage in the text. References reviewed:


```r
data$REFNUM[order(data$Property.Damage, decreasing=TRUE)[1:10]]
```

```
##  [1] 605943 577616 577615 581535 569288 581533 581537 529299 444407 187564
```
The following discrepancies were identified and corrected:

 1. Reference #605953 - the damage value was indicated as $115 billion, which would make it the highest in the dataset. However, the crop damage was limited to 32.5 million and the text mentions damage costs of $70 million. We believe this represents a data input error and we changed the estimate to $115 million instead.
 

```r
 data$Property.Damage[605943] <- data$Property.Damage[605953] / 1000
```
 
 1. Reference #187564 - the damage value was indicated as $5 billion, but the text mentions that most damage estimates were close to $50 million. The value was thus changed to $50 million.
 

```r
 data$Property.Damage[187564] <- 50000000
```

After performing the pre-processing, the following summary statistics were obtained.


```r
summary(data$Property.Damage)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 0.000e+00 0.000e+00 4.694e+05 5.000e+02 1.150e+11
```

```r
summary(data$Crop.Damage)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.000e+00 0.000e+00 0.000e+00 5.442e+04 0.000e+00 5.000e+09
```

###Processing event types


```r
count_unique <- length(unique(data$EVTYPE))
```

The original dataset includes 985 event types. This is a number that cannot be used for analysis, as the results will simply be non-summarizable. Additionaly, the detailed specification used bears limited value to policy makers. As an example, these are the event types related to thunderstorms:


```r
unique(data$EVTYPE)[grepl("Thunder",unique(data$EVTYPE),ignore.case = TRUE)]
```

```
##  [1] "THUNDERSTORM WINDS"             "THUNDERSTORM WIND"             
##  [3] "THUNDERSTORM WINS"              "THUNDERSTORM WINDS LIGHTNING"  
##  [5] "THUNDERSTORM WINDS/HAIL"        "THUNDERSTORM WINDS HAIL"       
##  [7] "FLASH FLOODING/THUNDERSTORM WI" "THUNDERSTORM"                  
##  [9] "THUNDERSTORM WINDS/FUNNEL CLOU" "SEVERE THUNDERSTORM"           
## [11] "SEVERE THUNDERSTORMS"           "SEVERE THUNDERSTORM WINDS"     
## [13] "THUNDERSTORMS WINDS"            "THUNDERSTORMS"                 
## [15] "LIGHTNING THUNDERSTORM WINDSS"  "THUNDERSTORM WINDS 60"         
## [17] "THUNDERSTORM WINDSS"            "LIGHTNING THUNDERSTORM WINDS"  
## [19] "LIGHTNING AND THUNDERSTORM WIN" "THUNDERSTORM WINDS53"          
## [21] "THUNDERSTORM WINDS 13"          "THUNDERSNOW"                   
## [23] "THUNDERSTORM WINDS URBAN FLOOD" "THUNDERSTORM WINDS SMALL STREA"
## [25] "THUNDERSTORM WINDS 2"           "THUNDERSTORM WINDS 61"         
## [27] "THUNDERSTORM DAMAGE"            "THUNDERTORM WINDS"             
## [29] "THUNDERSTORMW 50"               "THUNDERSTORMS WIND"            
## [31] "THUNDERSTORM  WINDS"            "THUNDERTSORM WIND"             
## [33] "THUNDERSTORM WINDS/ HAIL"       "THUNDERSTORM WIND/LIGHTNING"   
## [35] "THUNDERSTORM WIND G50"          "THUNDERSTORM WINDS/HEAVY RAIN" 
## [37] "THUNDERSTROM WINDS"             "THUNDERSTORM WINDS      LE CEN"
## [39] "THUNDERSTORM WINDS G"           "THUNDERSTORM WIND G60"         
## [41] "THUNDERSTORM WINDS."            "THUNDERSTORM WIND G55"         
## [43] "THUNDERSTORM WINDS G60"         "THUNDERSTORM WINDS FUNNEL CLOU"
## [45] "THUNDERSTORM WINDS 62"          "THUNDERSTORM WINDS/FLASH FLOOD"
## [47] "THUNDERSTORM WINDS 53"          "THUNDERSTORM WIND 59"          
## [49] "THUNDERSTORM WIND 52"           "THUNDERSTORM WIND 69"          
## [51] "THUNDERSTORMW WINDS"            "THUNDERSTORM WIND 60 MPH"      
## [53] "THUNDERSTORM WIND 65MPH"        "THUNDERSTORM WIND/ TREES"      
## [55] "THUNDERSTORM WIND/AWNING"       "THUNDERSTORM WIND 98 MPH"      
## [57] "THUNDERSTORM WIND TREES"        "THUNDERSTORM WIND 59 MPH"      
## [59] "THUNDERSTORM WINDS 63 MPH"      "THUNDERSTORM WIND/ TREE"       
## [61] "THUNDERSTORM DAMAGE TO"         "THUNDERSTORM WIND 65 MPH"      
## [63] "THUNDERSTORM WIND."             "THUNDERSTORM WIND 59 MPH."     
## [65] "THUNDERSTORM HAIL"              "THUNDERSTORM WINDSHAIL"        
## [67] "THUNDERSTORM WINDS AND"         "THUNDERSTORM WINDS 50"         
## [69] "THUNDERSTORM WIND G52"          "THUNDERSTORM WINDS 52"         
## [71] "THUNDERSTORM WIND G51"          "THUNDERSTORM WIND G61"         
## [73] "THUNDERESTORM WINDS"            "THUNDERSTORM WINDS/FLOODING"   
## [75] "THUNDERSTORM W INDS"            "THUNDERSTORM WIND 50"          
## [77] "THUNDERSTORM WIND 56"           "THUNDERSTORM WIND/HAIL"        
## [79] "THUNDERSTORMW"                  "THUNDERSTORM WINDS/ FLOOD"     
## [81] "THUNDERSTORMWINDS"              "THUNDERSTORM WINDS HEAVY RAIN" 
## [83] "THUNDERSTROM WIND"              "Thunderstorm Wind"             
## [85] "Thundersnow shower"             "THUNDERSTORM WIND (G40)"       
## [87] "GUSTY THUNDERSTORM WINDS"       "GUSTY THUNDERSTORM WIND"       
## [89] "MARINE THUNDERSTORM WIND"
```

Unfortunately, the NOAA does not provide any grouping of the event types. As a result, the following strategy was devised to simplify the dataset:

 - simple string transformations to ease the analysis were done
 - "TSTM" abbreviation was replaced with full word ("thunderstorm")
 - All common adjectives were removed (high', 'extreme', 'excessive', 'unreasonable', 'unusual', 'strong', 'small', 'record', 'heavy', 'early', 'abnormal', 'non-severe').
 - A (Levenshtein algorithm)[http://en.wikipedia.org/wiki/Levenshtein_distance] was applied to find similar event types (using distance between types of 25%). 
 
The following function was used for implementation.


```r
normalize_type <- function(types, distance=0.3, loops=1) {
    ##normalize case
    types <- tolower(types)
    types <- gsub('/', ' ', types)
    #first, let's remove unwanted adjectives. Would be good to do this with an NLP package,
    #but I found it a bit non-intuitive to use
    types <- removeAdjectives(types)
    #let's also take care of TSTM abbreviation
    types <- gsub('tstm', 'thunderstorm', types)
    ##recode missing cases
    types <- recode_missing(types)
    
    #let's use data.table to make this faster
    library(data.table)
    types <- data.table(types)
    #loop k times - after each loop, the types should be coverging more and more.
    for(k in 1:loops) {
        #get unique list of types. Let's sort it so that shorter names get in front
        # (e.g. HEAT is before HEAT WAVE)
        u_types <- unique(types)
        u_types <- u_types[order(u_types[[1]], na.last=TRUE)]
        #str(u_types)
        #print(nrow(u_types))
        checked <- vector(mode="character", length=nrow(u_types))
        pos <- 1
        #loop through unique types
        for (i in 1:nrow(u_types)) {
            #print(i)
            type = u_types[[i, 1]]
            #let's see if this is not a "canonical type" yet
            if (!(type %in% checked)) {
                #find similar types using Levenshtein algorithm
                similar = agrep(type, u_types[[1]], value=TRUE, max.distance = distance)
                #replace all similar values with the current type (e.g. make it canonical)
                for (j in 1:length(similar)) {
                    #however, we should not replace already "canonized types" during this iteration
                    if (!(similar[j] %in% checked)) {
                        u_types[u_types == similar[j]] <- type
                        types[types == similar[j]] <- type
                    }
                }
                #add current type to canonical list
                checked[pos] <- type
                pos <- pos + 1
            }
        }
    }
    toupper(types[[1]])
}

#this function removes selected adjectives from the list
removeAdjectives <- function(list) {
    #predefined list of adjectives, determined by looking through the raw data
    adj = c('high', 'extreme', 'excessive',
            'unreasonable', 'unusual', 'strong', 'small', 'record',
            'heavy', 'early', 'abnormal', 'non-severe')
    #as gsub() does not accept a vector of replacements, we will loop through and apply 
    #gsub for each adjective separately (e.g. replace them to nothing)
    for (i in 1:length(adj)) {
        list <- gsub(adj[i], '', list)
    }
    #finally, let's trim the names to make them nicer
    library(stringr)
    list <- str_trim(list)
    list
}

recode_missing <- function(list) {
    list[which(list == '?' | list == '')] <- NA
    list
}
```


```r
data$Event.Type <- normalize_type(data$EVTYPE)
types <- unique(data$Event.Type)
count <- length(types)
```

After applying the function to the dataset, the following 60 event types remained:


```r
sort(types)
```

```
##  [1] "ACCUMULATED SNOWFALL"       "AGRICULTURAL FREEZE"       
##  [3] "APACHE COUNTY"              "ASTRONOMICAL  TIDE"        
##  [5] "AVALANCE"                   "BEACH EROSIN"              
##  [7] "BEACH FLOOD"                "BELOW NORMAL PRECIPITATION"
##  [9] "BITTER WIND CHILL"          "BLACK ICE"                 
## [11] "BLIZZARD"                   "BLOWING DUST"              
## [13] "BLOW-OUT TIDE"              "BRUSH FIRE"                
## [15] "COASTAL EROSION"            "COASTALFLOOD"              
## [17] "COASTAL SURGE"              "COLD"                      
## [19] "DAMAGING FREEZE"            "DAM BREAK"                 
## [21] "DAM FAILURE"                "DEEP HAIL"                 
## [23] "DENSE FOG"                  "DOWNBURST"                 
## [25] "DRIEST MONTH"               "DRIFTING SNOW"             
## [27] "DROUGHT"                    "DROWNING"                  
## [29] "DRY"                        "DUST DEVEL"                
## [31] "DUST STORM"                 "FALLING SNOW ICE"          
## [33] "FIRST FROST"                "FOG"                       
## [35] "FREEZE"                     "FROST"                     
## [37] "FUNNEL"                     "GLAZE"                     
## [39] "GRADIENT WIND"              "GUSTNADO"                  
## [41] "GUSTY WIND"                 "HAIL"                      
## [43] "HEAT"                       "HURRICANE"                 
## [45] "ICE"                        "LACK OF SNOW"              
## [47] "LANDSLUMP"                  "LIGNTNING"                 
## [49] "LOW"                        "LY WARM"                   
## [51] "MIX"                        "NONE"                      
## [53] "STORM SURGE"                "SURF"                      
## [55] "SWELLS"                     "TORNADO"                   
## [57] "WAVES"                      "WAYTERSPOUT"               
## [59] "WIND"
```

Now the data is assumed to be clean enough to be used for analysis and recommendations.

##Results

###Harmfulness to population health

###Economic consequences
