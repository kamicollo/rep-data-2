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

normalize_type <- function(types, distance=0.2, loops=3) {
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