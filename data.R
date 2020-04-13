## Genera los datos para el seguimiento de covid

#these libraries need to be loaded
library(utils)
library(httr)
#library(ggplot2)
#library(RColorBrewer)

#download the dataset from the ECDC website to a local temporary file
GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

#read the Dataset sheet into “R”. The dataset will be called "data".
data <- read.csv(tf)

data$date <- as.POSIXlt(data$dateRep, format='%d/%m/%Y')
data <- data[order(data$date, decreasing=FALSE ),]

# cumulative sum
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    data$cumsum[k] <- cumsum(data$cases[k])
}


# days since 100 detected cases
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- data$cumsum[k]>=100
    #data$c100[k][l] <- 1:sum(l)
    data$c100[k] <- 1:sum(k)-which(l)[1]
}


# cumulative deaths
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    data$decs[k] <- cumsum(data$deaths[k])
}

# days since first death
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- data$decs[k]>=1
    #data$cd1[k][l] <- 1:sum(l)
    data$cd1[k] <- 1:sum(k)-which(l)[1]
}

# days since first 10 death
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- data$decs[k]>=10
    #data$cd10[k][l] <- 1:sum(l)
    data$cd10[k] <- 1:sum(k)-which(l)[1]
}


# cases rate cum
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- k & data$cumsum>0
    if (sum(l)>1){
        data$rcase[l][2:sum(l)] <-  (data$cumsum[l][2:sum(l)]/
                                         data$cumsum[l][1:(sum(l)-1)]) - 1}
}

# deaths rate cum 
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- k & data$decs>0
    if(sum(l)>1){
        data$rdeath[l][2:sum(l)] <-  (data$decs[l][2:sum(l)]/
                                          data$decs[l][1:(sum(l)-1)]) - 1
    }
}

# cases rate
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- k & data$cases>0
    if(sum(l)>1){
        data$rcase1[l][2:sum(l)] <-  (data$cases[l][2:sum(l)]/
                                          data$cases[l][1:(sum(l)-1)]) - 1
    }
}

# deaths rate
for(i in levels(data$countriesAndTerritories)){
    k <- data$countriesAndTerritories==i
    l <- k & data$deaths>0
    if(sum(l)>1){
        data$rdeath1[l][2:sum(l)] <-  (data$deaths[l][2:sum(l)]/
                                           data$deaths[l][1:(sum(l)-1)]) - 1
    }
}


data$date <- as.POSIXct(data$date) # For ggplot



