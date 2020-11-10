library(knitr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(lattice)
library(R.utils)
# DOWNLOAD DATA FROM SOURCE AND CONVERT TO DATAFRAME.
# Download zip datafile from cloudfront
source_url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
datasource_zip="/Users/Brantana-Air/Desktop/Data_Projects/Coursera/Johns_Hopkins/Reproducible_Research/Course_Project_2/storms.zip"
download.file(source_url,destfile=datasource_zip)
# Unzip datafile
bunzip2(datasource_zip, "storms.csv", remove = FALSE, skip = TRUE)
# Convert csv file to dataframe
datasource_csv="/Users/Brantana-Air/Desktop/Data_Projects/Coursera/Johns_Hopkins/Reproducible_Research/Course_Project_2/storms.csv"
df0 <- as.data.frame(read.csv(file=datasource_csv, header=TRUE, sep=",",stringsAsFactors = FALSE))
# Select only more recent and accurate dates
df0$BGN_DATE<-mdy_hms(df0$BGN_DATE)
df0<-df0 %>% filter(year(BGN_DATE)>1996)
# DATA ANALYSIS
# 1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
# are most harmful with respect to population health?
# Number of buckets to plot, N
N = 25
# Produce dataframe for Injuries
df1 <- df0 %>% 
        group_by(EVTYPE) %>%
        summarise(Sum_Injuries = sum(INJURIES))
df1 <- df1[order(df1$Sum_Injuries, decreasing=TRUE), ]
df1$cumulative <- cumsum(df1$Sum_Injuries)
s1 = sum(df1$Sum_Injuries)
df1 <- df1[0:N,]
df1$EVTYPE <- factor(df1$EVTYPE, levels = df1$EVTYPE[order(df1$cumulative)])
# Produce dataframe for Fatalities
df2 <- df0 %>% 
        group_by(EVTYPE) %>%
        summarise(Sum_Fatalities = sum(FATALITIES))
df2 <- df2[order(df2$Sum_Fatalities, decreasing=TRUE), ]
df2$cumulative <- cumsum(df2$Sum_Fatalities)
s2 = sum(df2$Sum_Fatalities)
df2 <- df2[0:N,]
df2$EVTYPE <- factor(df2$EVTYPE, levels = df2$EVTYPE[order(df2$cumulative)])
# Produce Pareto plots
# Plot 1 - Major incidents of injuries
ggplot(df1, aes(x=df1$EVTYPE)) +
        scale_y_continuous(name = "Injuries by Event Type", 
                sec.axis = sec_axis(~./s1, name = "Cumulative % of Overall Injuries", 
                                               labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
        geom_bar(aes(y=Sum_Injuries), fill='blue', stat="identity") +
        geom_point(aes(y=df1$Sum_Injuries), color = rgb(0, 1, 0), pch=16, size=1) +
        geom_path(aes(y=df1$cumulative, group=1), colour="slateblue1", lty=1, size=0.9) +
        theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=1)) +
        labs(title = "Injuries", x = 'Event Type', y = 'Count', subtitle = "Pareto Plot 1")
#dev.copy(png,file="plot1.png")
#dev.off()
# Plot 2 - Major incidents of fatalities        
ggplot(df2, aes(x=df2$EVTYPE)) +
        scale_y_continuous(name = "Fatalities by Event Type", 
                sec.axis = sec_axis(~./s2, name = "Cumulative % of Overall Fatalities", 
                                              labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
        geom_bar(aes(y=Sum_Fatalities), fill='blue', stat="identity") +
        geom_point(aes(y=df2$Sum_Fatalities), color = rgb(0, 1, 0), pch=16, size=1) +
        geom_path(aes(y=df2$cumulative, group=1), colour="slateblue1", lty=1, size=0.9) +
        theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=1)) +
        labs(title = "Fatalities", x = 'Event Type', y = 'Count', subtitle = "Pareto Plot 2")       
#dev.copy(png,file="plot2.png")
#dev.off()
# 2. Across the United States, which types of events have the greatest economic consequences?
# Convert property damage exponent symbols to numeric orders of magnitude
df3 <- df0
df3 <- df3 %>%  mutate(PROPDMGEXP = tolower(PROPDMGEXP)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="-",0)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="+",0)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="?",0)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="b",9)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="m",6)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="k",3)) %>%
        mutate(PROPDMGEXP=replace(PROPDMGEXP,PROPDMGEXP=="h",2)) %>%
        mutate(PROPDMGEXP = as.numeric(PROPDMGEXP))
# Convert crop damage exponent symbols to numeric orders of magnitude
df3 <- df3 %>%  mutate(CROPDMGEXP = tolower(CROPDMGEXP)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="-",0)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="+",0)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="?",0)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="b",9)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="m",6)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="k",3)) %>%
        mutate(CROPDMGEXP=replace(CROPDMGEXP,CROPDMGEXP=="h",2)) %>%
        mutate(CROPDMGEXP = as.numeric(CROPDMGEXP))
# Address na's
df3[which(is.na(df3$PROPDMGEXP) & !is.na(df3$PROPDMG)), "PROPDMGEXP"]<-0
df3[which(is.na(df3$CROPDMGEXP) & !is.na(df3$CROPDMG)), "CROPDMGEXP"]<-0
# Develop total damage 
df3$totaldamage<-df3$PROPDMG*10^df3$PROPDMGEXP+df3$CROPDMG*10^df3$CROPDMGEXP
# Number of buckets to plot, N
N = 25
# Produce dataframe for Injuries - Damage basis = Billion US$
df4 <- df3 %>% 
        group_by(EVTYPE) %>%
        summarise(Sum_Damage = sum(totaldamage)/10^9)
df4 <- df4[order(df4$Sum_Damage, decreasing=TRUE), ]
df4$cumulative <- cumsum(df4$Sum_Damage)
s4 = sum(df4$Sum_Damage)
df4 <- df4[0:N,]
df4$EVTYPE <- factor(df4$EVTYPE, levels = df4$EVTYPE[order(df4$cumulative)])
# Plot 3 - Major incidents of economic damage
ggplot(df4, aes(x=df4$EVTYPE)) +
        scale_y_continuous(name = "Economic Damage, B$", 
                           sec.axis = sec_axis(~./s4, name = "Cumulative % of Ovarall Economic Damage", 
                                               labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
        geom_bar(aes(y=Sum_Damage), fill='blue', stat="identity") +
        geom_point(aes(y=df4$Sum_Damage), color = rgb(0, 1, 0), pch=16, size=1) +
        geom_path(aes(y=df4$cumulative, group=1), colour="slateblue1", lty=1, size=0.9) +
        theme(axis.text.x = element_text(angle=90, vjust=0.6, hjust=1)) +
        labs(title = "Economic Damage", x = 'Event Type', y = 'Damage by Event Type, B$', subtitle = "Pareto Plot 3")
#dev.copy(png,file="plot3.png")
#dev.off()







