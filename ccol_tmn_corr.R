#install.packages("plyr")
#install.packages("tsbox")
#install.packages("ggpubr")
library(tsbox)
library(tidyverse)
library(ggpubr)


# Importing site mean monthly minimum temperature...
# ...keeping the month column as a character column
ccol_tmn_csv <- read.csv("CAPE_COLUMBINE.means.tmn.csv", stringsAsFactors = F)

# Removes data error with the last row
ccol_tmn_csv <- ccol_tmn_csv[1:509, 2]


# Converting csv data into a time series: aim to get the date index right...
tmn.site <- ts(ccol_tmn_csv, start = 1959, frequency = 12) 

# Plot to inpsect time series data
plot(tmn.site)

# Check the data characteristics of the time series data
str(tmn.site)

# Converst the time series data back into a dataframe object with the correct date indexing
ccol_tmn_df <- ts_df(tmn.site)

# Checks the characteristics of the date frame object
str(ccol_tmn_df)

# Importing the csv data for the CRU records: keeping the numbers 'as-is' for the column names...
# ... also keeps the row names as character strings, preventing conversion to factors
cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)

# Extracting the Cape Columbine data for mean monthly minimum temperature
cru_ccol_tmn <- t(cru_tmn_csv[1, 2:length(cru_tmn_csv)])

# Giving the data a name
colnames(cru_ccol_tmn) <- "tmn"

# COnverting CRU data for Cape Columbine into a time series data to achieve date indexing
tmn.cru <- ts(cru_ccol_tmn, start = 1901, frequency = 12)

# Plots various sections of the CRU data to check the shape of the data
plot(tmn.cru)
plot(window(tmn.cru, 1959, 2001))

# Converts the time series back into a dataframe object with date indexing
cru_ccol_tmn_df <- ts_df(tmn.cru)

# Checking characteristics of data
str(cru_ccol_tmn_df)

# Combining the two data frames by "time", which is the date of the observations
corr_tmn_df <- full_join(cru_ccol_tmn_df, ccol_tmn_df, by = "time")

# Checking the characteristics of the dataframe
str(corr_tmn_df)

# Code that omits the NA values
corr_clean_tmn_df <- na.omit(corr_tmn_df)

# Renames the columns to work with the correlation analysis
colnames(corr_clean_tmn_df) <- c("time", "cru", "ccol")

# Finds the correlation between the two sets of data...
# ...might want to find the correlation between variables row by row
# Finds the correlation and a confidence interval for the correlation coefficient
cor.test(corr_clean_tmn_df$cru, corr_clean_tmn_df$ccol)

# Makes a regression plot of the two variabels
ggscatter(corr_clean_tmn_df, x = "cru", y = "ccol", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "ccol")