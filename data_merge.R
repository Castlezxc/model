#install.packages("plyr")
library(plyr)
library(readr)

# Keeping the month column as a character column
cape_columbine_tmn <- read.csv("CAPE_COLUMBINE.means.tmn.csv", stringsAsFactors = F)
str(cape_columbine_tmn)

# Converting the inmported file into time series data - this makes plotting a lot easier
# Not too sure which format is easier to calculate correlations between data points
# I do not think that we will be analysing the change in trends and so won't need time series
tmn <- ts(cape_columbine_tmn$tmn, start = 1959, frequency = 12) 
plot(tmn)

