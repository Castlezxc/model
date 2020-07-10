#install.packages("plyr")
#install.packages("tsbox")
#install.packages("ggpubr")
#install.packages("reshape2")
library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

# Contains the same code as ccol_tmn_corr.R except without the str() functions and comments

ccol_pre_csv <- read.csv("CAPE_COLUMBINE.totals.pre.csv", stringsAsFactors = FALSE)
ccol_pre_v <- ccol_pre_csv[2:nrow(ccol_pre_csv) - 1, 2]
ccol_pre <- data.frame(ccol_pre_v)
colnames(ccol_pre) <- c("ccol.pre")
str(ccol_pre)

pre.site <- ts(ccol_pre, start = 1959, frequency = 12)
plot(pre.site)
str(pre.site)
ccol_pre_df <- ts_df(pre.site)
str(ccol_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_ccol_pre_csv <- t(cru_pre_csv[1, 2:length(cru_pre_csv)])
cru_ccol_pre <- data.frame(cru_ccol_pre_csv)
str(cru_ccol_pre)
row.names(cru_ccol_pre) <- c()
colnames(cru_ccol_pre) <- "cru.pre"

pre.cru <- ts(cru_ccol_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_ccol_pre_df <- ts_df(pre.cru)
str(cru_ccol_pre_df)

corr_pre_df <- full_join(cru_ccol_pre_df, ccol_pre_df, by = "time")
str(corr_pre_df)
colnames(corr_pre_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_pre_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_pre_df <- na.omit(corr_pre_df)
colnames(corr_clean_pre_df) <- c("time", "cru", "ccol")
cor.test(corr_clean_pre_df$cru, corr_clean_pre_df$ccol)

ggscatter(corr_clean_pre_df, x = "cru", y = "ccol", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "ccol")
