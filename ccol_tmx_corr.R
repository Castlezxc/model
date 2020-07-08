#install.packages("plyr")
#install.packages("tsbox")
#install.packages("ggpubr")
library(tsbox)
library(tidyverse)
library(ggpubr)

# Contains the same code as ccol_tmn_corr.R except without the str() functions and comments

ccol_tmx_csv <- read.csv("CAPE_COLUMBINE.means.tmx.csv", stringsAsFactors = FALSE)
ccol_tmx_csv <- ccol_tmx_csv[2:nrow(ccol_tmx_csv) - 1, 2]
tmx.site <- ts(ccol_tmx_csv, start = 1959, frequency = 12)
ccol_tmx_df <- ts_df(tmx.site)
cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_ccol_tmx <- t(cru_tmx_csv[1, 2:length(cru_tmx_csv)])
colnames(cru_ccol_tmx) <- "tmx"
tmx.cru <- ts(cru_ccol_tmx, start = 1901, frequency = 12)
cru_ccol_tmx_df <- ts_df(tmx.cru)
corr_tmx_df <- full_join(cru_ccol_tmx_df, ccol_tmx_df, by = "time")
str(corr_tmx_df)
corr_clean_tmx_df <- na.omit(corr_tmx_df)
colnames(corr_clean_tmx_df) <- c("time", "cru", "ccol")
cor.test(corr_clean_tmx_df$cru, corr_clean_tmx_df$ccol)

ggscatter(corr_clean_tmx_df, x = "cru", y = "ccol", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "ccol")
