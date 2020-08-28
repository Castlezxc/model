library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

dbn_tmn_csv <- read.csv("DBN_INT_means_tmn.csv", stringsAsFactors = FALSE)
dbn_tmn_v <- dbn_tmn_csv[1:259, 2]
dbn_tmn <- data.frame(dbn_tmn_v)
colnames(dbn_tmn) <- c("dbn.tmn")
str(dbn_tmn)

tmn.site <- ts(dbn_tmn, start = 1979, frequency = 12)
plot(tmn.site)
str(tmn.site)
dbn_tmn_df <- ts_df(tmn.site)
str(dbn_tmn_df)

cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)
cru_dbn_tmn_v <- t(cru_tmn_csv[4, 2:length(cru_tmn_csv)])
str(cru_dbn_tmn_v)
cru_dbn_tmn <- data.frame(cru_dbn_tmn_v)
str(cru_dbn_tmn)
row.names(cru_dbn_tmn) <- c()
colnames(cru_dbn_tmn) <- "cru.tmn"

tmn.cru <- ts(cru_dbn_tmn, start = 1901, frequency = 12)
plot(tmn.cru)
str(tmn.cru)
cru_dbn_tmn_df <- ts_df(tmn.cru)
str(cru_dbn_tmn_df)

corr_tmn_df <- full_join(cru_dbn_tmn_df, dbn_tmn_df, by = "time")
str(corr_tmn_df)
colnames(corr_tmn_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_tmn_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_tmn_df <- na.omit(corr_tmn_df)
row.names(corr_clean_tmn_df) <- c()

cor.test(corr_clean_tmn_df$cru, corr_clean_tmn_df$site)

ggscatter(corr_clean_tmn_df, x = "cru", y = "site", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CRU", ylab = "Site")