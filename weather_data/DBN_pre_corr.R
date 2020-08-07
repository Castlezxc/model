library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

dbn_pre_csv <- read.csv("DBN_INT_totals_pre.csv", stringsAsFactors = FALSE)
dbn_pre_v <- dbn_pre_csv[1:259, 2]
dbn_pre <- data.frame(dbn_pre_v)
colnames(dbn_pre) <- c("dbn.pre")
str(dbn_pre)

pre.site <- ts(dbn_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
dbn_pre_df <- ts_df(pre.site)
str(dbn_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_dbn_pre_v <- t(cru_pre_csv[4, 2:length(cru_pre_csv)])
str(cru_dbn_pre_v)
cru_dbn_pre <- data.frame(cru_dbn_pre_v)
str(cru_dbn_pre)
row.names(cru_dbn_pre) <- c()
colnames(cru_dbn_pre) <- "cru.pre"

pre.cru <- ts(cru_dbn_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_dbn_pre_df <- ts_df(pre.cru)
str(cru_dbn_pre_df)

corr_pre_df <- full_join(cru_dbn_pre_df, dbn_pre_df, by = "time")
str(corr_pre_df)
colnames(corr_pre_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_pre_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_pre_df <- na.omit(corr_pre_df)
row.names(corr_clean_pre_df) <- c()

cor.test(corr_clean_pre_df$cru, corr_clean_pre_df$site)

ggscatter(corr_clean_pre_df, x = "cru", y = "site", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "site")
