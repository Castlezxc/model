library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

dbn_tmx_csv <- read.csv("DBN_INT_means_tmx.csv", stringsAsFactors = FALSE)
dbn_tmx_v <- dbn_tmx_csv[1:259, 2]
dbn_tmx <- data.frame(dbn_tmx_v)
colnames(dbn_tmx) <- c("dbn.tmx")
str(dbn_tmx)

tmx.site <- ts(dbn_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
dbn_tmx_df <- ts_df(tmx.site)
str(dbn_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_dbn_tmx_v <- t(cru_tmx_csv[4, 2:length(cru_tmx_csv)])
str(cru_dbn_tmx_v)
cru_dbn_tmx <- data.frame(cru_dbn_tmx_v)
str(cru_dbn_tmx)
row.names(cru_dbn_tmx) <- c()
colnames(cru_dbn_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_dbn_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_dbn_tmx_df <- ts_df(tmx.cru)
str(cru_dbn_tmx_df)

corr_tmx_df <- full_join(cru_dbn_tmx_df, dbn_tmx_df, by = "time")
str(corr_tmx_df)
colnames(corr_tmx_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_tmx_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_tmx_df <- na.omit(corr_tmx_df)
row.names(corr_clean_tmx_df) <- c()

cor.test(corr_clean_tmx_df$cru, corr_clean_tmx_df$site)

ggscatter(corr_clean_tmx_df, x = "cru", y = "site", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "site")