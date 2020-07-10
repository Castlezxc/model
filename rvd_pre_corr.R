library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

rvd_pre_csv <- read.csv("RIVERSDALE.totals.pre.csv", stringsAsFactors = FALSE)
rvd_pre_v <- rvd_pre_csv[1:nrow(rvd_pre_csv) - 1, 2]
rvd_pre <- data.frame(rvd_pre_v)
colnames(rvd_pre) <- c("rvd.pre")
str(rvd_pre)

pre.site <- ts(rvd_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
rvd_pre_df <- ts_df(pre.site)
str(rvd_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_rvd_pre_v <- t(cru_pre_csv[2, 2:length(cru_pre_csv)])
str(cru_rvd_pre_v)
cru_rvd_pre <- data.frame(cru_rvd_pre_v)
str(cru_rvd_pre)
row.names(cru_rvd_pre) <- c()
colnames(cru_rvd_pre) <- "cru.pre"

pre.cru <- ts(cru_rvd_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_rvd_pre_df <- ts_df(pre.cru)
str(cru_rvd_pre_df)

corr_pre_df <- full_join(cru_rvd_pre_df, rvd_pre_df, by = "time")
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