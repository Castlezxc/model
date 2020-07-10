library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

vnd_pre_csv <- read.csv("VREDENDAL.totals.pre.csv", stringsAsFactors = FALSE)
vnd_pre_v <- vnd_pre_csv[1:nrow(vnd_pre_csv) - 1, 2]
vnd_pre <- data.frame(vnd_pre_v)
colnames(vnd_pre) <- c("vnd.pre")
str(vnd_pre)

pre.site <- ts(vnd_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
vnd_pre_df <- ts_df(pre.site)
str(vnd_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_vnd_pre_v <- t(cru_pre_csv[2, 2:length(cru_pre_csv)])
str(cru_vnd_pre_v)
cru_vnd_pre <- data.frame(cru_vnd_pre_v)
str(cru_vnd_pre)
row.names(cru_vnd_pre) <- c()
colnames(cru_vnd_pre) <- "cru.pre"

pre.cru <- ts(cru_vnd_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_vnd_pre_df <- ts_df(pre.cru)
str(cru_vnd_pre_df)

corr_pre_df <- full_join(cru_vnd_pre_df, vnd_pre_df, by = "time")
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