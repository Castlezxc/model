library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

ska_pre_csv <- read.csv("Skakuza_total_pre.csv", stringsAsFactors = FALSE)
ska_pre_v <- ska_pre_csv[1:1077, 2]
ska_pre <- data.frame(ska_pre_v)
colnames(ska_pre) <- c("ska.pre")
str(ska_pre)

pre.site <- ts(ska_pre, start = 1911, frequency = 12)
plot(pre.site)
str(pre.site)
ska_pre_df <- ts_df(pre.site)
str(ska_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_ska_pre_v <- t(cru_pre_csv[4, 2:length(cru_pre_csv)])
str(cru_ska_pre_v)
cru_ska_pre <- data.frame(cru_ska_pre_v)
str(cru_ska_pre)
row.names(cru_ska_pre) <- c()
colnames(cru_ska_pre) <- "cru.pre"

pre.cru <- ts(cru_ska_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_ska_pre_df <- ts_df(pre.cru)
str(cru_ska_pre_df)

corr_pre_df <- full_join(cru_ska_pre_df, ska_pre_df, by = "time")
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