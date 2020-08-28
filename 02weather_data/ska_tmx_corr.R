library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

ska_tmx_csv <- read.csv("Skakuza_means_tmx.csv", stringsAsFactors = FALSE)
ska_tmx_v <- ska_tmx_csv[1:497, 2]
ska_tmx <- data.frame(ska_tmx_v)
colnames(ska_tmx) <- c("ska.tmx")
str(ska_tmx)

tmx.site <- ts(ska_tmx, start = 1960, frequency = 12)
plot(tmx.site)
str(tmx.site)
ska_tmx_df <- ts_df(tmx.site)
str(ska_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_ska_tmx_v <- t(cru_tmx_csv[4, 2:length(cru_tmx_csv)])
str(cru_ska_tmx_v)
cru_ska_tmx <- data.frame(cru_ska_tmx_v)
str(cru_ska_tmx)
row.names(cru_ska_tmx) <- c()
colnames(cru_ska_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_ska_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_ska_tmx_df <- ts_df(tmx.cru)
str(cru_ska_tmx_df)

corr_tmx_df <- full_join(cru_ska_tmx_df, ska_tmx_df, by = "time")
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
          xlab = "CRU", ylab = "Site")