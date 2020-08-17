library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

ska_tmn_csv <- read.csv("Skakuza_means_tmn.csv", stringsAsFactors = FALSE)
ska_tmn_v <- ska_tmn_csv[1:497, 2]
ska_tmn <- data.frame(ska_tmn_v)
colnames(ska_tmn) <- c("ska.tmn")
str(ska_tmn)

tmn.site <- ts(ska_tmn, start = 1960, frequency = 12)
plot(tmn.site)
str(tmn.site)
ska_tmn_df <- ts_df(tmn.site)
str(ska_tmn_df)

cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)
cru_ska_tmn_v <- t(cru_tmn_csv[4, 2:length(cru_tmn_csv)])
str(cru_ska_tmn_v)
cru_ska_tmn <- data.frame(cru_ska_tmn_v)
str(cru_ska_tmn)
row.names(cru_ska_tmn) <- c()
colnames(cru_ska_tmn) <- "cru.tmn"

tmn.cru <- ts(cru_ska_tmn, start = 1901, frequency = 12)
plot(tmn.cru)
str(tmn.cru)
cru_ska_tmn_df <- ts_df(tmn.cru)
str(cru_ska_tmn_df)

corr_tmn_df <- full_join(cru_ska_tmn_df, ska_tmn_df, by = "time")
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
          xlab = "cru", ylab = "site")