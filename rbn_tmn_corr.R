library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

rbn_tmn_csv <- read.csv("ROBERTSON.means.tmn.csv", stringsAsFactors = FALSE)
rbn_tmn_v <- rbn_tmn_csv[1:nrow(rbn_tmn_csv) - 1, 2]
rbn_tmn <- data.frame(rbn_tmn_v)
colnames(rbn_tmn) <- c("rbn.tmn")
str(rbn_tmn)

tmn.site <- ts(rbn_tmn, start = 1979, frequency = 12)
plot(tmn.site)
str(tmn.site)
rbn_tmn_df <- ts_df(tmn.site)
str(rbn_tmn_df)

cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)
cru_rbn_tmn_v <- t(cru_tmn_csv[2, 2:length(cru_tmn_csv)])
str(cru_rbn_tmn_v)
cru_rbn_tmn <- data.frame(cru_rbn_tmn_v)
str(cru_rbn_tmn)
row.names(cru_rbn_tmn) <- c()
colnames(cru_rbn_tmn) <- "cru.tmn"

tmn.cru <- ts(cru_rbn_tmn, start = 1901, frequency = 12)
plot(tmn.cru)
str(tmn.cru)
cru_rbn_tmn_df <- ts_df(tmn.cru)
str(cru_rbn_tmn_df)

corr_tmn_df <- full_join(cru_rbn_tmn_df, rbn_tmn_df, by = "time")
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