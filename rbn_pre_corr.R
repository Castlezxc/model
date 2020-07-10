library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

rbn_pre_csv <- read.csv("ROBERTSON.totals.pre.csv", stringsAsFactors = FALSE)
rbn_pre_v <- rbn_pre_csv[1:nrow(rbn_pre_csv) - 1, 2]
rbn_pre <- data.frame(rbn_pre_v)
colnames(rbn_pre) <- c("rbn.pre")
str(rbn_pre)

pre.site <- ts(rbn_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
rbn_pre_df <- ts_df(pre.site)
str(rbn_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_rbn_pre_v <- t(cru_pre_csv[2, 2:length(cru_pre_csv)])
str(cru_rbn_pre_v)
cru_rbn_pre <- data.frame(cru_rbn_pre_v)
str(cru_rbn_pre)
row.names(cru_rbn_pre) <- c()
colnames(cru_rbn_pre) <- "cru.pre"

pre.cru <- ts(cru_rbn_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_rbn_pre_df <- ts_df(pre.cru)
str(cru_rbn_pre_df)

corr_pre_df <- full_join(cru_rbn_pre_df, rbn_pre_df, by = "time")
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