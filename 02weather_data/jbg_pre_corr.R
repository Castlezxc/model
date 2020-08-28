library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

jbg_pre_csv <- read.csv("JBG_INT_totals_pre.csv", stringsAsFactors = FALSE)
jbg_pre_v <- jbg_pre_csv[1:259, 2]
jbg_pre <- data.frame(jbg_pre_v)
colnames(jbg_pre) <- c("jbg.pre")
str(jbg_pre)

pre.site <- ts(jbg_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
jbg_pre_df <- ts_df(pre.site)
str(jbg_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_jbg_pre_v <- t(cru_pre_csv[4, 2:length(cru_pre_csv)])
str(cru_jbg_pre_v)
cru_jbg_pre <- data.frame(cru_jbg_pre_v)
str(cru_jbg_pre)
row.names(cru_jbg_pre) <- c()
colnames(cru_jbg_pre) <- "cru.pre"

pre.cru <- ts(cru_jbg_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_jbg_pre_df <- ts_df(pre.cru)
str(cru_jbg_pre_df)

corr_pre_df <- full_join(cru_jbg_pre_df, jbg_pre_df, by = "time")
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
          xlab = "CRU precipitation", ylab = "Site rainfall")