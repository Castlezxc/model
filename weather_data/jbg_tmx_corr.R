library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

jbg_tmx_csv <- read.csv("JBG_INT_means_tmx.csv", stringsAsFactors = FALSE)
jbg_tmx_v <- jbg_tmx_csv[1:259, 2]
jbg_tmx <- data.frame(jbg_tmx_v)
colnames(jbg_tmx) <- c("jbg.tmx")
str(jbg_tmx)

tmx.site <- ts(jbg_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
jbg_tmx_df <- ts_df(tmx.site)
str(jbg_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_jbg_tmx_v <- t(cru_tmx_csv[4, 2:length(cru_tmx_csv)])
str(cru_jbg_tmx_v)
cru_jbg_tmx <- data.frame(cru_jbg_tmx_v)
str(cru_jbg_tmx)
row.names(cru_jbg_tmx) <- c()
colnames(cru_jbg_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_jbg_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_jbg_tmx_df <- ts_df(tmx.cru)
str(cru_jbg_tmx_df)

corr_tmx_df <- full_join(cru_jbg_tmx_df, jbg_tmx_df, by = "time")
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