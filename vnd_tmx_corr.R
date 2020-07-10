library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

vnd_tmx_csv <- read.csv("VREDENDAL.means.tmx.csv", stringsAsFactors = FALSE)
vnd_tmx_v <- vnd_tmx_csv[1:nrow(vnd_tmx_csv) - 1, 2]
vnd_tmx <- data.frame(vnd_tmx_v)
colnames(vnd_tmx) <- c("vnd.tmx")
str(vnd_tmx)

tmx.site <- ts(vnd_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
vnd_tmx_df <- ts_df(tmx.site)
str(vnd_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_vnd_tmx_v <- t(cru_tmx_csv[2, 2:length(cru_tmx_csv)])
str(cru_vnd_tmx_v)
cru_vnd_tmx <- data.frame(cru_vnd_tmx_v)
str(cru_vnd_tmx)
row.names(cru_vnd_tmx) <- c()
colnames(cru_vnd_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_vnd_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_vnd_tmx_df <- ts_df(tmx.cru)
str(cru_vnd_tmx_df)

corr_tmx_df <- full_join(cru_vnd_tmx_df, vnd_tmx_df, by = "time")
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