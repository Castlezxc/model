library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

grg_tmx_csv <- read.csv("GEORGE_AIRPORT.means.tmx.csv", stringsAsFactors = FALSE)
grg_tmx_v <- grg_tmx_csv[1:nrow(grg_tmx_csv) - 1, 2]
grg_tmx <- data.frame(grg_tmx_v)
colnames(grg_tmx) <- c("grg.tmx")
str(grg_tmx)

tmx.site <- ts(grg_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
grg_tmx_df <- ts_df(tmx.site)
str(grg_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_grg_tmx_v <- t(cru_tmx_csv[2, 2:length(cru_tmx_csv)])
str(cru_grg_tmx_v)
cru_grg_tmx <- data.frame(cru_grg_tmx_v)
str(cru_grg_tmx)
row.names(cru_grg_tmx) <- c()
colnames(cru_grg_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_grg_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_grg_tmx_df <- ts_df(tmx.cru)
str(cru_grg_tmx_df)

corr_tmx_df <- full_join(cru_grg_tmx_df, grg_tmx_df, by = "time")
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