library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

cpt_tmx_csv <- read.csv("CAPE_TOWN_INT_AIRPORT_means_tmx.csv", stringsAsFactors = FALSE)
cpt_tmx_v <- cpt_tmx_csv[1:nrow(cpt_tmx_csv) - 1, 2]
cpt_tmx <- data.frame(cpt_tmx_v)
colnames(cpt_tmx) <- c("cpt.tmx")
str(cpt_tmx)

tmx.site <- ts(cpt_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
cpt_tmx_df <- ts_df(tmx.site)
str(cpt_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_cpt_tmx_v <- t(cru_tmx_csv[2, 2:length(cru_tmx_csv)])
str(cru_cpt_tmx_v)
cru_cpt_tmx <- data.frame(cru_cpt_tmx_v)
str(cru_cpt_tmx)
row.names(cru_cpt_tmx) <- c()
colnames(cru_cpt_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_cpt_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_cpt_tmx_df <- ts_df(tmx.cru)
str(cru_cpt_tmx_df)

corr_tmx_df <- full_join(cru_cpt_tmx_df, cpt_tmx_df, by = "time")
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