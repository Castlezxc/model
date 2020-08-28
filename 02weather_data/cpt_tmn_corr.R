library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

cpt_tmn_csv <- read.csv("CAPE_TOWN_INT_AIRPORT_means_tmn.csv", stringsAsFactors = FALSE)
cpt_tmn_v <- cpt_tmn_csv[1:nrow(cpt_tmn_csv) - 1, 2]
cpt_tmn <- data.frame(cpt_tmn_v)
colnames(cpt_tmn) <- c("cpt.tmn")
str(cpt_tmn)

tmn.site <- ts(cpt_tmn, start = 1979, frequency = 12)
plot(tmn.site)
str(tmn.site)
cpt_tmn_df <- ts_df(tmn.site)
str(cpt_tmn_df)

cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)
cru_cpt_tmn_v <- t(cru_tmn_csv[2, 2:length(cru_tmn_csv)])
str(cru_cpt_tmn_v)
cru_cpt_tmn <- data.frame(cru_cpt_tmn_v)
str(cru_cpt_tmn)
row.names(cru_cpt_tmn) <- c()
colnames(cru_cpt_tmn) <- "cru.tmn"

tmn.cru <- ts(cru_cpt_tmn, start = 1901, frequency = 12)
plot(tmn.cru)
str(tmn.cru)
cru_cpt_tmn_df <- ts_df(tmn.cru)
str(cru_cpt_tmn_df)

corr_tmn_df <- full_join(cru_cpt_tmn_df, cpt_tmn_df, by = "time")
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
          xlab = "CRU", ylab = "Site")