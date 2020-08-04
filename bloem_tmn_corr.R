library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

bloem_tmn_csv <- read.csv("Bloemfontein_Airport_mean_tmn.csv", stringsAsFactors = FALSE)
bloem_tmn_v <- bloem_tmn_csv[1:259, 2]
bloem_tmn <- data.frame(bloem_tmn_v)
colnames(bloem_tmn) <- c("bloem.tmn")
str(bloem_tmn)

tmn.site <- ts(bloem_tmn, start = 1979, frequency = 12)
plot(tmn.site)
str(tmn.site)
bloem_tmn_df <- ts_df(tmn.site)
str(bloem_tmn_df)

cru_tmn_csv <- read.csv("CRU.sites.tmn.csv", check.names=FALSE, stringsAsFactors = F)
cru_bloem_tmn_csv <- t(cru_tmn_csv[3, 2:length(cru_tmn_csv)])
cru_bloem_tmn <- data.frame(cru_bloem_tmn_csv)
str(cru_bloem_tmn)
row.names(cru_bloem_tmn) <- c()
colnames(cru_bloem_tmn) <- "cru.tmn"

tmn.cru <- ts(cru_bloem_tmn, start = 1901, frequency = 12)
plot(tmn.cru)
str(tmn.cru)
cru_bloem_tmn_df <- ts_df(tmn.cru)
str(cru_bloem_tmn_df)

corr_tmn_df <- full_join(cru_bloem_tmn_df, bloem_tmn_df, by = "time")
str(corr_tmn_df)
colnames(corr_tmn_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_tmn_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_tmn_df <- na.omit(corr_tmn_df)
colnames(corr_clean_tmn_df) <- c("time", "cru", "bloem")
cor.test(corr_clean_tmn_df$cru, corr_clean_tmn_df$bloem)

ggscatter(corr_clean_tmn_df, x = "cru", y = "bloem", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "bloem")
