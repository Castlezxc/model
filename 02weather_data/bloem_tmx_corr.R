library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

bloem_tmx_csv <- read.csv("Bloemfontein_mean_tmx.csv", stringsAsFactors = FALSE)
bloem_tmx_v <- bloem_tmx_csv[1:259, 2]
bloem_tmx <- data.frame(bloem_tmx_v)
colnames(bloem_tmx) <- c("bloem.tmx")
str(bloem_tmx)

tmx.site <- ts(bloem_tmx, start = 1979, frequency = 12)
plot(tmx.site)
str(tmx.site)
bloem_tmx_df <- ts_df(tmx.site)
str(bloem_tmx_df)

cru_tmx_csv <- read.csv("CRU.sites.tmx.csv", check.names=FALSE, stringsAsFactors = F)
cru_bloem_tmx_csv <- t(cru_tmx_csv[3, 2:length(cru_tmx_csv)])
cru_bloem_tmx <- data.frame(cru_bloem_tmx_csv)
str(cru_bloem_tmx)
row.names(cru_bloem_tmx) <- c()
colnames(cru_bloem_tmx) <- "cru.tmx"

tmx.cru <- ts(cru_bloem_tmx, start = 1901, frequency = 12)
plot(tmx.cru)
str(tmx.cru)
cru_bloem_tmx_df <- ts_df(tmx.cru)
str(cru_bloem_tmx_df)

corr_tmx_df <- full_join(cru_bloem_tmx_df, bloem_tmx_df, by = "time")
str(corr_tmx_df)
colnames(corr_tmx_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_tmx_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_tmx_df <- na.omit(corr_tmx_df)
colnames(corr_clean_tmx_df) <- c("time", "cru", "bloem")
cor.test(corr_clean_tmx_df$cru, corr_clean_tmx_df$bloem)

ggscatter(corr_clean_tmx_df, x = "cru", y = "bloem", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CRU", ylab = "Site")
