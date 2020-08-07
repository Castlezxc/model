library(tsbox)
library(tidyverse)
library(ggpubr)
library(reshape2)

bloem_pre_csv <- read.csv("Bloemfontein_total_rainfall.csv", stringsAsFactors = FALSE)
bloem_pre_v <- bloem_pre_csv[1:259, 2]
bloem_pre <- data.frame(bloem_pre_v)
colnames(bloem_pre) <- c("bloem.pre")
str(bloem_pre)

pre.site <- ts(bloem_pre, start = 1979, frequency = 12)
plot(pre.site)
str(pre.site)
bloem_pre_df <- ts_df(pre.site)
str(bloem_pre_df)

cru_pre_csv <- read.csv("CRU.sites.pre.csv", check.names=FALSE, stringsAsFactors = F)
cru_bloem_pre_csv <- t(cru_pre_csv[3, 2:length(cru_pre_csv)])
cru_bloem_pre <- data.frame(cru_bloem_pre_csv)
str(cru_bloem_pre)
row.names(cru_bloem_pre) <- c()
colnames(cru_bloem_pre) <- "cru.pre"

pre.cru <- ts(cru_bloem_pre, start = 1901, frequency = 12)
plot(pre.cru)
str(pre.cru)
cru_bloem_pre_df <- ts_df(pre.cru)
str(cru_bloem_pre_df)

corr_pre_df <- full_join(cru_bloem_pre_df, bloem_pre_df, by = "time")
str(corr_pre_df)
colnames(corr_pre_df) <- c("time", "cru", "site")

plot <- ggplot(data = corr_pre_df, aes(time, cru, col = "blue"))
plot + geom_line() + geom_line(aes(time, site, col = "red"))

corr_clean_pre_df <- na.omit(corr_pre_df)
colnames(corr_clean_pre_df) <- c("time", "cru", "bloem")
cor.test(corr_clean_pre_df$cru, corr_clean_pre_df$bloem)

ggscatter(corr_clean_pre_df, x = "cru", y = "bloem", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "cru", ylab = "bloem")
