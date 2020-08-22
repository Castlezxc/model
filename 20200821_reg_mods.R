library(ggpubr)
spi_annual <- read.csv('sa_spi_annual.csv', stringsAsFactors = F)
str(spi_annual)
names(spi_annual) <- c('t', 'Year', 'spi_1', 'spi_3', 'spi_6', 'spi_12')
agric_va <- read.csv('01agric_data/SA_agr_value_added__annual_growth_percentage.csv', stringsAsFactors = FALSE)
str(agric_va)
names(agric_va) <- c('Year', 'va', 't')
gdp <- read.csv("01agric_data/SA_GDP.csv", stringsAsFactors = F)

merged <- data.frame(spi_annual[which(spi_annual$Year > 1960), ], agric_va$va)
names(merged) <- c('t', 'Year', 'spi_1', 'spi_3', 'spi_6', 'spi_12', 'va')
row.names(merged) <- c()
merged$t <- c(1:59)
merged$gdp <- gdp$gdp_growth

model_a1 <- lm(va ~ spi_1, merged)
model_a3 <- lm(va ~ spi_3, merged)
model_a6 <- lm(va ~ spi_6, merged)
model_a12 <- lm(va ~ spi_12, merged)
model_a12_t <- lm(va ~ t + spi_12, merged)

anova(model_a12, model_a12_t)

summary(model_a1)
summary(model_a3)
summary(model_a6)
summary(model_a12)

plot(spi_12, va)
abline(model_12)

model_g1  <- lm(gdp ~ t + spi_1, merged)
model_g3  <- lm(gdp ~ t + spi_3, merged)
model_g6  <- lm(gdp ~ t + spi_6, merged)
model_g12 <- lm(gdp ~ t, merged)
model_g12_t <- lm(gdp ~ t + spi_12, merged)

anova(model_g12, model_g12_t)

summary(model_g1)
summary(model_g3)
summary(model_g6)
summary(model_g12)

plot(merged$spi_12, merged$gdp)
abline(model_g12)
