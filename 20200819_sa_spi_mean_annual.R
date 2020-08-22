library(ggpubr)

spi_df                  <- read.csv("sa_spi.csv", stringsAsFactors = FALSE)
years                   <- 1901:2019
month                   <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                             "Aug", "Sep", "Oct", "Nov", "Dec")

spi_annual_1            <- sapply(years, function(x) mean(spi_df[grep(x, spi_df[ , 2]), 4]))
spi_annual_3            <- sapply(years, function(x) mean(spi_df[grep(x, spi_df[ , 2]), 5]))
spi_annual_6            <- sapply(years, function(x) mean(spi_df[grep(x, spi_df[ , 2]), 6]))
spi_annual_12           <- sapply(years, function(x) mean(spi_df[grep(x, spi_df[ , 2]), 7]))

spi_annual_df           <- data.frame(years, spi_annual_1, spi_annual_3, 
                                      spi_annual_6, spi_annual_12)
colnames(spi_annual_df) <- c('Years', 'spi_1', 'spi_3', 'spi_6', 'spi_12')
write.csv(spi_annual_df, file = 'sa_spi_annual.csv')

#plot(spi_df$Year, spi_df$SPI_12)
spi_annual_df <- read.csv("sa_spi_annual.csv")

plot                    <- ggplot(data = spi_annual_df, aes(Years, spi_1, fill = 'SPI_1')) 
plot + geom_col() + geom_col(aes(Years, spi_3, fill = 'SPI_3')) + 
  geom_col(aes(Years, spi_6, fill = 'SPI_6')) + geom_col(aes(Years, spi_12, fill = 'SPI_12'))

ggplot(spi_df, aes(x = Year, y = SPI_12, fill = (SPI_12 > 0))) +
geom_col() + scale_fill_manual(values = c("red", "blue"), labels = c("Negative", "Positive")) +
ggtitle("Annual average for 12-month SPI") +
labs(y="SPI value", x="Year", fill = 'SPI')

ds_spi1   <- c(length(which(spi_df[ , 4] < -1)), 
                length(which(spi_df[ , 4] < -2)),
                length(which(spi_df[ , 4] < -3)))
ds_spi3   <- c(length(which(spi_df[ , 5] < -1)), 
                length(which(spi_df[ , 5] < -2)),
                 length(which(spi_df[ , 5] < -3)))
ds_spi6   <- c(length(which(spi_df[ , 6] < -1)), 
                length(which(spi_df[ , 6] < -2)),
                length(which(spi_df[ , 6] < -3)))
ds_spi12  <- c(length(which(spi_df[ , 7] < -1)), 
                length(which(spi_df[ , 7] < -2)),
                length(which(spi_df[ , 7] < -3)))


ds_count <- data.frame(ds_spi1, ds_spi3, ds_spi6, ds_spi12, 
                               row.names = c('Moderately dry', 'Severely dry', 'Extremely dry'))
names(ds_count) <- c('spi_1', 'spi_3', 'spi_6', 'spi_12')
#write.csv(drought_severity_count, file = "count_spi_scale.csv")

spi_1961                <- spi_df[which(spi_df$Year > 1960), 2:7]

ds_spi1_61              <- c(length(which(spi_1961$SPI_1 < -1)), 
                            length(which(spi_1961$SPI_1 < -2)),
                            length(which(spi_1961$SPI_1 < -3)))
ds_spi3_61              <- c(length(which(spi_1961$SPI_3 < -1)), 
                            length(which(spi_1961$SPI_3 < -2)),
                            length(which(spi_1961$SPI_3 < -3)))
ds_spi6_61              <- c(length(which(spi_1961$SPI_6 < -1)), 
                            length(which(spi_1961$SPI_6 < -2)),
                            length(which(spi_1961$SPI_6 < -3)))
ds_spi12_61             <- c(length(which(spi_1961$SPI_12 < -1)), 
                            length(which(spi_1961$SPI_12 < -2)),
                            length(which(spi_1961$SPI_12 < -3)))

ds_count_1661 <- data.frame(ds_spi1_61, ds_spi3_61, ds_spi6_61, ds_spi12_61, 
                            row.names = c('Moderately dry', 'Severely dry', 'Extremely dry'))
names(ds_count_1661) <- c('spi_1', 'spi_3', 'spi_6', 'spi_12')

measure <- c('spi_1', 'spi_3', 'spi_6', 'spi_12')
severity <- c('moderate', 'severe', 'extreme')
period <- c('full', 'recent')

plot_df <- data.frame(c(rep("1-month", 4), rep("3-month", 4), rep("6-month", 4), rep("12-month", 4)))
names(plot_df) <- c('measure')
plot_df$measure <- factor(plot_df$measure, levels = c("1-month", "3-month", "6-month", "12-month"))
plot_df$period <- rep(c(rep('full', 2), rep('recent', 2)), 4)
plot_df$severity <- rep(c('moderate', 'severe'), 8)
plot_df$count <- c(ds_count$spi_1[1:2], ds_count_1661$spi_1[1:2], ds_count$spi_3[1:2],
                   ds_count_1661$spi_3[1:2], ds_count$spi_6[1:2],  ds_count_1661$spi_6[1:2],
                   ds_count$spi_12[1:2], ds_count_1661$spi_12[1:2])

moderatecount <- ggplot(plot_df[which(plot_df$severity == 'moderate'), ], 
                        aes(measure, count, fill = period)) +
                 geom_col() #+ scale_color_manual(values =c('red4', 'black')) + scale_fill_manual()

severecount <- ggplot(plot_df, aes(measure, count)) +  
               geom_col(data=plot_df[which(plot_df$severity == 'severe'), ], 
                        fill = period, position = 'dodge') + 
               #geom_col(data=plot_df[which((plot_df$severity == 'severe') & 
               #(plot_df$period == 'recent')), ], fill = 'blue') +
               labs(title = 'Comparison of the number of drought events per SPI measure over time'
                    ,fill = 'Period', y = 'Number of events in period', x = 'SPI measure') +
               scale_x_discrete(labels=c("1-month", "3-month", "6-month", "12-month")) +
               scale_fill_discrete(name = "Period", labels = c("1901 - 2020", "1961 - 2020"))
               
severecount

ggplot(data=plot_df[which(plot_df$severity == 'severe'), ], aes(measure, count, fill = period)) +
geom_bar(stat = 'identity', position = 'dodge') + 
labs(title = 'Comparison of the number of severe drought events' , 
     y = 'Number of events in period', x = 'SPI measure', 
     labels=c("1-month", "3-month", "6-month", "12-month")) +
scale_fill_manual(name = "Period", labels = c("1901 - 2020", "1961 - 2020"), 
                  values =  c("red", "blue"))

