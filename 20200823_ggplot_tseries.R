library(ggpubr)

corn_yield <- read.csv("01agric_data/corn_yield_sa.csv")
wheat_yield <- read.csv("01agric_data/wheat_yield_sa.csv")
data <- read.csv('20200823_model_data.csv')
data$X <- c()

corn_yield$crop <- rep('maize', nrow(corn_yield))
wheat_yield$crop <- rep('wheat', nrow(wheat_yield))

crop <- data.frame(rbind(corn_yield, wheat_yield))


#Agricultural gdp
ggplot(data = data, aes(Year, va)) + geom_line(color='blue') + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(title = 'Annual Growth in Agricultural Value Added for South Africa', 
       y = 'Percent')
#Overall gdp
ggplot(data = data, aes(Year, gdp)) + geom_line(color='blue') + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(title = 'Annual Growth in GDP for South Africa', 
       y = 'Percent')
#Corn yield
ggplot(data = corn_yield, aes(year, mt.ha)) + geom_line(color='blue') + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(title = 'Maize Yield per Year for South Africa', 
       y = 'Megaton per hectar', x = "Year")
#Wheat yield
ggplot(data = corn_yield, aes(year, mt.ha)) + geom_line(color='blue') + 
  geom_hline(yintercept = 0, linetype = 'dotted') +
  labs(title = 'Wheat Yield per Year for South Africa', 
       y = 'Megaton per hectar', x = "Year")

ggplot(data = crop, aes(year, mt.ha, color = crop)) + geom_line() + 
  geom_hline(yintercept = 0, linetype = 'dotted') + 
  scale_color_manual(values=c('red','blue')) +
  labs(title = 'Maize & Corn Yield per Year for South Africa', 
       y = 'Megaton per hectar', x = "Year") 




