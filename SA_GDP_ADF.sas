goptions reset=all i=join;
axis1 label=(angle=90 'Annual % GDP growth');
axis2 minor=(number=9) order = 0 to 59 by 1;
symbol1 color=black;
title1 'SA annual % GDP growth';

proc gplot data=SA_GDP;
plot gdp_growth*t / vaxis=axis1 haxis=axis2;
run;

proc arima data=sa_gdp;
identify var=gdp_growth nlag=6 stationarity=(adf=(0,1,2,3));
run;
