goptions reset=all i=join;
axis1 label=(angle=90 '1000 mega tons');
axis2 label=('Year') order = 1960 to 2020 by 5;
symbol1 color=black;
title1 'SA annual corn production';

proc gplot data=SA_DATA.Corn_prod;
	plot mt1000*t / vaxis=axis1 haxis=axis2;
run;

proc arima data=sa_data.corn_prod;
	identify var=mt1000 nlag=6 stationarity=(adf=(0,1,2,3));
run;
