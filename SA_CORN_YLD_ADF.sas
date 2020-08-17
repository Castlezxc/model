goptions reset=all i=join;
axis1 label=(angle=90 'Megaton per Hectar');
axis2 label=('Year') order = 1960 to 2020 by 5;
symbol1 color=black;
title1 'SA corn yield per year';

proc gplot data=SA_DATA.corn_yld;
	plot mt_ha*t / vaxis=axis1 haxis=axis2;
run;

proc arima data=sa_data.corn_yld;
	identify var=mt_ha nlag=6 stationarity=(adf=(0,1,2,3));
run;
