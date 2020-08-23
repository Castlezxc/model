goptions reset=all i=join;
axis1 label=(angle=90 'Annual % growth');
axis2 label=('Year') order = 1961 to 2019 by 5;
symbol1 color=black;
title1 'SA agricultural value added';

proc gplot data=SA_DATA.SA_VA;
	plot agr_va*Year / vaxis=axis1 haxis=axis2;
run;

proc arima data=sa_data.sa_va;
	identify var=agr_va nlag=6 stationarity=(adf=(0,1,2,3));
run;
quit;
