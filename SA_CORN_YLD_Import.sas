PROC IMPORT OUT= SA_DATA.CORN_YLD 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\corn_yi
eld_sa.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
