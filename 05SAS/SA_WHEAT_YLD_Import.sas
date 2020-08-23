PROC IMPORT OUT= SA_DATA.WHEAT_YLD 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\wheat_y
ield_sa.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
