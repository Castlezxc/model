PROC IMPORT OUT= SA_DATA.corn_prod 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\corn_pr
oduction_sa.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
