PROC IMPORT OUT= SA_DATA.wheat_prod 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\wheat_p
roduction_sa.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
