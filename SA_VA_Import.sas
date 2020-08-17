PROC IMPORT OUT= SA_DATA.SA_VA 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\SA_agr_
value_added__annual_growth_percentage.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
