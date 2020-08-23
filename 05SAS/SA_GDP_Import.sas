PROC IMPORT OUT= SA_DATA.SA_GDP 
            DATAFILE= "C:\Users\Kevin\Desktop\02model\agric_data\SA_GDP.
csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
