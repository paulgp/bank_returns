

* Data Steps 

1. CRSP-FRB LINK
First, pull data from CRSP-FRB LINK available here: https://www.newyorkfed.org/research/banking_research/datasets.html

This gives all PERMCOs to link to RSSDs. 

`permco_rssdid_xwalk.csv` -> `permco.txt`

2. WRDS Compustat data

From WRDS, we pull data on all PERMCOs in the data. This gives us ticker information for every bank

`ticker_list.csv` -> `ticker_list.txt`

3. Stock price data comes from Yahoo! Finance

4. FRY9-C data is pulled for each ticker using the RSSDID of the bhc

data from `BHCF20211231.txt` using rssdids from `permco_rssdid_xwalk.csv`

5. FDIC Cert-No comes from identifying all the banks held by the parent holding company in the FRY-9C data. 

To do this, it requires three steps:

a. Loop thorugh every child of the BHC and identify the RSSDID assocaited with the commercial banks

for each rssdid  in `permco_rssdid_xwalk.csv` 
 find all children with rssdid in `CSV_RELATIONSHIPS.csv`

b. Pull the FDIC Cert-No for this bank using the call reports
 for each child's rssd, find their FDIC cert NUM in `FFIEC CDR Call Bulk All Schedules 12312022`

c. With the Cert-NUM, pull the deposit data from the FDIC website
with each child's cert num, pull deposit data and then aggregate to parent


