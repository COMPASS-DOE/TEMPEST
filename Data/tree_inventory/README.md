# tree_inventory

This folder contains the `inventory.csv` file that records tree inventory
data for the TEMPEST plots. Its fields include:

Name          | Description
------------- | ----------------------------------------
Plot          | Plot name (string, Control/Fresh/Salt)
Section       | Plot section (string, East/Center/West)
Tag           | Tag number (integer)
Species_code  | Species code (genus + species; https://plants.usda.gov/)
DBH_{year}    | Diameter at 1.4 m in {year}, cm (numeric)
Date_{year}   | Date inventory performed in {year} (string, YYYY-MM-DD)
Status_{year} | ForestGEO status code in {year} (string, LI = living; DS = dead, stem standing; DC = dead, stem fallen)
Sapflux_ID    | If sapflux tree, noted here (string)
In_Plot       | For historical reasons, some trees are out of plot (string, TRUE/FALSE)
Notes         | Notes
