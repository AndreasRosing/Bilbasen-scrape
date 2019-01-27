# Bilbasen-scrape

This repository contains two R scripts for scraping select cars from the Danish used-cars website www.bilbasen.dk. 

scraping_bilbasen.R - script containing only the scraping functionality. The first few lines are input to the main function scrape_bilbasen(url, output_name). The "url" variable is the url to the main page of a specific car (e.g. VW Passat), an example is shown in the input part called "Landing page - entry page". The "output_name" variable is the user defined name of the output tsv-file combined with a date stamp and the prefix "bilbasen_" (e.g. "bilbasen_vw_passat_2018-09-10.tsv"). NB! One has to specify the output directory in the very last line of code of the function. 

scrape_and_clean_bilbasen.R - script containing all of the script scraping_bilbasen.R with extra cars added. Furthermore, this script contains all of the cleaning steps taken in order to create analysis ready data sets.

Note that the cleaning process is specific to each car manufacturer, as an example stationcars have the addition "Variant"  to the names of VW cars and "Avant" for Audi's. Hence they must be tailored to the cars needed. The few cars where cleaning steps are taken belong to a group of interest to myself. However, the general process is the same for different manufacturers and it is fairly easily adaptable to other cars.

A short description of the variables targeted for scraping and the final variables created by the cleaning process is part of the analysis performed in the markdown document Bilbasen_algoritm_test.md in this repository.
