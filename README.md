# Supplementary materials for the paper _Connected bikeability in London: which localities are better connected by bike and does this matter?_.


_Roger&nbsp;Beecham ([r.j.beecham@leeds.ac.uk](mailto:r.j.beecham@leeds.ac.uk))_<br>
_Yuanxuan&nbsp;Yang ([y.yang6@leeds.ac.uk](mailto:y.yang6@leeds.ac.uk))_<br>
_Caroline&nbsp;Tait ([ugm4cjt@leeds.ac.uk](mailto:ugm4cjt@leeds.ac.uk))_<br>
_Robin&nbsp;Lovelace ([r.lovelace@leeds.ac.uk](mailto:r.lovelace@leeds.ac.uk))_


This repository contains supplementary materials for:

Beecham, R., Yang, Y., Tait, C. and Lovelace, R. _Connected bikeability in London: which localities are better connected by bike and does this matter?_. DOI: [osf.io/gbfz8](https://osf.io/gbfz8).

## Repository contents

* [data-processing.md](data-processing.md): R code for collecting and processing data underpinning the connected bikeability index:
  + London bikeshare OD trip data [from TfL](https://cycling.data.tfl.gov.uk/)
  + [CycleStreets routes](https://www.cyclestreets.net/) (via batch requests)
  + Records from London's [Cycle Infrastructure Database](https://data.london.gov.uk/dataset/cycling-infrastructure-database) using the [CycleInfraLnd](https://github.com/PublicHealthDataGeek/CycleInfraLnd) package  
  + Ordnance Survey [Open Roads Data](https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads)
* [generate-index.md](generate-index.md): R code for extracting and transforming derived variables that form the bikeability index.
* [generate-villages.md](generate-villages.md): R code for generating and storing our customised bikeshare villages -- the spatial unit for analysis.
* [application.md](application.md): R code for the data analysis discussed in our index -- where geographic variation in connected bikeability is evaluated against commuting demand.  
