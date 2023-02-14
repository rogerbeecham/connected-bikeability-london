# Supplementary materials for the paper _Connected bikeability in London: which localities are better connected by bike and does this matter?_


_Roger&nbsp;Beecham ([r.j.beecham@leeds.ac.uk](mailto:r.j.beecham@leeds.ac.uk))_<br>
_Yuanxuan&nbsp;Yang ([y.yang6@leeds.ac.uk](mailto:y.yang6@leeds.ac.uk))_<br>
_Caroline&nbsp;Tait ([ugm4cjt@leeds.ac.uk](mailto:ugm4cjt@leeds.ac.uk))_<br>
_Robin&nbsp;Lovelace ([r.lovelace@leeds.ac.uk](mailto:r.lovelace@leeds.ac.uk))_


This repository contains supplementary materials for:

Beecham, R., Yang, Y., Tait, C. and Lovelace, R. (2023) "Connected bikeability in London: which localities are better connected by bike and does this matter?", *Environment & Planning B: Urban Analytics and City Science*. DOI: [osf.io/gbfz8](https://osf.io/gbfz8).


## Repository contents

* [data-processing.md](data-processing.md): R code for collecting and processing data underpinning the connected bikeability index:
  + London bikeshare OD trip data [from TfL](https://cycling.data.tfl.gov.uk/)
  + [CycleStreets routes](https://www.cyclestreets.net/) (via batch requests)
  + Records from London's [Cycle Infrastructure Database](https://data.london.gov.uk/dataset/cycling-infrastructure-database) using the [CycleInfraLnd](https://github.com/PublicHealthDataGeek/CycleInfraLnd) package
  + Ordnance Survey [Open Roads Data](https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads)
* [generate-villages.md](generate-villages.md): R code for generating and storing customised bikeshare villages -- the spatial unit for analysis.
* [validate-scores.md](validate-scores.md): R code for exploring how bikeability varies with observed cycle trips and London's cycling infrastructure.
* [maup-analysis.md](maup-analysis.md): R code for generating hypothetical outcome plots to represent uncertainty from zoning effects when aggregating OD scores to bikeshare village level.
* [application.md](application.md): R code for the data analysis discussed in our index -- where geographic variation in connected bikeability is evaluated against commuting demand.

The code in the repo loads datasets that are too big to host via github. We have made these available separately via a shared dropbox folder. For a link to this folder contact Roger (r.j.beecham@leeds.ac.uk / @rjbeecham).
