Data processing and index
================

## Introduction

This document describes how data underpinning the bikeability index were
harvested and transformed.

Please cite:

Beecham, R., Yang, Y., Tait, C. and Lovelace, R. (2023) “Connected
bikeability in London: which localities are better connected by bike and
does this matter?”, *Environment & Planning B: Urban Analytics and City
Science*. DOI: [osf.io/gbfz8](https://osf.io/gbfz8).

## R scripts

Code for collecting and processing data on the four components on which
connected bikeability is based is organised into three scripts:

- [`tidy_cyclestreets_data.R`](/code/prepare-vars/tidy_cyclestreets_data.R):
  Processes CycleStreets routes.

- [`obtain_bikeability_vars_part1.R`](/code/prepare-vars/obtain_bikeability_vars_part1.R):
  Processes [CycleStreets routes](https://www.cyclestreets.net), data
  from the TfL [Cycling Infrastructure
  Database](https://data.london.gov.uk/dataset/cycling-infrastructure-database)
  and OS [Open
  Roads](https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads).

- [`obtain_bikeability_vars_part2.R`](/code/prepare-vars/obtain_bikeability_vars_part2.R):
  Relates point locations and infrastructure features with cycle routes
  – e.g. links [Cycling Infrastructure
  Database](https://data.london.gov.uk/dataset/cycling-infrastructure-database)
  and OS [Open
  Roads](https://www.ordnancesurvey.co.uk/business-government/products/open-map-roads)
  with [cyclstreets routes](https://www.cyclestreets.net).

Code for exploring and transforming components and summarising
additively to generate bikeability index:

- [`connected_bikeability.R`](/code/connected_bikeability.R).