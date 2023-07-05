# climheat_meteo
Support with meteorologic data heating/cooling load calculations.

Stations records are extracted for Global Historical Climate Network (GHCN) which contains harmonised observation data from meteorological stations around the world.

The procedure relays on using `rnoaa` package which automates and facilitates data loading from GHCN servers. Stations ids are needed for meaningful data extraction.

A data processing routine includes the following steps:

1. Loading GHCN data fron a server with `1_load_observations.R`. Normally, this step is needed only once for a region in question at a given moment, while it can be beneficial to re-run it to fetch the latest GHCN updates from time to time.

2. Filter observations according to the desired quality criteria with `2_filter_observations.R`. That is a step which implies trade-off between data availability and quality. Filtering can be tailored for specifical problem under consideration. Currently the idea is to have only stations with all the available years have an appropriate amount of missed data.

3. Evaluate HDDs/CDDs using GHCN-extracted observations records and compare the result gainst official Eurostat data.