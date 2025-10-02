# fleetR: Airline Fleet Explorer
What do you get when you combine a lot of enthusiasm about planes, a posit::conf workshop about R in Production, evidently too much free time, and a working knowledge of R? You get [fleetR - a dashboard for exploring airline fleets!](https://mstavro.github.io/fleetR/) In this submission to Posit's 2025 Table Contest, I'm visualizing United's fleet in tables served primarily two ways:

1. Tables hidden inside of a map, using gt, mapgl, and some bslib elements

![fleetR_1](https://github.com/user-attachments/assets/af641aad-7989-4e36-a2dd-1e81fd3eda63)

2. A table of tables (of sorts), featuring gt, gtExtras, and bslib

![fleetR_2](https://github.com/user-attachments/assets/c11c4d79-b515-4747-8e54-b41212114afa)

I haven't really seen a lot of people showcasing the power of gt tables in mapping packages like mapgl and leaflet (maybe I'm looking in the wrong spot), so this seemed like the perfect opportunity to do so!

My Quarto doc leverages several APIs behind the scenes alongside some open-source information maintained by enthusiasts to deliver the data in the tables shown on the map. Given OpenSky API limits, I'm limited to querying about 30 aircraft every hour, and what gets displayed on the map are the aircraft from the hourly random sample (on the bright side, the API is readily accessible to anyone who wants to recreate the project). The same functions are used to deliver data to the full fleet table.

But wait - there's more! I've also written up [a handy Quarto book](https://mstavro.github.io/fleetR-docs/) to go along with my submission, so that anyone interested in visualizing some other fleets, say, the Lufthansa or Delta fleets, can fork my repo and make changes as they follow along.

The dashboard's hosted here, and rebuilt with a GitHub Action roughly every hour or so (depending on how eager the runner's feeling):
https://mstavro.github.io/fleetR/

The Quarto book for those looking to read more on how to code this up (and maybe learn about my creative musings) is here:
https://mstavro.github.io/fleetR-docs/

... and the repo containing the reprex is here:
https://github.com/mstavro/fleetR
