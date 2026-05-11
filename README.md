# Økonomiske konsekvenser av EU ETS2 Masteroppgave

Dette repositoryet inneholder kode og data brukt i analysen av de økonomiske konsekvensene av ETS2 for det norske tungtransportmarkedet. 

Analysen fokuserer på:
- konkurranseforholdet mellom norske og utenlandske aktører
- i hvilken grad virkemiddelet kan bidra til omstilling mot elektriske lastebiler 

Arbeidet er gjennomført som en del av en masteroppgave i økonomi og administrasjon ved Norges miljø- og biovitenskapelige universitet (NMBU).

## Filstruktur 
- `Master.R` - Hovedscript som laster inn alle filer og kjører modellen
- `Functions.R` - Modellfunksjoner
- `Data.R` - Parametere og kalibrering
- `Scenarios.R` - Definisjon av scenarioer
- `Policy.R` - Beregning av endring i marginalkostnad som følge av politikkendring 
- `Solver.R` - Løsning av likevekt
- `Results.R` - Beregning og utskrift av resultater
- `Sensitivity.R` - Sensitivitetsanalyse

## Instruksjoner
Alle resultater i oppgaven kan reproduseres ved å
1. Åpne prosjektet i RStudio
2. Åpne `Master.R`
3. Kjøre hele scriptet med Source-knappen

## Kontaktinformasjon
- Johanne Bergflødt - johanne.marie.bergflodt@nmbu.no
- Marie Trømborg - marie.tromborg@nmbu.no
