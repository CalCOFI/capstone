# Datasets

## /raw

`194903-202001_Bottle.zip` Bottle data (physical measurements) as downloaded from [CalCOFI database](https://wp.calcofi.org/wp/data/oceanographic-data/bottle-database/) January 2020.

`194903-202001_Cast.csv` Cast data (spatiotemporal and spatial information from each sampling event) as downloaded from [CalCOFI database](https://wp.calcofi.org/wp/data/oceanographic-data/bottle-database/) January 2020.

`cast_table.csv` Duplicate of `194903-202001_Cast.csv`.

`bottle-metadata.csv` Column descriptions for bottle data.

`cast-metadata.csv` Column descriptions for cast data.

## /processed

`bottle_and_cast.csv` Generated via python preprocessing scripts. Contains merged bottle and cast data from all casts 2000 or later.

`bottle-cast-recent.RData` Contains merged bottle and cast data from all casts 2000 or later. Generated via `data_preprocessing.R` script.
