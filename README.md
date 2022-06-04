# CalCOFI22
Capstone project for CCDSP fellowship, where we will store data and code <br>
Link to CalCOFI website:  https://wp.calcofi.org/wp/data/oceanographic-data/bottle-database/ <br>
Data sets visual from Erin: <br>
![image](https://user-images.githubusercontent.com/30590837/149233121-d5e2e83e-b72a-41e5-9e83-00ef40877b43.png)
<br>
Bottle in the image refers to the bottle dataset on https://wp.calcofi.org/wp/data/oceanographic-data/bottle-database/

<br>
How casting is done: <br>

![image](https://user-images.githubusercontent.com/30590837/149233368-01275c98-ef42-46bf-8d36-31a8d14f5392.png)

#Directory setup
- **Data directory:** Raw data is obtained directly from the CalCOFI organizations bottle dataset. Processed data comes from running the files in the `scripts/data-preprocessing` directory. See `README.md` and `file-descriptions.md` ini the data directory for more information.
- **Results directory:** Plots and images used for presentations and posters for the CCDSP fellowship.
- **Scripts directory:** The `shiny` directory contains all of the code for the shiny application. `app.R` is the UI/UX component and `spatial-page/page_functions.R` is the backend functions for plots and mapping. The `.css`, `.js` and `.html` are used to improve the UI and are used in `app.R`. The `data-analysis` and `data-processing` directories contain scripts to analyse and process data respectively. The `drafts` directory contains cruder versions of functions present in `page-functions.R`, used to build and test these functions.