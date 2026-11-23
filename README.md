# Benthic Herbivory Project
#### Author: Natalie Goeler-Slough
___
### Final project for OCN 682 (Fall 2026)

***Live App Link**: [Benthic Herbivory Project](https://01a20a5b-dac1-a95a-0aea-ae01c1aa6806.share.connect.posit.cloud/)*

___

#### Project Abstract:
Coral reef resilience depends on the balance between vital reef-building corals and competing macroalgae. Herbivory maintains this balance, as fish graze algal competitors and clear substrate for coral recruitment. While protection can restore fish biomass, high biomass does not guarantee the ecological functioning required to curb algal dominance. Traditional ‘snapshot’ surveys miss temporal dynamics by focusing on a single timepoint and ignoring seasonal or annual variation. As anthropogenic stressors increase, understanding how fish community dynamics drive realized grazing is critical for reef resilience. In this project, we investigate herbivorous fish communities and grazing intensity over space and time. We use underwater video stations at focal 1m² benthic quadrats to evaluate how grazing pressure and fish community structure vary over seasons at Hanauma Bay. Second, we link fish size and habitat complexity to algal removal across fishing gradients in a spatial study across Oʻahu.

___

#### Contents of this Shiny App:
* **Bite rates around O‘ahu:** Map of fish grazing rates around O‘ahu, with circles sized to standardized bite rates. Also displays proprtion grazeable substrate and coral cover percentages on popup.
* **Benthic composition explorer:** Interactive charts, allowing users to explore benthic composition across specific sites/dates, in summarized or individual quadrat format.
* **Top biters:** Plot displaying the six fish species with the most bites, alongside photos of these top six biting fish.

___

#### [Data:](https://github.com/OCN-682-UH/Goeler-Slough_Final_Project/tree/main/Shiny_App/Data)
* *coralnet_annotations.csv*
  * Benthic analysis from CoralNet, point annotations for each quadrat (GoPro)
  * CoralNet [labelset description](https://github.com/OCN-682-UH/Goeler-Slough_Final_Project/blob/main/Shiny_App/Data/CoralNet_Labelset.csv)
* *Herbivory_Data_FinalProj.csv*
  * Data from herbivory videos, containing bite rates with paired fish ID, joined with field data
  * [Data dictionary](https://github.com/OCN-682-UH/Goeler-Slough_Final_Project/blob/main/Shiny_App/Data/Herbivory_Videos_Metadata.csv)

___

#### Project Structure:
```text
── Shiny_App
    ├── app.R
    ├── data
    │   ├── coralnet_annotations.csv
    │   ├── CoralNet_Labelset.csv
    │   ├── Herbivory_Data_FinalProj.csv
    │   └── Herbivory_Videos_Metadata.csv
    ├── rsconnect
    └── www
        └── fish
```
___

#### Local Installation & Usage

1. **Clone the GitHub repository:**
   ```bash
   git clone https://github.com
   cd Goeler-Slough_Final_Project
   ```

2. **Install packages:**
   ```r
   install.packages(c("shiny", "tidyverse", "shinyWidgets", "bslib", "plotly", "leaflet"))
   ```

3. **Run the app:**
   ```r
   shiny::runApp()
   ```

___

#### Contact:
Natalie Goeler-Slough: nsgs@hawaii.edu  
*Marine Biology PhD Student, [McDevitt-Irwin Lab](https://www.jamiemcdevittirwin.com/) at University of Hawai‘i Mānoa.*