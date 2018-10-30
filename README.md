# GHG_shiny_app

### What is this project?
Developing a R.Shiny app for visualisation of Scottish greenhouse gas data, building on Gregor Boyd's ShinySparql code.
https://github.com/GregorBoyd/ShinySparql

### What changes have been made so far?
1. Filter categories are now auto-generated based upon the data collected from https://statistics.gov.scot. 
2. This app now imports all pollutants and allows a selection of different greenhouse gases in addition to source sectors.
3. This now auto-generates totals for all sectors and all pollutants (and both), and makes them all selectable.
4. This now uses shiny widgets to select data which are empirically awesome.
5. This app now creates a data table populated with the result of the user selections.

### What's next?
1. I'm having some difficulty allowing users to download their selected dataset as .csv
2. Some basic summary statistics, e.g. growth rates between years and the like.


### Can I join in?
Of course, go wild!  I'm particularly keen to get edits from people well-versed in optimising this sort of R-Shiny code.

### Can I play with the latest version?
https://androo.shinyapps.io/GHG_shiny_app/
