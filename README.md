# CliDaMonDisplay

### Shiny app for calculating and displaying monthly degree days and solar radiation for German locations selected by postal code

Monthly and annual climate data are provided for specific periods allocated to German postcode zone. The climate data can be used to calulate the energy demand for space heating. Temperature and solar radiation during heating days as well as degree days are provided for specific base temperatures (10°C, 12°C, and 15°C). The function CliDaMonDisplay () of the package "CliDaMonDisplay" is using the data package 'clidamonger' containing monthly values for more than 800 German weather stations (starting from 1995) and the calculation package 'CliDaMon' for finding the three closest weather stations to a postcode and for calculating the degree days. The packages are based on the IWU Excel Workbook 'Gradtagzahlen-Deutschland' (available at https://www.iwu.de/publikationen/fachinformationen/energiebilanzen/gradtagzahltool/).
Calculate monthly climate data - heating degree days and solar radiation by postcode

---

### Method

A description of the method can be found in

Loga, Tobias & Großklos, Marc & Landgraf, Katrin. (2020). Klimadaten für die Realbilanzierung - Grundlagen des Tools „Gradtagzahlen-Deutschland.xlsx“ - MOBASY-Teilbericht. 10.13140/RG.2.2.25695.28324.


---

### Usage

```r
library (CliDaMonDisplay)

```
---

### License

<a rel="license" href="https://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/80x15.png" /></a><br />This work is licensed under a <a rel="license" href="https://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

---


### Variables

A description of the input variables of the function ClimateByMonthDisplay () 
can be found in the help section of the package.

The method and the output quantities of the calculation are explained in German language in the file 'Info.Rmd'. 
This information is also displayed in the section "Info" of the shiny app.  

---


### Views
 <a href="https://trackgit.com">
<img src="https://us-central1-trackgit-analytics.cloudfunctions.net/token/ping/m74l9bu39linw5t9ut8m" alt="trackgit-views" />
</a>
