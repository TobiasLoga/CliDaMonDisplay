#####################################################################################X
##
##    File name:        "CliDaMonDisplay.R"
##
##    Project:          MOBASY
##
##    Author:           Tobias Loga (t.loga@iwu.de)
##                      IWU - Institut Wohnen und Umwelt, Darmstadt / Germany
##
##    Created:          22-04-2022
##    Last changes:     01-03-2024
##
#####################################################################################X

# devtools::install_github ("TobiasLoga/CliDaMon")
# devtools::install_github ("TobiasLoga/AuxFunctions")

library (shiny)
library (shinydashboard)
library (markdown)

library (clidamonger)
library (CliDaMon)
library (AuxFunctions)


#_-----

#####################################################################################X
## Parameters -----      
#####################################################################################X



Year_SelectionList_Last  <- 2023
Year_SelectionList_First <- 1995
Year_Selected            <- 2023
Month_Selected           <- 12



#_-----

#####################################################################################X
## Auxiliary functions -----      
#####################################################################################X


DataFrame_FixRows <- function (
    myDataFrame,
    nRowFix 
) {
  
  myDataFrame [ nrow (myDataFrame)+1 : nRowFix, ] <- NA
  
  return (myDataFrame)
  
}


Format_DataFrameForOutput <- function (
  myDataFrame,
  myDigits = NULL,
  myColNames = NULL,
  myRowNames = NULL
){
  
  myDataFrame_Char <- myDataFrame
  
  n_Col <- ncol (myDataFrame)
  
  if (! is.null (myDigits)) {
    
    for (i_Col in (1:n_Col)) {
      
      if (! is.na (myDigits [i_Col])) {
        
        myDataFrame_Char [ , i_Col] <-
          formatC (
            as.numeric (myDataFrame [ , i_Col]), 
            digits = myDigits [i_Col], 
            format ="f"
          )
      } # End if
      
    } # End loop by i_Col
    
  } # End If
  
  if (! is.null (myColNames)) {
    colnames (myDataFrame_Char) <-
      ifelse (
        is.na (myColNames),
        colnames (myDataFrame_Char),
        myColNames
      )
  } # End if 

  if (! is.null (myRowNames)) {
    rownames (myDataFrame_Char) <-
      ifelse (
        is.na (myRowNames),
        rownames (myDataFrame_Char),
        myRowNames
      )
  } # End if 
  
  return (myDataFrame_Char )
  
} # End of function
#
# ## Example for checking the function
# Format_DataFrameForOutput (
#   myDataFrame = clidamonger::tab.estim.sol.orient,
#   myDigits = c (NA, NA, NA, 2, 4, NA, 3, NA, 4, 4, 4, 5, 2),
#   myColNames = c ("ID", "OrNaem", "OrCode", "OrDegr", "InclDegr", "Study", NA, NA, NA, NA, NA, NA, NA)
# )


RemoveStringFromDFColNames <- function (
    myDataFrame,
    myStringToRemove = ""
) {
  colnames (myDataFrame) <- 
    gsub (
      pattern = myStringToRemove, 
      replacement = "",
      x = colnames (myDataFrame)
    )
  return (myDataFrame)
}



CalculateClimate <- function (
    input,
    Index_InputVersion = 1
) {

  Temperature_HDD_Base_1 <- as.numeric (input$Temperature_HDD_Base_1)
  Temperature_HDD_Base_2 <- as.numeric (input$Temperature_HDD_Base_2)

  Temperature_HDD_Room_1 <- as.numeric (input$Temperature_HDD_Room_1)
  Temperature_HDD_Room_2 <- as.numeric (input$Temperature_HDD_Room_2)
  
  if (input$ShowInput_BaseTemp_RoomTemp_2 == FALSE) {
    Temperature_HDD_Base_2 <- Temperature_HDD_Base_1
    Temperature_HDD_Room_2 <- Temperature_HDD_Room_1
  }
  
  PostCode_1            <- input$PostCode_1
  PostCode_2            <- input$PostCode_2
  Code_ClimateStation_1 <- input$Code_ClimateStation_1
  Code_ClimateStation_2 <- input$Code_ClimateStation_2

  if (input$ShowInput_Location_2 == FALSE) {
    PostCode_2            <- PostCode_1
    Code_ClimateStation_2 <- Code_ClimateStation_1
  }

  Year_End_1  <- input$Year_End_1
  Year_End_2  <- input$Year_End_2
  Month_End_1 <- input$Month_End_1
  Month_End_2 <- input$Month_End_2
  n_Year_1    <- input$n_Year_1
  n_Year_2    <- input$n_Year_2
  
  
  if (input$ShowInput_LastYear_2 == FALSE){
    Year_End_2 <- Year_End_1
  }
  
  if (input$ShowInput_LastMonth_2 == FALSE){
    Month_End_2 <- Month_End_1
  }
  
  Year_Start_1 <- 
    as.numeric (Year_End_1) - 
    (as.numeric (n_Year_1) - (as.numeric (Month_End_1)  %/% 12))
  # =E4-(G4-QUOTIENT(F4;12))
  # 2023 - (2 - (12 %/% 12))

  Month_Start_1 <- 
    ((as.numeric (Month_End_1) - 12) %% 12) + 1
  # =REST(F4-12;12)+1
  # ((11 - 12) %% 12) + 1

  Year_Start_2 <- 
    as.numeric (Year_End_2)  - 
    (as.numeric (n_Year_2) - (as.numeric (Month_End_2) %/% 12))

  Month_Start_2 <- 
    Month_Start_1
    # ((as.numeric (input$Month_End_2) - 12) %% 12) + 1

  Degree_Inclination_Solar <- as.numeric (input$Degree_Inclination_Solar_1)
  
  if (Index_InputVersion == 2) {
    
    Code_Type_Location     <- input$Code_Type_LocationBuilding_2
    PostCode               <- PostCode_2
    Code_ClimateStation    <- Code_ClimateStation_2
    Year_Start             <- Year_Start_2
    Month_Start            <- Month_Start_2
    n_Year                 <- n_Year_2
    Temperature_HDD_Base   <- Temperature_HDD_Base_2
    Temperature_HDD_Room   <- Temperature_HDD_Room_2
    #Degree_Inclination_Solar <- as.numeric (input$Degree_Inclination_Solar_2)
    
  } else {
    
    Code_Type_Location     <- input$Code_Type_LocationBuilding_1
    PostCode               <- PostCode_1
    Code_ClimateStation    <- Code_ClimateStation_1
    Year_Start             <- Year_Start_1
    Month_Start            <- Month_Start_1
    n_Year                 <- n_Year_1
    Temperature_HDD_Base   <- Temperature_HDD_Base_1
    Temperature_HDD_Room   <- Temperature_HDD_Room_1
    #Degree_Inclination_Solar <- as.numeric (input$Degree_Inclination_Solar_1)
    
  }
  
  myResultList <-
    CliDaMon::ClimateByMonth (
      myClimateData_PostCodes     = as.data.frame (clidamonger::tab.stationmapping),
      myClimateData_StationTA     = as.data.frame (clidamonger::list.station.ta),
      myClimateData_TA_HD         = as.data.frame (clidamonger::data.ta.hd),
      myClimateData_Sol           = as.data.frame (clidamonger::data.sol),
      myParTab_SolOrientEst       = as.data.frame (clidamonger::tab.estim.sol.orient),
      Indicator_Type_LocationBuilding =
        ifelse (Code_Type_Location == "ID_Station", 2, 1),
      Indicator_Type_AssignStationToPostcode =
        ifelse (Code_Type_Location == "Postcode_1Station", 1, 2),
      PostCode                    = PostCode,
      Code_ClimateStation         = Code_ClimateStation,
      Indicator_ExcludeSelectedStation = 0,
      Month_Start                 = Month_Start,
      # Year_Start                  = input$RangeYear [1],
      # n_Year                      = input$RangeYear [2] - input$RangeYear [1] + 1,
      Year_Start                  = Year_Start,
      n_Year                      = n_Year,
      Temperature_HDD_Base        = Temperature_HDD_Base,
      Temperature_HDD_Room        = Temperature_HDD_Room,
      Degree_Inclination_Solar    = Degree_Inclination_Solar 
    )        
  
  return ( myResultList )
  
}

#_ -----



#####################################################################################X
## FUNCTION / SHINY APP "ClimateByMonthDisplay ()" -----
#####################################################################################X
# This function is not working with shinyapps.io, therfore disabled 

#' @title Shiny App for calculating and displaying monthly climate data - heating degree days and solar radiation by postcode
#'
#' @description
#' Monthly and annual climate data are provided for specific periods
#' allocated to German postcode zone. The climate data can be used to calculate
#' the energy demand for space heating. Temperature and solar radiation during heating days
#' as well as degree days are provided for specific base temperatures (10°C, 12°C, and 15°C).
#'
#' The package "CliDaMonDisplay" is using the data package 'clidamonger' containing monthly values for more than 800 German weather stations (starting from 1995) and the calculation package 'CliDaMon' for finding the three closest weather stations to a postcode and for calculating the degree days. The packages are based on the IWU Excel Workbook 'Gradtagzahlen-Deutschland' (available at https://www.iwu.de/publikationen/fachinformationen/energiebilanzen/gradtagzahltool/ - the algorithms are similar but not identical)
#'
#' @param Year_SelectionList_Last an integer indicating the last year to be included 
#' in the selection list of the user interface. The input should correspond 
#' to the data included in the used R data package 'clidamonger'.  
#'
#' @param Year_SelectionList_First an integer indicating the first year to be included 
#' in the selection list of the user interface. The input should correspond 
#' to the data included in the R data package 'clidamonger'. The default value is 1995.  
#'
#' @param Year_Selected an integer indicating the preselected year in the selection list.   
#'
#' @param Month_Selected an integer indicating the preselected momth in the selection list.   
#'
#' @return Run the interactive shiny app. 
#' The input and output is assigned to climate 1 (default: 1 year) 
#' and to climate 2 (default: long term average).  
#' The output dataframes for each of both climates are:
#'
#' DF_ClimCalc:          a dataframe containing climate data for 12 months and the complete year.
#'                       If more than one year is evaluated the resulting values of each month
#'                       is the average of this month and the result data of the year
#'                       is an average year.
#'
#' DF_Evaluation:        a dataframe containing climate data for all considered months.
#'
#' DF_StationInfo:       a dataframe containing information about the used climate stations.
#'
#' DF_FunctionArguments: a dataframe containing the values of all function arguments (one row).
#'
#'
#' @examples 
#' CliDaMonDisplay (
#'    Year_SelectionList_Last = 2023,
#'    Year_Selected           = 2023,
#'    Month_Selected          = 12
#'    )
#'

# #' @export
# CliDaMonDisplay <- function (
#     Year_SelectionList_Last  = 2023,
#     Year_SelectionList_First = 1995,
#     Year_Selected  = 2023,
#     Month_Selected = 12
# ) {
# 



#_ -----


#####################################################################################X
## USER INTERFACE -----
#####################################################################################X


ui <- shinydashboard::dashboardPage (

  
  shinydashboard::dashboardHeader (
    title = "IWU - Gradtagzahlen"
  ),
  
  shinydashboard::dashboardSidebar ( 
    
    #minified = FALSE, 
    #collapsed = FALSE, 
    
    shinydashboard::sidebarMenu (
      shinydashboard::menuItem (
        "Info",      
        tabName = "Tab_Info",      
        icon = shiny::icon ("info-circle")
        ),
      shinydashboard::menuItem (
        "Dashboard", 
        tabName = "Tab_Dashboard",
        icon = shiny::icon ("chart-line", class = NULL, lib = "font-awesome"),
#        icon = fontawesome::fa ("chart-line"),
#        icon = fontawesome::fa ("fa-solid fa-chart-line"),
#          icon = icon ("fa-solid fa-chart-line"),
#        icon = icon ("dashboard"),
        selected = TRUE
        ),
      shinydashboard::menuItem (
        "Daten",     
        tabName = "Tab_Data",          
        icon = shiny::icon ("table")
        )
    ) # End side barMenu
    
  ), # End dashboardSidebar 
  
  
  
  #######################################################################X
  ## Dashboard Body -----
  
  shinydashboard::dashboardBody (
    
    shinydashboard::tabItems (
      
      
      #######################################################################X
      ## tabItem "Tab_Info"  -----
      
      shinydashboard::tabItem (
        
        tabName = "Tab_Info",
        
        # h2 ("Information"),
        
        
        markdown (
          # Comment Tobias: I did not manage to source the file "info.Rmd" in 
          # shinyapps.io; see commented script below the text. Any suggestions are welcome :)  
          
          
"## IWU - Gradtagzahlen Deutschland - Shiny App
Version: 01.03.2024

## Erläuterungen 

Diese Shiny-App dient der Ermittlung von Monatswerten für Klimadaten, die in der energetischen Bilanzierung und bei der Klimabereinigung verwendet werden können. Sie umfasst einen Teil der Funktionalität des Excel-Tools 'Gradtagzahlen-Deutschland.xlsx'. Das Tool wurde im Kontext des Forschungsprojektes MOBASY erstellt. 

Wir stellen das Werkzeug gerne anderen Experten zur Nutzung Verfügung, können jedoch keinerlei Support übernehmen. Wir übernehmen keine Gewähr für die Vollständigkeit, Richtigkeit und Genauigkeit der Berechnungen und der Daten. Fehler können gemeldet werden an: Tobias Loga t.loga@iwu.de


### Erläuterung der Variablen 

- **D [d] Tage (days)** <br>
   Länge der betrachteten Periode

- **TA [°C] Außenlufttemperatur (air temperature)**

- **HD [d] Heiztage (heating days)**  <br>
   Heiztage sind Tage an denen die über den Tag gemittelte Außenlufttemperatur unter der Heizgrenztemperatur liegt.

- **HDD [Kd] Heizgradtage (heating degree days)** <br>
   Zur Ermittlung der Heizgradtage eines Monats werden die an Heiztagen auftretenden Differenzen zwischen der Heizgrenztemperatur und dem Tagesmittel der Außenlufttemperatur erfasst und aufsummiert.

- **RHDD [Kd] Gradtagzahlen	(room heating degree days)** <br>
   Zur Ermittlung der Gradtagszahl eines Monats werden die an Heiztagen auftretenden Differenzen zwischen der Raumtemperatur und dem Tagesmittel der Außenlufttemperatur erfasst und aufsummiert.

- **CT [-] Indikator für die Vollständigkeit (completeness indicator)** <br>
   Der Indikator kann im Prinzip Werte zwischen 0,00 und 1,00 annehmen. Ein Wert von 0,95 bedeutet beispielsweise, dass für 5% des Zeitraums keine Messdaten für die Außentemperatur vorliegen (Messdatenausfälle der Wetter-Stationen). 

- **G_Hor, G_E, G_S, G_W, G_N [kWh] Globalstrahlung auf unterschiedliche Orientierungen** <br>
   Monatswerte bzw. Jahreswerte Globalstrahlung; Orientierung der Flächen horizontal sowie vertikal Ost, Süd, West, Nord

- **G_Hor_HD, G_E_HD, G_S_HD, G_W_HD, G_N_HD [kWh] Globalstrahlung an Heiztagen** <br>
  Monatswerte bzw. Jahreswerte Globalstrahlung an Heiztagen; Orientierung der Flächen horizontal sowie vertikal Ost, Süd, West, Nord


### Gradtagzahl oder Heizgradtage - Welchen Wert wofür verwenden?										

Die Gradtagzahl ist die richtige Eingangsgröße für eine Energiebilanzrechnung, bei der innerhalb der Heizperiode solare und interne Gewinne mit berücksichtigt werden, wodurch sich der Wärmebedarf entsprechend reduziert.										
Heizgradtage sind der geeignete Vergleichswert um für gemessene Verbräuche eine Klimabereinigung vorzunehmen. Dabei wird der Verbrauchswert durch die entsprechenden Heizgradtage geteilt, woduch sich ein Wärmebedarf pro Temperaturdifferenz ergibt. Durch den Vergleich dieser Werte für mehrere Heizperioden lässt sich feststellen, ob sich ein Verbrauchswert klimabereinigt vermindert oder erhöht hat.


### Welches Gebäude hat welche Heizgrenze?										

Je besser der Wärmeschutz eines Gebäudes ist, um so niedriger liegt die Heizgrenztemperatur. 								

Im Folgenden sind Anhaltswerte für die Heizgrenztemperatur genannt:			

|  Baustandard	         | Heizgrenze |
|  --------------------- | ---------- |
|  Bestandsgebäude	     |   15,0 °C  |								
|  Niedrigenergiehäuser	 |   12,0 °C	|					
|  Passivhäuser	         |   10,0 °C  |						


Diese Heizgrenztemperaturen gelten für Standardansätze des Klimas und der Nutzung (z.B. Raumtemperatur 20°C). Die tatsächliche Heizgrenztemperatur eines Gebäudes kann jedoch deutlich davon abweichen. Gegenüber den genannten Zahlenwerten erhöhte Werte können sich beispielsweise bei höheren Raumtemperaturen oder bei starker Verschattung der Fenster ergeben. 		

23-02-2024

Institut Wohnen und Umwelt GmbH

Tobias Loga 									

www.iwu.de 

          
          
")
        

        #includeMarkdown ("info/info.Rmd")
        # this is working locally, but is not found after deployment 
        # --> shinyapps.io fails to run. 
        

      ), # End tabItem "Tab_Info"
      

      #######################################################################X
      ## tabItem "Tab_Dashboard"  -----
      
      shinydashboard::tabItem (
        
        tabName = "Tab_Dashboard",
        
        h2 ("IWU - Gradtagzahlen Deutschland - Shiny App"),
        
        
        fluidPage (
          
            # titlePanel (
            #   title = strong ("IWU - Gradtagzahlen Deutschland - Shiny App"),
            #   windowTitle = "IWU - Gradtagzahlen Deutschland - Shiny App"
            # ),
          
          
          ### sidebarLayout START -----
          
          sidebarLayout ( 
            
            sidebarPanel (
              
              fluidRow (
                #        mainPanel (
                
                
                column (
                  12, 
                  #style = "height:600px", 
                  # # geht im Prinzip, aber optimale Höhe hängt von Fenstergröße ab
                  #style = "height:800px;background-color: yellow",
                  
                  
                  tabsetPanel (
                    type = "tabs",
                    
                    #######################################################################X
                    #### [ tabPanel "Eingaben" START -----
                    
                    tabPanel (
                      "Eingaben", 
                      
                      #strong ("Eingaben"), " ",
                      
                      br (),
                      
                      #######################################################################X
                      #### Eingaben "Klima 1" ----- 
                      
                      fluidRow (
                        strong ("Eingaben Klima 1")
                      ),
                      
                      br (),
                      
                      
                      conditionalPanel (
                        
                        condition = 
                          "(input.Code_Type_LocationBuilding_1 == 'Postcode_1Station') | 
                (input.Code_Type_LocationBuilding_1 == 'Postcode_3Stations')",
                
                
                fluidRow (
                  
                  column (
                    7,
                    "Postleitzahl"
                  ),
                  
                  column (
                    5,
                    numericInput (
                      inputId = "PostCode_1",
                      label = NULL,
                      #label = "Postleitzahl",
                      value = 13469,
                      min = 0,
                      max = 99999,
                      width = 100
                    ),
                    style = "height:35px"
                  ),
                  
                  style = "border: 1px dotted lightgrey"
                  
                ), # End fluidRow
                
                
                      ), # End conditionalPanel
                
                
                conditionalPanel (
                  
                  condition = 
                    "(input.Code_Type_LocationBuilding_1 == 'ID_Station')",
                  
                  fluidRow (
                    column (
                      7,
                      "Klimastation (Nr.)",
                    ),
                    
                    column (
                      5,
                      #"Klimastation (Nr.)",
                      numericInput (
                        inputId = "Code_ClimateStation_1",
                        label = NULL,
                        #label = "Klimastation (Nr.)",
                        value = 917,
                        min = 1,
                        max = 99999,
                        width = 100
                      ),
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                  
                ), # End conditionalPanel
                
                
                fluidRow (
                  
                  column (
                    7,
                    "Jahr",
                  ),
                  
                  column (
                    5,
                    selectInput (
                      inputId = "Year_End_1",
                      label = NULL,
                      #label   = "Jahr",
                      choices = c (Year_SelectionList_First:Year_SelectionList_Last),
                      #choices = c (1995:2023),
                      selected = Year_Selected,
                      #selected = 2021,
                      width = 100
                    ),
                    style = "height:35px"
                  ),
                  
                  style = "border: 1px dotted lightgrey"
                  #style = "border: 1px solid lightgrey"
                  #style = "background-color: yellow",
                  #style = "height:30px;background-color: white",
                  
                ),
                
                
                fluidRow (
                  
                  column (
                    7,
                    "letzter Monat"
                  ),
                  
                  column (
                    5,
                    selectInput (
                      inputId = "Month_End_1",
                      label = NULL,
                      #label   = "letzter Monat",
                      choices = c (1:12),
                      selected = Month_Selected,
                      #selected = 12,
                      width = 100
                    ),
                    style = "height:35px"
                  ),
                  
                  style = "border: 1px dotted lightgrey"
                  
                ),   # End fluidRow
                
                
                fluidRow (
                  
                  column (
                    7,
                    "Anzahl Jahre"
                  ),
                  
                  column (
                    5,
                    selectInput (
                      inputId = "n_Year_1", 
                      label = NULL,
                      #label   = "Anzahl Jahre",
                      choices = c (1:20),
                      selected = 1,
                      width = 100
                    ),
                    style = "height:35px"
                  ),
                  
                  style = "border: 1px dotted lightgrey"
                  
                ),   # End fluidRow
                
                
                fluidRow (
                  
                  column (
                    7,
                    "Heizgrenz-Temperatur [°C]"
                  ),
                  
                  column (
                    5,
                    selectInput (
                      inputId = "Temperature_HDD_Base_1",
                      label   = NULL,
                      choices = c (10, 12, 15),
                      selected = 12,
                      width = 100
                    ),
                    style = "height:35px"
                  ),
                  
                  style = "border: 1px dotted lightgrey"
                  
                ),   # End fluidRow
                
                
                conditionalPanel (
                  
                  condition = 
                    "(input.Code_Type_DegreeDays == 'RHDD')",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Raum-Temperatur [°C]"
                    ),
                    
                    column (
                      5,
                      numericInput (
                        inputId = "Temperature_HDD_Room_1", 
                        label = NULL,
                        value = 20,
                        min = 15,
                        max = 25,
                        width = 100
                      ),
                      style = "height:35px"
                    ),
                    
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ) # END fluidRow
                  
                ), # END conditionalPanel
                
                br (),
                
                
                #######################################################################X
                #### Eingaben "Klima 2" ----- 
                
                fluidRow (strong ("Eingaben Klima 2")),
                
                br (),
                
                conditionalPanel (
                  
                  condition =  
                    "((input.Code_Type_LocationBuilding_2 == 'Postcode_1Station') | 
                   (input.Code_Type_LocationBuilding_2 == 'Postcode_3Stations')) &
                   (input.ShowInput_Location_2 |
                    input.Code_Type_LocationBuilding_1 == 'ID_Station')",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Postleitzahl"
                    ),
                    
                    column (
                      5,
                      numericInput (
                        inputId = "PostCode_2",
                        label = NULL,
                        # label = "Postleitzahl",
                        value = 13469,
                        min = 0,
                        max = 99999,
                        width = 100
                      ),
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                ), # End conditionalPanel()
                
                
                conditionalPanel (
                  
                  condition = 
                    "(input.Code_Type_LocationBuilding_2 == 'ID_Station') &
                   (input.ShowInput_Location_2 |
                    input.Code_Type_LocationBuilding_1 != 'ID_Station')",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Klimastation (Nr.)"
                    ),
                    
                    column (
                      5,
                      numericInput (
                        inputId = "Code_ClimateStation_2",
                        label = NULL,
                        # label = "Klimastation (Nr.)",
                        value = 917,
                        min = 1,
                        max = 99999,
                        width = 100
                      ),
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                ),  # End conditionalPanel       
                
                
                conditionalPanel (
                  
                  condition =
                    "input.ShowInput_LastYear_2",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Jahr"
                    ),
                    
                    column (
                      5,
                      selectInput (
                        inputId = "Year_End_2",
                        label = NULL,
                        # label   = "Jahr",
                        choices = c (Year_SelectionList_First:Year_SelectionList_Last),
                        #choices = c (1995:2023),
                        selected = Year_Selected,
                        width = 100
                      ), 
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ) # End fluidRow
                  
                ), # End conditionalPanel
                
                
                conditionalPanel (
                  
                  condition = "input.ShowInput_LastMonth_2", 
                  
                  fluidRow (
                    
                    column (
                      7,
                      "letzter Monat"
                    ),
                    
                    column (
                      5,
                      selectInput (
                        inputId = "Month_End_2",
                        label = NULL,
                        # label   = "letzter Monat",
                        choices = c (1:12),
                        selected = Month_Selected,
                        width = 100
                      ), 
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                ), # End conditionPanel
                
                
                fluidRow (
                  
                  column (
                    7,
                    "Anzahl Jahre"
                  ),
                  
                  column (
                    5,
                    selectInput (
                      inputId = "n_Year_2", 
                      label = NULL,
                      # label   = "Anzahl Jahre",
                      choices = c (1:25),
                      selected = 20,
                      width = 100
                    ), 
                    style = "height:35px"
                  ),

                  style = "border: 1px dotted lightgrey"
                  
                ), # End fluidRow
                
                
                conditionalPanel (
                  
                  condition = 
                    "input.ShowInput_BaseTemp_RoomTemp_2",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Temperatur Heizgrenze [°C]"
                    ),
                    
                    column (
                      5,
                      selectInput (
                        inputId = "Temperature_HDD_Base_2",
                        label   = NULL,
                        choices = c (10, 12, 15),
                        selected = 12,
                        width = 100
                      ), 
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                  
                ), # END conditionalPanel
                
                
                conditionalPanel (
                  
                  condition = 
                    "(input.Code_Type_DegreeDays == 'RHDD') &
                   input.ShowInput_BaseTemp_RoomTemp_2",
                  
                  fluidRow (
                    
                    column (
                      7,
                      "Raum-Temperatur [°C]"
                    ),
                    
                    column (
                      5,
                      numericInput (
                        inputId = "Temperature_HDD_Room_2", 
                        label = NULL,
                        value = 20,
                        min = 15,
                        max = 25,
                        width = 100
                      ), 
                      style = "height:35px"
                    ),
                    
                    style = "border: 1px dotted lightgrey"
                    
                  ), # End fluidRow
                  
                ), # END conditionalPanel
                
                
                    ), #### ] END tabPanel "Eingaben" ----
                
                
                #######################################################################X
                #### tabPanel "Einstellungen" START -----
                
                
                tabPanel (
                  "Einstellungen",
                  
                  
                  fluidRow (
                    
                    
                    br (),
                    
                    column (
                      12,
                      
                      radioButtons (
                        inputId = "Code_Type_DegreeDays",
                        label = "Art der Gradtage",
                        c ("Heizgradtage (RHDD)" = "RHDD",
                           "Gradtagzahl (HDD)" = "HDD"),
                        selected = "RHDD"
                      ),
                      
                      "Gradtagzahl = aufsummierte Differenzen zwischen Innen- und Außentemperatur)",
                      br (),
                      "Heizgradtage = aufsummierte Differenzen zwischen Heizgrenz- und Außentemperatur)"
                    )
                  ),
                  
                  
                  br (),
                  
                  strong ("Art der Standortwahl"),
                  
                  br (),
                  
                  fluidRow (
                    
                    column (
                      6,
                      radioButtons (
                        inputId = "Code_Type_LocationBuilding_1",
                        label = "Klima 1",
                        c ("PLZ / 1 Station"    = "Postcode_1Station",
                           "PLZ / 3 Stationen"  = "Postcode_3Stations",
                           "Nummer der Station" = "ID_Station"),
                        selected = "Postcode_3Stations"
                      ),
                      style = "border: 1px dotted lightgrey"
                    ),
                    
                    column (
                      6,
                      radioButtons (
                        inputId = "Code_Type_LocationBuilding_2",
                        label = "Klima 2",
                        c ("PLZ / 1 Station"    = "Postcode_1Station",
                           "PLZ / 3 Stationen"  = "Postcode_3Stations",
                           "Nummer der Station" = "ID_Station"),
                        selected = "Postcode_3Stations"
                      ),
                      style = "border: 1px dotted lightgrey"
                    ),
                    
                    
                    column (
                      12,
                      
                      strong ("Solarstrahlung geneigte Fläche"),
                      
                      fluidRow (
                        
                        column (
                          7,
                          "Neigung in Grad"
                        ),
                        
                        column (
                          5,
                          selectInput (
                            inputId = "Degree_Inclination_Solar_1", 
                            label   = NULL,
                            choices = c (30, 45, 60),
                            selected = 45,
                            width = 100
                          ), 
                          style = "height:35px"
                        ),
                        
                        style = "border: 1px dotted lightgrey"
                        
                      ), # End fluidRow
                      
                      br (),
                      
                    ),
                    
                    br (),
                    
                  ),
                  
                  strong ("Diagramme einblenden"),
                  
                  fluidRow (
                    
                    column (
                      6,
                      column (
                        12,
                        checkboxInput(
                          inputId =  "ShowPlot_Temperature_1",
                          label = "Temperatur 1",
                          value = TRUE,
                          width = NULL
                        ),
                        style = "height:25px"
                      ),
                      column (
                        12,
                        checkboxInput (
                          inputId =  "ShowPlot_Temperature_2",
                          label = "Temperatur 2",
                          value = TRUE,
                          width = NULL
                        ),  
                        style = "height:25px"
                      )
                    ),
                    
                    column (
                      6,
                      column (
                        12,
                        checkboxInput (
                          inputId =  "ShowPlot_RHDD_HDD",
                          label = "Gradtagzahl, Heizgradtage",
                          value = TRUE,
                          width = NULL
                        ),
                        style = "height:40px"
                      ),
                      column (
                        12,
                        checkboxInput (
                          inputId =  "ShowPlot_HD",
                          label = "Heiztage",
                          value = TRUE,
                          width = NULL
                        ),
                        style = "height:25px"
                      )
                    ),
                    
                  ), # End fluidRow
                  
                  
                  
                  
                  br (),
                  
                  strong ("Klima 2: individuelle Eingafelder einblenden"),
                  
                  fluidRow (
                    column (
                      12,
                      checkboxInput (
                        inputId =  "ShowInput_Location_2",
                        label = "Postleitzahl / Klimastation",
                        value = FALSE,
                        width = NULL
                      ),
                      style = "height:25px"
                    )
                  ),
                  
                  fluidRow (
                    column (
                      4,
                      checkboxInput (
                        inputId =  "ShowInput_LastYear_2",
                        label = "Jahr",
                        value = FALSE,
                        width = NULL
                      ),
                      style = "height:25px"
                    ),
                    
                    column (
                      8,
                      checkboxInput (
                        inputId =  "ShowInput_LastMonth_2",
                        label = "letzter Monat",
                        value = FALSE,
                        width = NULL
                      ),
                      style = "height:25px"
                    )
                    
                  ),
                  
                  checkboxInput (
                    inputId =  "ShowInput_BaseTemp_RoomTemp_2",
                    label = "Heizgrenze / Raumtemperatur",
                    value = FALSE,
                    width = NULL
                  ),
                  
                  
                  
                  
                  
                )             
                #### END tabPanel "Einstellungen" -----

                  )
                
                )        
                
              ),

              br(),
              

              fluidRow (

                br (),
                
                strong ("Ergebnisse"),
                
                br (),
                
                column (12,
                  
                    br (),
                    strong ("Klima 1 im Verhältnis zu Klima 2"),

                ),
                
                column (6,
                        textOutput ("Text_Result_RHDD_HDD_Ratio1to2")
                ),
                column (6,
                        textOutput ("Text_Result_G_S_HD_Ratio1to2")
                ),
                
                
                br (),
                
                column (12,
                        
                        br (),
                        strong ("Klima 2 im Verhältnis zu Klima 1"),
                        
                ),
                
                column (6,
                        textOutput ("Text_Result_RHDD_HDD_Ratio2to1")
                ),
                column (6,
                        textOutput ("Text_Result_G_S_HD_Ratio2to1")
                ),
                
                
                
                # fluidRow (
                  # column (4,
                  #         textOutput ("Text_Result_RHDD_HDD_Ratio1to2")
                  # ),
                  # column (4,
                  #         textOutput ("Text_Result_G_S_HD_Ratio1to2")
                  # ),
                # ), # End fluidRow
                
                style = "background-color: white"
                
              ), # End fluidRow "Ergebnisse"
              
              
              

              
              fluidRow (
                column (
                  4,
                  br (),
                  strong ("Jahreswerte"),
                  tableOutput ("Table_Result_Year"),
                  #style = "height:800px;background-color: yellow",
                  
                ),
                style = "background-color: white"
              )
              
            ),
            
            
            #### mainPanel START -----
            
            
            mainPanel (
              
              strong (""),
              
              
              conditionalPanel (
                
                "input.ShowPlot_Temperature_1",
                
                "Außentemperatur Klima 1 (Monatsmittel und Monatsmittel an Heiztagen)",
                
                plotOutput (outputId = "TemperaturePlot_1",
                            height = "200px"),
                
              ),
              
              conditionalPanel (
                
                "input.ShowPlot_Temperature_2",
                
                "Außentemperatur Klima 2 (Monatsmittel und Monatsmittel an Heiztagen)",
                
                plotOutput (outputId = "TemperaturePlot_2",
                            height = "200px")
                
              ),
              
              
              conditionalPanel (
                
                "input.ShowPlot_RHDD_HDD",
                
                conditionalPanel (
                  "input.Code_Type_DegreeDays == 'RHDD'",
                  
                  "Gradtagzahl",
                  plotOutput (outputId = "Plot_RHDD",
                              height = "200px")
                ),
                
                conditionalPanel (
                  "input.Code_Type_DegreeDays == 'HDD'",
                  "Heizgradtage",
                  plotOutput (outputId = "Plot_HDD",
                              height = "200px")
                )
                
              ),
              
              conditionalPanel (
                
                "input.ShowPlot_HD",
                
                "Heiztage",
                plotOutput (outputId = "Plot_HeatingDays",                   
                            height = "200px"),
              ), # End conditionalPanel
              
            ) #### END mainPanel ----
            
          ), ### END sidebarLayout ----
          
          
          br (),
          strong ("Klima 1 (oben) und Klima 2 (unten) / Mittel über Zeitraum"),
          tableOutput ("Table_ClimCalc_Both"),
          
          
          br (),
          
          strong ("Klima 1"),
          #tableOutput ("Table_HDD_Compact_1"),
          tableOutput ("Table_ClimCalc_1"),
          tableOutput ("Table_Evaluation_1"),
          tableOutput ("Table_StationInfo_1"),
          tableOutput ("Table_FunctionArguments_1"),
          #tableOutput ("Table_OutputStructure_1"),
          
          
          br (),
          
          strong ("Klima 2"),
          #tableOutput ("Table_HDD_Compact_2"),
          tableOutput ("Table_ClimCalc_2"),
          tableOutput ("Table_Evaluation_2"),
          tableOutput ("Table_StationInfo_2"),
          tableOutput ("Table_FunctionArguments_2"),
          #tableOutput ("Table_OutputStructure_2"),
          
          textOutput ("Text_SelectedInput"),
          #verbatimTextOutput ("Text_SelectedInput")
          
          
        )
        
        
        
      ), # End tabItem "Tab_Dashboard"
      
      
      #######################################################################X
      ## tabItem "Tab_Data"  -----
      
      shinydashboard::tabItem (
        
        tabName = "Tab_Data",
        
        h2("Daten-Tabellen"),
        
        
        fluidPage (
          
          #titlePanel ("Daten-Tabellen"),

          fluidRow (
          

            # Choose Output tables ----
            selectInput (
              "myOutputSelection", 
              "Choose an output table:",
              choices = c(
                "ResultTable_Year",
                "DF_ClimCalc_1",
                "DF_ClimCalc_2",
                "DF_ClimCalc_Both",
                "DF_Evaluation_1",
                "DF_Evaluation_2"
                # "Data.TA.HD"
              )
            ),
            
            checkboxInput(
              inputId =  "TransposeOutputTable",
              label = "Zeilen und Spalten vertauschen",
              value = FALSE,
              width = NULL

            ),
            
          
          
          
            # # Button
            # downloadButton (
            #   outputId = "downloadData", 
            #   class = "Download",
            #   icon = shiny::icon("download")
            # )
            
          ),
          
          column (
              12,
              
              fluidRow (
                DT::DTOutput (
                  "myTable"
                )
                
                # tableOutput (
                #   "myTable"
                # )
              )
          
            )

        

          
        
            

        ) # End fluid page
        
        
        
        
      ) # End tabItem "Tab_Data" 
      
    ) # End tabItems
    
  ) # End dashboardBody
  
) # End dashboardPage

  
  
  
  
  
  
  

#_ -----


#####################################################################################X
## SERVER FUNCTION -----
#####################################################################################X


server <- function (input, output, session) {
  
  session$onSessionEnded (function() {
    stopApp()
  }) 
  # This prevents R from crashing when closing the Shiny app window.
  # The object "session" was added to the parameters of the server function. 

  
  output$Text_SelectedInput <- renderText ({
    paste0 (
      "Eingaben: ", 
      "Art der Zuordnung Klimadaten: ", input$Code_Type_LocationBuilding_1, " / ", 
      "ID der Klimastation: ", input$Code_ClimateStation_1, " / ", 
      "PLZ: ",  input$PostCode_1, " / ", 
      "Jahr: ", input$Year_End_1, " / ",
      "Letzter Monat: ", input$Month_End_1, "/"
      # "Test: ", TestFunction1 (input$PostCode, input$Year_Start, input$Month_Start), " / "
      # input$RangeYears [1], " / ", input$RangeYears [2], " / ",
      # "Test: ", mean (input$PostCode, input$Year_Start, input$Month_Start)
    )
  })


  ##########################################################################################X
  ## Climate data calculation ----

  # Result of CalculateClimate () is a list of 5 dataframes:
  # 
  # DF_ClimCalc: a dataframe containing climate data for 12 months and the complete year. 
  # If more than one year is evaluated the resulting values of each month is the average of this month 
  # and the result data of the year is an average year.
  # 
  # DF_Evaluation: a dataframe containing climate data for all considered months.
  # 
  # DF_StationInfo: a dataframe containing information about the used climate stations.
  # 
  # DF_FunctionArguments: a dataframe containing the values of all function arguments (one row).
  # 
  # DF_OutputStructure: a dataframe containing information about the data structure of the output 
  # (dataframe names and number of column).


  ######################################################################################X
  ## Climate calculation
  
  
  myResultList_1 <- reactive ({
      CalculateClimate (
        input = input,
        Index_InputVersion = 1
        )
  })

  
  myResultList_2 <- reactive ({
    CalculateClimate (
      input = input,
      Index_InputVersion = 2
    )
  })

    
  # DF_ClimCalc_1 <-
  #   reactive ({
  #     as.data.frame (
  #       myResultList_1 () ["DF_ClimCalc"]
  #     )
  #   })
  # 
  # 
  # DF_ClimCalc_2 <-
  #   reactive ({
  #     as.data.frame (
  #       myResultList_2 () ["DF_ClimCalc"]
  #     )
  #   })
  # 
  
  
  DF_ClimCalc_1 <-
    reactive ({
      RemoveStringFromDFColNames (
        myDataFrame = 
          as.data.frame (myResultList_1 () ["DF_ClimCalc"]),
        myStringToRemove = "DF_ClimCalc."
        
      )
    })
  
  
  DF_ClimCalc_2 <-
    reactive ({
      RemoveStringFromDFColNames (
        myDataFrame = 
          as.data.frame (myResultList_2 () ["DF_ClimCalc"]),
        myStringToRemove = "DF_ClimCalc."
      )
    })
  
  
  
  
  DF_ClimCalc_Both <-
    reactive ({
      data.frame (
          "Index" = c (1:24),
          "Month" = as.character (
            c (
              DF_ClimCalc_1 () [c(1:12), "Month"], 
              DF_ClimCalc_2 () [c(1:12), "Month"])),
          "Analysis" = c (rep ("Klima 1", 12), rep ("Klima 2", 12)),
          "TA" = c (
            as.numeric (DF_ClimCalc_1 () [c(1:12), "TA"]) ,
            as.numeric (DF_ClimCalc_2 () [c(1:12), "TA"])
          ),
          "TA_HD" = c (
            as.numeric (DF_ClimCalc_1 () [c(1:12), "TA_HD"]) ,
            as.numeric (DF_ClimCalc_2 () [c(1:12), "TA_HD"])
          ),
          "HD" = c (
            as.numeric (DF_ClimCalc_1 () [c(1:12), "HD"]) ,
            as.numeric (DF_ClimCalc_2 () [c(1:12), "HD"])
            ),
          "HDD" = c (
            as.numeric (DF_ClimCalc_1 () [c(1:12), "HDD"]) ,
            as.numeric (DF_ClimCalc_2 () [c(1:12), "HDD"])
          ),
          "RHDD" = c (
            as.numeric (DF_ClimCalc_1 () [c(1:12), "RHDD"]) ,
            as.numeric (DF_ClimCalc_2 () [c(1:12), "RHDD"])
          )
      )
    })

    
    DF_Evaluation_1 <-
      reactive ({
        RemoveStringFromDFColNames (
          myDataFrame = 
            as.data.frame (myResultList_1 () ["DF_Evaluation"]),
          myStringToRemove = "DF_Evaluation."
        )
      })
    
    
    DF_Evaluation_2 <-
      reactive ({
        RemoveStringFromDFColNames (
          myDataFrame = 
            as.data.frame (myResultList_2 () ["DF_Evaluation"]),
          myStringToRemove = "DF_Evaluation."
        )
      })

    
    DF_AuxDataByMonth_1 <- 
      reactive({
        data.frame (
          MonthName = c ("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
          Temperature_HDD_Base = rep (input$Temperature_HDD_Base_1, 12)
        )
      })
    
    
    DF_StationInfo_1 <-
      reactive ({
        as.data.frame (
          myResultList_1 () ["DF_StationInfo"]
        ) 
      })
    
    
    DF_StationInfo_2 <-
      reactive ({
        as.data.frame (
          myResultList_2 () ["DF_StationInfo"]
        ) 
      })
    

    DF_FunctionArguments_1 <-
      reactive ({
        RemoveStringFromDFColNames (
          myDataFrame = as.data.frame (
            myResultList_1 () ["DF_FunctionArguments"]
            ),
          myStringToRemove = "DF_FunctionArguments."
        )
      })

    
    DF_FunctionArguments_2 <-
      reactive ({
        RemoveStringFromDFColNames (
          myDataFrame = as.data.frame (
            myResultList_2 () ["DF_FunctionArguments"]
          ),
          myStringToRemove = "DF_FunctionArguments."
        )
      })

  
    ######################################################################################X
    ## Output tables ------
          
    DF_OutputStructure_1 <-
      reactive ({
        as.data.frame (
          myResultList_1 () ["DF_OutputStructure"]
        ) 
      })
    
    
    DF_OutputStructure_2 <-
      reactive ({
        as.data.frame (
          myResultList_2 () ["DF_OutputStructure"]
        ) 
      })
    
    
    output$Table_ClimCalc_1 <-
      renderTable ({
        DF_ClimCalc_1 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_ClimCalc_2 <-
      renderTable ({
        DF_ClimCalc_2 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_Evaluation_1 <-
      renderTable ({
        DF_Evaluation_1 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_Evaluation_2 <-
      renderTable ({
        DF_Evaluation_2 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
  
    output$Table_StationInfo_1 <-
      renderTable ({
        DF_StationInfo_1 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_StationInfo_2 <-
      renderTable ({
        DF_StationInfo_2 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_FunctionArguments_1 <-
      renderTable ({
        DF_FunctionArguments_1 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_FunctionArguments_2 <-
      renderTable ({
        DF_FunctionArguments_2 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_OutputStructure_1 <-
      renderTable ({
        DF_OutputStructure_1 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )
    
    
    output$Table_OutputStructure_2 <-
      renderTable ({
        DF_OutputStructure_2 ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      )

        
    ResultTable_Year <-
      reactive ({
        as.data.frame (
          t (
            cbind (
              Format_DataFrameForOutput (
                # rbind (cbind ("2015-01", "2015-12"),
                #        cbind ("2000-01", "2020-12")),
                cbind (
                  rbind (
                    DF_Evaluation_1 () [
                      1,
                      c (
                        "Year"
                      )],
                    DF_Evaluation_2 () [
                      1,
                      c (
                        "Year"
                      )]
                  ),
                  rbind (
                    DF_Evaluation_1 () [
                      nrow (DF_Evaluation_1 ()),
                      c (
                        "Year"
                      )],
                    DF_Evaluation_2 () [
                      nrow (DF_Evaluation_2 ()),
                      c (
                        "Year"
                      )]
                  ),
                  rbind (
                    DF_Evaluation_1 () [
                      1,
                      c (
                        "Month"
                      )],
                    DF_Evaluation_2 () [
                      1,
                      c (
                        "Month"
                      )]
                  )
                ),
                myRowNames = c ("Klima.1", "Klima.2"),
                myColNames = c ("Year_Start", "Year_End", "Month_Start"),
                myDigits   = c (  0,    0)
              ),
              Format_DataFrameForOutput (
                rbind (
                  DF_ClimCalc_1 () [
                    13,
                    c (
                      "D",
                      "TA",
                      "HD",
                      "TA_HD",
                      "HDD",
                      "RHDD",
                      "CT",
                      "G_Hor",
                      "G_Hor_HD",
                      "G_E_HD",
                      "G_S_HD",
                      "G_W_HD",
                      "G_N_HD"
                    )],
                  DF_ClimCalc_2 () [
                    13,
                    c (
                      "D",
                      "TA",
                      "HD",
                      "TA_HD",
                      "HDD",
                      "RHDD",
                      "CT",
                      "G_Hor",
                      "G_Hor_HD",
                      "G_E_HD",
                      "G_S_HD",
                      "G_W_HD",
                      "G_N_HD"
                    )]
                ),
                myRowNames = c ("Klima 1", "Klima 2"),
                myColNames = c ("D", "TA", "HD", "TA_HD", "HDD", "RHDD","CT",
                                "G_Hor","G_Hor_HD","G_E_HD","G_S_HD", "G_W_HD", "G_N_HD"),
                myDigits   = c (  1,    2,    1,       2,     1,      1,   2,
                                  0, 0, 0, 0, 0, 0)
              )
            )
          )
          
        )
      })
      

    
    output$Table_Result_Year <-
      renderTable ({
        ResultTable_Year ()
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE,
      striped = TRUE,
      spacing = "xs"
      # width = "2000px"
      # digits = 0 # wirkt sich auf alle aus
      # digits = c (0, 0, 0, 1, 1, 1, 2) # keine Auswirkung
      )


    output$Table_HDD_Compact_1 <-
      renderTable ({
        Format_DataFrameForOutput (
          DF_ClimCalc_1 () [
            c(1:13),
            c (
              "Month", 
              "D",
              "TA",
              "HD",
              "TA_HD",
              "HDD",
              "RHDD",
              "CT"
            )],
          myColNames = c ("Month", "D", "TA", "HD", "TA_HD", "HDD", "RHDD", "CT"),
          myDigits   = c (      0,   1,    2,    1,       2,     1,     1,     2)
        )
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      # digits = 0 # wirkt sich auf alle aus
      # digits = c (0, 0, 0, 1, 1, 1, 2) # keine Auswirkung
      )

    
    output$Table_HDD_Compact_2 <-
      renderTable ({
        Format_DataFrameForOutput (
          DF_ClimCalc_2 () [
            c(1:13),
            c (
              "Month", 
              "D",
              "TA",
              "HD",
              "TA_HD",
              "HDD",
              "RHDD",
              "CT"
            )],
          myColNames = c ("Month", "D", "TA", "HD", "TA_HD", "HDD", "RHDD", "CT"),
          myDigits   = c (      0,   1,    2,    1,       2,     1,     1,     2)
        )
      },
      rownames = TRUE,
      align = "r",
      bordered = TRUE
      # digits = 0 # wirkt sich auf alle aus
      # digits = c (0, 0, 0, 1, 1, 1, 2) # keine Auswirkung
      )

    ######################################################################################X
    ## Output text
    
    
    output$Text_Result_RHDD_HDD_Ratio1to2 <- renderText ({

        if (input$Code_Type_DegreeDays == "RHDD") {
          paste0 (
            "RHDD: ",
            round (DF_ClimCalc_1 () [13, "RHDD"], 0), " / ",  
            round (DF_ClimCalc_2 () [13, "RHDD"], 0), " = ",
            round (DF_ClimCalc_1 () [13, "RHDD"] / DF_ClimCalc_2 () [13, "RHDD"], 2)
          )
        } else {
          paste0 (
            "HDD: ",
            round (DF_ClimCalc_1 () [13, "HDD"], 0), " / ",  
            round (DF_ClimCalc_2 () [13, "HDD"], 0), " = ",
            round (DF_ClimCalc_1 () [13, "HDD"] / DF_ClimCalc_2 () [13, "HDD"], 2)
          )
        } # End if else
             
      
    }) # End Text_Result_RHDD_HDD_Ratio1to2'

        
    output$Text_Result_G_S_HD_Ratio1to2 <- renderText ({
      
      paste0 (
        "G_S_HD: ",
        round (DF_ClimCalc_1 () [13, "G_S_HD"], 0), " / ",  
        round (DF_ClimCalc_2 () [13, "G_S_HD"], 0), " = ",
        round (DF_ClimCalc_1 () [13, "G_S_HD"] / DF_ClimCalc_2 () [13, "G_S_HD"], 2)
      ) # End paste0     
      
    }) # End Text_Result_G_S_HD_Ratio1to2'
    
    
    output$Text_Result_RHDD_HDD_Ratio2to1 <- renderText ({

        if (input$Code_Type_DegreeDays == "RHDD") {
          paste0 (
            "RHDD: ",
            round (DF_ClimCalc_2 () [13, "RHDD"], 0), " / ",  
            round (DF_ClimCalc_1 () [13, "RHDD"], 0), " = ",
            round (DF_ClimCalc_2 () [13, "RHDD"] / DF_ClimCalc_1 () [13, "RHDD"], 2)
          )
        } else {
          paste0 (
            "RHDD: ",
            round (DF_ClimCalc_2 () [13, "HDD"], 0), " / ",  
            round (DF_ClimCalc_1 () [13, "HDD"], 0), " = ",
            round (DF_ClimCalc_2 () [13, "HDD"] / DF_ClimCalc_1 () [13, "HDD"], 2)
          )
        } # End if else
      
    }) # End Text_Result_RHDD_HDD_Ratio2to1'
    
    
    output$Text_Result_G_S_HD_Ratio2to1 <- renderText ({
      
      paste0 (
        "G_S_HD: ",
        round (DF_ClimCalc_2 () [13, "G_S_HD"], 0), " / ",  
        round (DF_ClimCalc_1 () [13, "G_S_HD"], 0), " = ",
        round (DF_ClimCalc_2 () [13, "G_S_HD"] / DF_ClimCalc_1 () [13, "G_S_HD"], 2)
      ) # End paste0
      
      
    }) # End Text_Result_G_S_HD_Ratio2to1
    
    
    
    ######################################################################################X
    ## Plot chart -----

    
    y_Lim_Temperature <- reactive ({
      c (
        round (
          min (DF_ClimCalc_1 () [ , "TA"],
               DF_ClimCalc_2 () [ , "TA"],
               DF_Evaluation_1 () [ , "TA"],
               DF_Evaluation_2 () [ , "TA"],
               na.rm = TRUE),
          0
          ),
        round (
          max (DF_ClimCalc_1 () [ , "TA"],
               DF_ClimCalc_2 () [ , "TA"],
               DF_Evaluation_1 () [ , "TA"],
               DF_Evaluation_2 () [ , "TA"],
               na.rm = TRUE),
          0
          )
        )
      }
    )

    
    Chart_1 <- reactive ({

      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_point (
          data = DF_ClimCalc_1 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_1 () [1:12, "ID"],
            y = DF_ClimCalc_1 () [1:12, "TA"]
            ),
          colour = 'grey', 
          size = 1, 
          na.rm = TRUE,
          show.legend = TRUE
          ) +
        # ggplot2::geom_line (
        #   mapping = aes (x = (myDF_ClimCalc [1:12, "ID"]) ,
        #                  y = myDF_ClimCalc [1:12, "TA"], group = 1),
        #   colour = 'red', size = 0.1) +
        # ggplot2::geom_line (
        #   mapping = ggplot2::aes (
        #     x = 1:12,
        #     y = DF_ClimCalc_1 () [1:12, "TA"]
        #   ),
        #   colour = 'black', linewidth = 0.5) +
        ggplot2::scale_x_discrete (
          name = "Monat",
          labels = DF_ClimCalc_1 () [1:12, "Month"]) +
        ggplot2::ylab ("Temperatur [°C]") +
        ggplot2::geom_hline (yintercept=0) +
        ggplot2::theme (legend.position = c (0.5, 0.5)) +
        #ggplot2::theme (legend.position = "bottom")
        # labs (title="Monatsmittel und Monatsmittel an Heiztagen (blau)") +
        ggplot2::coord_cartesian (ylim = y_Lim_Temperature (), expand = TRUE)

    })


    Chart_2 <- reactive ({
      
      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_point (
          data = DF_ClimCalc_2 () [1:12, ],
          mapping = ggplot2::aes (
              x = DF_ClimCalc_2 () [1:12, "ID"],
              y = DF_ClimCalc_2 () [1:12, "TA"]
            ),
            colour = 'grey', 
            size = 1, 
            na.rm = TRUE,
            show.legend = TRUE
          ) +
        # ggplot2::geom_line (
        #   mapping = aes (x = (myDF_ClimCalc [1:12, "ID"]) ,
        #                  y = myDF_ClimCalc [1:12, "TA"], group = 1),
        #   colour = 'red', size = 0.1) +
        # ggplot2::geom_line (
        #   mapping = ggplot2::aes (
        #     x = 1:12,
        #     y = DF_ClimCalc_2 () [1:12, "TA"]
        #   ),
        #   colour = 'black', linewidth = 0.5) +
        ggplot2::scale_x_discrete (
          name = "Monat",
          labels = DF_ClimCalc_2 () [1:12, "Month"]) +
        ggplot2::ylab ("Temperatur [°C]") +
        ggplot2::geom_hline (yintercept=0) + 
        # labs (title="Monatsmittel und Monatsmittel an Heiztagen (blau)") +
        ggplot2::coord_cartesian (ylim = y_Lim_Temperature (), expand = TRUE)

      })
    
    
      output$TemperaturePlot_1 <- renderPlot ({

      Chart_1 () +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_1     () [ , "Index_EvalMonth"],
              y = DF_Evaluation_1     () [ , "TA"],
              group = DF_Evaluation_1 () [ , "Index_EvalYear"]
            ),
          colour = 'lightgrey', linewidth = 0.5, na.rm = TRUE) +   # 'lightgrey', linewidth = 0.01
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_1     () [ , "Index_EvalMonth"],
              y = DF_Evaluation_1     () [ , "TA_HD"],
              group = DF_Evaluation_1 () [ , "Index_EvalYear"]
            ),
          colour = 'lightblue', linewidth = 0.5, na.rm = TRUE) +  # 'lightblue', linewidth = 0.01
        ggplot2::geom_point (
          data = DF_ClimCalc_1 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_1 () [1:12, "ID"],
            y = DF_ClimCalc_1 () [1:12, "TA"]
          ),
          colour = 'black', 
          size = 3, 
          na.rm = TRUE, 
          show.legend = TRUE
          ) +
        ggplot2::geom_line (
          mapping = ggplot2::aes (
            x = 1:12,
            y = DF_ClimCalc_1 () [1:12, "TA"]
          ),
          colour = 'black', 
          linewidth = 0.5, 
          na.rm = TRUE, 
          show.legend = TRUE
          ) +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (DF_FunctionArguments_1 () [1, "Temperature_HDD_Base"])  
              #y = as.numeric (input$Temperature_HDD_Base_1)  
            ),
          colour = 'red', 
          linewidth = 1.0, 
          linetype = "dashed", 
          na.rm = TRUE, 
          show.legend = TRUE
          ) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (
                if (input$Code_Type_DegreeDays == "RHDD") {
                  as.numeric (DF_FunctionArguments_1 () [1, "Temperature_HDD_Room"])  
                  #input$Temperature_HDD_Room_1  
                } else {
                  as.numeric (DF_FunctionArguments_1 () [1, "Temperature_HDD_Base"])
                  #input$Temperature_HDD_Base_1
                }
                )  
            ),
          colour = 'red', 
          linewidth = 1.0, 
          linetype = "dotted", 
          na.rm = TRUE,
          show.legend = TRUE
          ) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_1     () [1:12, "TA_HD"]
            ),
          colour = 'blue', 
          linewidth = 1.0, 
          linetype = "dashed", 
          na.rm = TRUE, 
          show.legend = TRUE
          ) + 
        ggplot2::geom_point (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_1     () [1:12, "TA_HD"]
            ),
          colour = 'blue', 
          size = 3.0, 
          na.rm = TRUE, 
          show.legend = TRUE
          ) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c (1:12, 1:12),
              y = c (
                as.numeric (DF_ClimCalc_1     () [1:12, "TA_HD"]) ,
                rep (as.numeric (
                  if (input$Code_Type_DegreeDays == "RHDD") {
                    as.numeric (DF_FunctionArguments_1 () [1, "Temperature_HDD_Room"])
                    #input$Temperature_HDD_Room_1  
                  } else {
                    as.numeric (DF_FunctionArguments_1 () [1, "Temperature_HDD_Base"])
                    #input$Temperature_HDD_Base_1
                  }
                ), 12)
              ),
              group = c (1:12, 1:12)
            ),
          colour = 'green', 
          linewidth = 1.0, 
          na.rm = TRUE, 
          show.legend = TRUE
          )  +
        ggplot2::theme (legend.position = "bottom")
        
    })


    output$TemperaturePlot_2 <- renderPlot ({
      
      Chart_2 () +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_2     () [ , "Index_EvalMonth"],
              y = DF_Evaluation_2     () [ , "TA"],
              group = DF_Evaluation_2 () [ , "Index_EvalYear"]
            ),
          colour = 'lightgrey', linewidth = 0.5, na.rm = TRUE) + # 'lightgrey', linewidth = 0.01
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_2     () [ , "Index_EvalMonth"],
              y = DF_Evaluation_2     () [ , "TA_HD"],
              group = DF_Evaluation_2 () [ , "Index_EvalYear"]
            ),
          colour = 'lightblue', linewidth = 0.5, na.rm = TRUE) +   # 'lightgrey', linewidth = 0.01
        ggplot2::geom_point (
          data = DF_ClimCalc_2 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_2 () [1:12, "ID"],
            y = DF_ClimCalc_2 () [1:12, "TA"]
          ),
          colour = 'black', size = 3, na.rm = TRUE) +
        ggplot2::geom_line (
          mapping = ggplot2::aes (
            x = 1:12,
            y = DF_ClimCalc_2 () [1:12, "TA"]
          ),
          colour = 'black', linewidth = 0.5, na.rm = TRUE) +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (DF_FunctionArguments_2 () [1, "Temperature_HDD_Base"])
              #y = as.numeric (input$Temperature_HDD_Base_2)  
            ),
          colour = 'red', linewidth = 1.0, linetype = "dashed", na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (
                if (input$Code_Type_DegreeDays == "RHDD") {
                  as.numeric (DF_FunctionArguments_2 () [1, "Temperature_HDD_Room"])
                  #input$Temperature_HDD_Room_2  
                } else {
                  as.numeric (DF_FunctionArguments_2 () [1, "Temperature_HDD_Base"])
                  #input$Temperature_HDD_Base_2
                }
                )  
            ),
          colour = 'red', linewidth = 1.0, linetype = "dotted", na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_2     () [1:12, "TA_HD"]
            ),
          colour = 'blue', linewidth = 1.0, linetype = "dashed", na.rm = TRUE) + 
        ggplot2::geom_point (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_2     () [1:12, "TA_HD"]
            ),
          colour = 'blue', size = 3.0, na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c (1:12, 1:12),
              y = c (
                as.numeric (DF_ClimCalc_2     () [1:12, "TA_HD"]) ,
                rep (as.numeric (
                  if (input$Code_Type_DegreeDays == "RHDD") {
                    as.numeric (DF_FunctionArguments_2 () [1, "Temperature_HDD_Room"])
                    #input$Temperature_HDD_Room_2  
                  } else {
                    as.numeric (DF_FunctionArguments_2 () [1, "Temperature_HDD_Base"])
                    #input$Temperature_HDD_Base_2
                  }
                ), 12)
              ),
              group = c (1:12, 1:12)
            ),
          colour = 'green', linewidth = 1.0, na.rm = TRUE
          ) +
        ggplot2::theme (legend.position = c(0.07, 0.75))
        

    })
    
    
    # Both climates in one table 
    output$Table_ClimCalc_Both <- 
      renderTable (
        DF_ClimCalc_Both (),
        rownames = FALSE,
        align = "r",
        bordered = TRUE,
        #digits = c(0,0,0,2,2,0,0,0) # doesn't work
      ) 
  
    
    output$Plot_RHDD <- renderPlot ({
      
      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_col (
          data = DF_ClimCalc_Both () ,
          mapping = ggplot2::aes (
            x = Month,
            y = RHDD,
            fill = Analysis,
            group = Analysis
          ),
          position = "dodge",
          width = 0.5, 
          na.rm = TRUE,
          show.legend = TRUE
        ) + 
        ggplot2::scale_x_discrete (
          name = "Monat",
          limits = factor (DF_ClimCalc_1 () [1:12, "Month"]),
          labels = DF_ClimCalc_1 () [1:12, "Month"]) +
        ggplot2::ylab ("Gradtagzahl [Kd/a]") +
        ggplot2::scale_fill_brewer (palette = "Dark2") +
        ggplot2::theme (legend.position = c (
          0.07 + 0.93 *
            ((as.numeric (7 - (DF_ClimCalc_1 () [1, "Month"])) / 12) %% 1), 
          0.8
          )) +
        ggplot2::theme (legend.title = ggplot2::element_blank ())
      # ggplot2::theme (legend.position = c(0.07, 0.7))
      #+
      #ggplot2::theme (legend.position = "bottom")
      
    })
    
    
    output$Plot_HDD <- renderPlot ({
      
      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_col (
          data = DF_ClimCalc_Both () ,
          mapping = ggplot2::aes (
            x = Month,
            y = HDD,
            fill = Analysis,
            group = Analysis
          ),
          position = "dodge",
          width = 0.5, 
          na.rm = TRUE,
          show.legend = TRUE
        ) + 
        ggplot2::scale_x_discrete (
          name = "Monat",
          limits = factor (DF_ClimCalc_1 () [1:12, "Month"]),
          labels = DF_ClimCalc_1 () [1:12, "Month"]) +
        ggplot2::ylab ("Heizgradtage [Kd/a]") +
        ggplot2::scale_fill_brewer (palette = "Dark2") +
        ggplot2::theme (legend.position = c (
            0.07 + 0.93 *
              ((as.numeric (7 - (DF_ClimCalc_1 () [1, "Month"])) / 12) %% 1), 
            0.8
          )) +
        ggplot2::theme (legend.title = ggplot2::element_blank ())
      
          # ggplot2::theme (legend.position = "bottom")
      
    })
    

    output$Plot_HeatingDays <- renderPlot ({
      
      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_col (
          data = DF_ClimCalc_Both () ,
          mapping = ggplot2::aes (
            x = Month,
            y = HD,
            fill = Analysis,
            group = Analysis
          ),
          position = "dodge",
          width = 0.5, 
          na.rm = TRUE,
          show.legend = TRUE
        ) + 
        ggplot2::scale_x_discrete (
          name = "Monat",
          limits = factor (DF_ClimCalc_1 () [1:12, "Month"]),
          labels = DF_ClimCalc_1 () [1:12, "Month"]) +
        ggplot2::ylab ("Heiztage [d]") +
        ggplot2::scale_fill_brewer (palette = "Pastel2") +
        ggplot2::theme (legend.position = c (
            0.07 + 0.93 *
              ((as.numeric (7 - (DF_ClimCalc_1 () [1, "Month"])) / 12) %% 1), 
            0.8
          )) +
        ggplot2::theme (legend.title = ggplot2::element_blank ())
      
        #ggplot2::theme (legend.position = "bottom")

      }) # End render plot

    
    
    #######################################################################X
    ## Select and download data table  -----
    
    myOutputDataframe <- reactive ({
      
      switch (
        
        input$myOutputSelection,
        
        "ResultTable_Year" = ResultTable_Year (),
        
        "DF_ClimCalc_1"    = DF_ClimCalc_1 (),

        "DF_ClimCalc_2"    = DF_ClimCalc_2 (), 

        "DF_ClimCalc_Both" = DF_ClimCalc_Both (),
        
        "DF_Evaluation_1"  = DF_Evaluation_1 (),

        "DF_Evaluation_2"  = DF_Evaluation_2 (),

        # "Data.TA.HD"  = clidamonger::data.ta.hd
        # Doesn't work, too large?
        
      )
    })
    
    
    output$myTable <- 
      DT::renderDataTable  ({
          if (input$TransposeOutputTable == TRUE) {
            t (myOutputDataframe ()) 
          } else {
            myOutputDataframe ()
          }
        },
        extensions = 'Buttons', 
        options = 
          list (
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            lengthMenu = list(c(-1, 100, 50, 20), 
                              c('All', '100', '50', '20')),
            paging = T
            )
      )

    # Downloadable csv of selected dataset ----
    # Not used any more, DT::renderDataTable works better. 
    output$downloadData <- 
      downloadHandler (
        filename = function () {
          paste0 (
            input$myOutputSelection, 
            ".csv"
            )
        },
        content = function (file) {
          write.csv (
              myOutputDataframe ()
            , 
            file, 
            row.names = FALSE,
            dec = ",",
            sep = ";"
            )
        }
      )
    

    
    } # End Server function

#####################################################################################X
## RUN SHINYAPP
#####################################################################################X


  shinyApp (ui = ui, server = server)


  
  
  
## Note: Function CliDaMonDisplay () is currently disabled.   

        
#} # End definition of function CliDaMonDisplay ()



# CliDaMonDisplay (
#   Year_SelectionList_Last = 2023,
#   Year_Selected           = 2023,
#   Month_Selected          = 12
# )




