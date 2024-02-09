


library (shiny)
library (clidamonger)
library (CliDaMon)


#######################################################################X
## Functions -----


DataFrame_FixRows <- function (
    myDataFrame,
    nRowFix 
) {
  
  myDataFrame [ nrow (myDataFrame)+1 : nRowFix, ] <- NA
  
  return (myDataFrame)
  
}


# (colnames (ResultOfFunction$DF_ClimCalc))
# 
# View (ResultOfFunction$DF_ClimCalc)


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





TestFunction1 <- function (PostCode, Year_Start, Month_Start) {
  as.numeric (
    mean (c(PostCode, Year_Start, Month_Start))
  )  
}








## Murx / falsches Konzept, vielleicht kann's noch für irgendwas nützlich sein
#
# AuxTable_Months <- 
#   as.data.frame(
#     cbind (c ("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12", "Total"),
#            c ("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez", "Jahr")),
#   )
# colnames (AuxTable_Months) <- c ("ID", "Monat")


CalculateClimate <- function (
    input,
    Index_InputVersion = 1
) {
  
  
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
    Code_Type_Location  <- input$Code_Type_LocationBuilding_2
    PostCode            <- PostCode_2
    Code_ClimateStation <- Code_ClimateStation_2
    Year_Start          <- Year_Start_2
    Month_Start         <- Month_Start_2
    n_Year              <- n_Year_2
    Temperature_HDD_Base     <- as.numeric (input$Temperature_HDD_Base_2)
    Temperature_HDD_Room     <- as.numeric (input$Temperature_HDD_Room_2)
    #Degree_Inclination_Solar <- as.numeric (input$Degree_Inclination_Solar_2)
  } else {
    Code_Type_Location  <- input$Code_Type_LocationBuilding_1
    PostCode            <- PostCode_1
    Code_ClimateStation <- Code_ClimateStation_1
    Year_Start          <- Year_Start_1
    Month_Start         <- Month_Start_1
    n_Year              <- n_Year_1
    Temperature_HDD_Base     <- as.numeric (input$Temperature_HDD_Base_1)
    Temperature_HDD_Room     <- as.numeric (input$Temperature_HDD_Room_1)
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







# ClimateData <- function (
#     input,
#     myTableName = "DF_ClimCalc" # Further available tables: "DF_ClimCalc"
# ) {
#   
#   myResultTable <-
#       CliDaMon::ClimateByMonth (
#         myClimateData_PostCodes     = as.data.frame (clidamonger::tab.stationmapping),
#         myClimateData_StationTA     = as.data.frame (clidamonger::list.station.ta),
#         myClimateData_TA_HD         = as.data.frame (clidamonger::data.ta.hd),
#         myClimateData_Sol           = as.data.frame (clidamonger::data.sol),
#         myParTab_SolOrientEst       = as.data.frame (clidamonger::tab.estim.sol.orient),
#         Indicator_Type_LocationBuilding =
#           ifelse (input$Code_Type_LocationBuilding == "ID_Station", 2, 1),
#         Indicator_Type_AssignStationToPostcode =
#           ifelse (input$Code_Type_LocationBuilding == "Postcode_1Station", 1, 2),
#         PostCode = input$PostCode,
#         Code_ClimateStation         = input$Code_ClimateStation,
#         Indicator_ExcludeSelectedStation = 0,
#         Month_Start                 = input$Month_Start,
#         # Year_Start                  = input$RangeYear [1],
#         # n_Year                      = input$RangeYear [2] - input$RangeYear [1] + 1,
#         Year_Start                  = input$Year_Start,
#         n_Year                      = input$n_Year,
#         Temperature_HDD_Base        = as.numeric (input$Temperature_HDD_Base),
#         Temperature_HDD_Room        = as.numeric (input$Temperature_HDD_Room),
#         Degree_Inclination_Solar    = as.numeric (input$Degree_Inclination_Solar)
#       ) [myTableName]       
#   
#   
#   myResultTable <- 
#     as.data.frame (myResultTable)
# 
#   return ( t (myResultTable) )
#   
# }
#    
# 


#_ -----

#######################################################################X
## User Interface -----


ui <- fluidPage (
  
  # numericInput (
  #   inputId = "PostCode", 
  #   label = "Postleitzahl",
  #   value = 13469,
  #   min = 0,
  #   max = 99999,
  #   width = 100
  # ),
  # 
  # numericInput (
  #   inputId = "Code_ClimateStation", 
  #   label = "Nummer Klimastation",
  #   value = 917,
  #   min = 1,
  #   max = 99999,
  #   width = 100
  # ),
  # 
  
  
  titlePanel (
    title = strong ("IWU - Gradtagzahlen Deutschland - Shiny App"),
    windowTitle = "IWU - Gradtagzahlen Deutschland - Shiny App"
    ),
  
  
  
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
                strong ("Klima 1")
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
                    choices = c (1995:2023),
                    selected = 2021,
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
                      selected = 12,
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
              
              fluidRow (strong ("Klima 2")),
              
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
                      choices = c (1995:2023),
                      selected = 2021,
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
                      selected = 12,
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
              
              
                            
              
              
              
              
              
              

              
              
              

              

              
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
            ### 2024-01-26 - Version vor Umbau in zeilenweise Eingabefelder, 
            ### kann später gelöscht werden
            #
            #
            # #### Eingaben "Klima 1" 
            # 
            # fluidRow (strong ("Klima 1")),
            # 
            # br (),
            # 
            # # fluidRow (
            #   
            #   conditionalPanel (
            #     
            #     "(input.Code_Type_LocationBuilding_1 == 'Postcode_1Station') | 
            #     (input.Code_Type_LocationBuilding_1 == 'Postcode_3Stations')",
            #     
            #     column (6,
            #             "Postleitzahl",
            #             numericInput (
            #               inputId = "PostCode_1",
            #               label = NULL,
            #               # label = "Postleitzahl",
            #               value = 13469,
            #               min = 0,
            #               max = 99999,
            #               width = 100
            #             )
            #     )
            #     
            #   ),
            #   
            #   
            #   conditionalPanel (
            #     
            #     condition = 
            #       "(input.Code_Type_LocationBuilding_1 == 'ID_Station')",
            #     
            #     column (6,
            #             "Klimastation (Nr.)",
            #             numericInput (
            #               inputId = "Code_ClimateStation_1",
            #               label = NULL,
            #               # label = "Klimastation (Nr.)",
            #               value = 917,
            #               min = 1,
            #               max = 99999,
            #               width = 100
            #             )
            #             
            #     )
            #     
            #   )
            #   
            # ),
            # 
            # fluidRow (
            #   column (4, "Jahr"),
            #   column (4, "erster Monat"),
            #   column (4, "Anzahl Jahre")
            # ),
            # 
            # fluidRow (
            #   column (4,
            #           #strong ("Jahr"),
            #           selectInput (
            #             inputId = "Year_Start_1",
            #             label = NULL,
            #             # label   = "Jahr",
            #             choices = c (1995:2023),
            #             selected = 2021,
            #             width = 100)
            #   ),
            #   column (4,
            #           #strong ("erster Monat"),
            #           selectInput (
            #             inputId = "Month_Start_1",
            #             label = NULL,
            #             # label   = "Erster Monat des Zeitraums",
            #             choices = c (1:12),
            #             selected = 1,
            #             width = 100) 
            #   ),
            #   column (4,
            #           #strong ("Anzahl Jahre"),
            #           selectInput (
            #             inputId = "n_Year_1", 
            #             label = NULL,
            #             # label   = "Anzahl der Jahre",
            #             choices = c (1:20),
            #             selected = 1,
            #             width = 100)
            #   )
            #   
            # ),
            # 
            # fluidRow (
            #   
            #   column (4,
            #           "Temperatur Heizgrenze [°C]"
            #   ),
            #   
            #   conditionalPanel (
            #     
            #     condition = 
            #       "(input.Code_Type_DegreeDays == 'RHDD')",
            #     column (4,
            #             "Temperatur Raum [°C]"
            #     ),
            #     
            #   ), # END conditionalPanel
            #   
            #   
            #   column (4,
            #           "Solarstrahlung: Neigung Zusatzangabe in °"
            #   )
            # ),
            # 
            # fluidRow (
            #   
            #   column (4,
            #           # strong ("Heizgrenztemperatur [°C]"),
            #           
            #           selectInput (
            #             inputId = "Temperature_HDD_Base_1",
            #             label   = NULL,
            #             choices = c (10, 12, 15),
            #             selected = 12,
            #             width = 100)
            #   ),
            #   
            #   conditionalPanel (
            #     
            #     condition = 
            #       "(input.Code_Type_DegreeDays == 'RHDD')",
            #     
            #     column (4,
            #             
            #             # strong ("Raumtemperatur  [°C]"),
            #             numericInput (
            #               inputId = "Temperature_HDD_Room_1", 
            #               label = NULL,
            #               value = 20,
            #               min = 15,
            #               max = 25,
            #               width = 100
            #             )
            #             
            #     ),
            #     
            #   ), # END conditionalPanel
            #   
            #   
            #   column (4,
            #           # strong ("Solarstrahlung: Neigung der geneigten Fläche in °"),
            #           selectInput (
            #             inputId = "Degree_Inclination_Solar_1", 
            #             label   = NULL,
            #             choices = c (30, 45, 60),
            #             selected = 45,
            #             width = 100)
            #   )
            #   
            # ), # END fluidRow 
            # 
            # 
            
        #     
        #   
        # br (),
        #   
        # #### Eingaben "Klima 2"
        #   
        # fluidRow (strong ("Klima 2")),
        # 
        # br (),
        #     
        #     fluidRow (
        #       
        #       conditionalPanel (
        #         
        #         "(input.Code_Type_LocationBuilding_2 == 'Postcode_1Station') | 
        #          (input.Code_Type_LocationBuilding_2 == 'Postcode_3Stations')",
        # 
        #         column (6,
        #                 "Postleitzahl",
        #                 
        #                 numericInput (
        #                   inputId = "PostCode_2",
        #                   label = NULL,
        #                   # label = "Postleitzahl",
        #                   value = 13469,
        #                   min = 0,
        #                   max = 99999,
        #                   width = 100
        #                   )
        #                 
        #                 ),
        #         
        #         ),
        #       
        #       conditionalPanel (
        #         
        #         "(input.Code_Type_LocationBuilding_2 == 'ID_Station')",
        #         
        #         column (6,
        #                 "Klimastation (Nr.)",
        #                 
        #                 numericInput (
        #                   inputId = "Code_ClimateStation_2",
        #                   label = NULL,
        #                   # label = "Klimastation (Nr.)",
        #                   value = 917,
        #                   min = 1,
        #                   max = 99999,
        #                   width = 100
        #                   )
        #                 )
        #       
        #         ),
        #       
        #     ),
        #     
        #     fluidRow (
        #       column (4, "Jahr"),
        #       column (4, "erster Monat"),
        #       column (4, "Anzahl Jahre")
        #     ),
        #     
        #     
        #     fluidRow (
        #       column (4,
        #               #strong ("Jahr"),
        #               selectInput (
        #                 inputId = "Year_Start_2",
        #                 label = NULL,
        #                 # label   = "Jahr",
        #                 choices = c (1995:2023),
        #                 selected = 2002,
        #                 width = 100)
        #       ),
        #       column (4,
        #               #strong ("erster Monat"),
        #               selectInput (
        #                 inputId = "Month_Start_2",
        #                 label = NULL,
        #                 # label   = "Erster Monat des Zeitraums",
        #                 choices = c (1:12),
        #                 selected = 1,
        #                 width = 100) 
        #       ),
        #       column (4,
        #               #strong ("Anzahl Jahre"),
        #               selectInput (
        #                 inputId = "n_Year_2", 
        #                 label = NULL,
        #                 # label   = "Anzahl der Jahre",
        #                 choices = c (1:25),
        #                 selected = 20,
        #                 width = 100)
        #       ),
        #       
        #       
        #       fluidRow (
        #         
        #         
        #         column (4,
        #                 "Temperatur Heizgrenze [°C]"
        #         ),
        #         
        #         
        #         conditionalPanel (
        #           
        #           column (4,
        #                   "Temperatur Raum [°C]"
        #           ),
        #           
        #           condition = 
        #             "(input.Code_Type_DegreeDays == 'RHDD')",
        #           
        #         ) # END conditionalPanel
        #         
        #         
        #         
        #         
        #       ),
        #       
        #       fluidRow (
        #         
        #         
        #         
        #         column (4,
        #                 
        #                 # strong ("Heizgrenztemperatur [°C]"),
        #                 
        #                 selectInput (
        #                   inputId = "Temperature_HDD_Base_2",
        #                   label   = NULL,
        #                   choices = c (10, 12, 15),
        #                   selected = 12,
        #                   width = 100)
        #         ),
        #         
        #         
        #         conditionalPanel (
        #           
        #           condition = 
        #             "(input.Code_Type_DegreeDays == 'RHDD')",
        #           
        #           column (4,
        #                   
        #                   # strong ("Raumtemperatur  [°C]"),
        #                   
        #                   numericInput (
        #                     inputId = "Temperature_HDD_Room_2", 
        #                     label = NULL,
        #                     value = 20,
        #                     min = 15,
        #                     max = 25,
        #                     width = 100
        #                   ) 
        #           ),
        #           
        #         ) # END conditionalPanel
        #         
        # 
        #         
        #         ) # End fluidRow
        #       
        #       ) # End fluidRow
        # 
                  
                                        
                      
                  
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
    
              
             
              
              
              
              
              # column (3,
              #         strong ("Klima:")
              #         ),
              #
              #   column (2,
              #
              #           radioButtons (
              #             inputId = "Code_Type_LocationBuilding_1",
              #             label = "1",
              #             c ("." = "Postcode_1Station",
              #                "." = "Postcode_3Stations",
              #                "." = "ID_Station"),
              #             selected = "Postcode_3Stations"
              #           )
              #
              #   ),
              #
              #   column (7,
              #
              #           radioButtons (
              #             inputId = "Code_Type_LocationBuilding_2",
              #             label = "2",
              #             c (".  PLZ / 1 Station"     = "Postcode_1Station",
              #                ".  PLZ / 3 Stationen" = "Postcode_3Stations",
              #                ".  Nummer der Station" = "ID_Station"),
              #             selected = "Postcode_3Stations"
              #           )
              #
              #   ),

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


  br (),
  
        
  fluidRow (
    column (
      4,
      strong ("Result"),
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
  
  
  # fluidRow (
  #   column (6, 
  #           radioButtons (inputId = "Code_Type_LocationBuilding", label = "Art der Standortwahl:",
  #                         c ("Postleitzahl des Standorts - nächste Klimastation"     = "Postcode_1Station",
  #                            "Postleitzahl des Standorts - nächste 3 Klimastationen" = "Postcode_3Stations",
  #                            "Nummer der Klimastation" = "ID_Station")
  #           )
  #   ),
  #     column (6,
  #           numericInput (
  #             inputId = "PostCode", 
  #             label = "Postleitzahl",
  #             value = 13469,
  #             min = 0,
  #             max = 99999,
  #             width = 100
  #           ),
  #           numericInput (
  #             inputId = "Code_ClimateStation", 
  #             label = "Nummer Klimastation",
  #             value = 917,
  #             min = 1,
  #             max = 99999,
  #             width = 100
  #           ),
  #           
  #   )
  # ),
            
  # fluidRow (
  #   column (3,
  #             selectInput (
  #               inputId = "Year_Start",
  #               label   = "Jahr",
  #               choices = c (1995:2023),
  #               selected = 2015,
  #               width = 100)
  #           ),
  #   column (3,
  #             selectInput (
  #               inputId = "Month_Start",
  #               label   = "Erster Monat des Zeitraums",
  #               choices = c (1:12),
  #               selected = 1,
  #               width = 100) 
  #           ),
  #   column (3,
  #             selectInput (
  #               inputId = "n_Year", 
  #               label   = "Anzahl der Jahre",
  #               choices = c (1:20),
  #               selected = 1,
  #               width = 100)
  #           )
  #       
  #     ),
      
  # fluidRow (
  #   column (3,
  #           selectInput (
  #             inputId = "Temperature_HDD_Base",
  #             label   = "Heizgrenztemperatur [°C]",
  #             choices = c (10, 12, 15),
  #             selected = 12,
  #             width = 100)
  #   ),
  #   column (3,
  #           numericInput (
  #             inputId = "Temperature_HDD_Room", 
  #             label = "Raumtemperatur  [°C]",
  #             value = 20,
  #             min = 15,
  #             max = 25,
  #             width = 100
  #           ) 
  #   ),
  #   column (3,
  #           selectInput (
  #             inputId = "Degree_Inclination_Solar", 
  #             label   = "Solarstrahlung: Neigung der geneigten Fläche in °",
  #             choices = c (30, 45, 60),
  #             selected = 45,
  #             width = 100)
  #   )
  #   
  # ),
  

  
  # numericInput (
  #   inputId = "Year_Start", 
  #   label = "Jahr",
  #   value = 2020,
  #   min = 1995,
  #   max = 2050,
  #   width = 100
  # ),
  # 
  # numericInput (
    # inputId = "Month_Start",
    # label   = "Erster Monat des Zeitraums",
  #   value   = 1,
  #   min     = 1,
  #   max     = 12,
  #   width   = 100
  # ),
  # 
  # numericInput (
  #   inputId = "n_Year", 
  #   label   = "Anzahl der Jahre",
  #   value   = 1,
  #   min     = 1,
  #   max     = 20,
  #   width   = 100
  # ),
  # 
  # sliderInput (inputId = "RangeYears",
  #              label   = "Range of years",
  #              value = c (2015, 2020), min = 1995, max = 2023,
  #              width = 1000),
  
  
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



    
  # tableOutput ("DF_ClimCalc"),
  # 
  # tableOutput ("DF_Evaluation"),
    
  
  textOutput ("Text"),
  #verbatimTextOutput ("Text")
  
  
#   #  tableOutput ("TestAusgabe1"),
#   mainPanel (
#     tabsetPanel (
#       type = "tabs",
#       tabPanel ("Tab 1",
# #                tableOutput ("TestAusgabe1")
#       ),
#       tabPanel ("Tab 2",
# #                tableOutput ("TestAusgabe1")
#       )
#     )
#   )
  
  

  
  
    
)

#_ -----

#######################################################################X
## Sever Function -----


server <- function (input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  }) 
  # This prevents R from crashing when closing the Shiny app window.
  # The object "session" was added to the parameters of the server function. 
  
  
  
    # data <- reactive({
    #   rnorm(input$num)
    # })
    # output$hist <- renderPlot({
    #   hist(data())
    # })

    
    output$Text <- renderText ({
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


    output$TestAusgabe1 <-
      renderTable ({
        c (input$PostCode_1,
           input$Year_End_1,
           input$Month_End_1
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
    
    
    DF_ClimCalc_1 <-
      reactive ({
        as.data.frame (
          myResultList_1 () ["DF_ClimCalc"]
        )
      })
    
    DF_ClimCalc_2 <-
      reactive ({
        as.data.frame (
          myResultList_2 () ["DF_ClimCalc"]
        )
      })
    
    
    DF_ClimCalc_Both <-
      reactive ({
        data.frame (
            "Index" = c (1:24),
            "Month" = as.character (
              c (
                DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.Month"], 
                DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.Month"])),
            "Analysis" = c (rep ("Klima 1", 12), rep ("Klima 2", 12)),
            "TA" = c (
              as.numeric (DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.TA"]) ,
              as.numeric (DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.TA"])
            ),
            "TA_HD" = c (
              as.numeric (DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.TA_HD"]) ,
              as.numeric (DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.TA_HD"])
            ),
            "HD" = c (
              as.numeric (DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.HD"]) ,
              as.numeric (DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.HD"])
              ),
            "HDD" = c (
              as.numeric (DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.HDD"]) ,
              as.numeric (DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.HDD"])
            ),
            "RHDD" = c (
              as.numeric (DF_ClimCalc_1 () [c(1:12), "DF_ClimCalc.RHDD"]) ,
              as.numeric (DF_ClimCalc_2 () [c(1:12), "DF_ClimCalc.RHDD"])
            )
        )
      })

    
    
    ## Einen neu berechneten Vektor dazu zu packen geht nicht (Error in [: incorrect number of dimensions)
    #
    # DF_ClimCalc <-
    #   reactive ({
    #       as.data.frame (
    #         cbind (
    #           myResultList () ["DF_ClimCalc"] [ , ],
    #           myResultList () ["DF_ClimCalc"] [ , "DF_ClimCalc.D"] / 10 # max ( c(input$n_Year, 1), na.rm = TRUE) 
    #       ) 
    #     )
    #   })
    
    
    
    DF_Evaluation_1 <-
      reactive ({
        as.data.frame (
          myResultList_1 () ["DF_Evaluation"]
        ) 
      })
    
    DF_Evaluation_2 <-
      reactive ({
        as.data.frame (
          myResultList_2 () ["DF_Evaluation"]
        ) 
      })
    
    # DF_Evaluation_20years <-
    #   reactive ({
    #     DataFrame_FixRows (
    #       DF_Evaluation (),
    #       20 * 12) 
    #   })
     
    
    #myDF_Evaluation_20years [ nrow (myDF_Evaluation)+1 : (20 * 12), ] <- NA
    
    
    DF_AuxDataByMonth_1 <- 
      reactive({
        data.frame (
          MonthName = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
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
        as.data.frame (
          myResultList_1 () ["DF_FunctionArguments"]
        ) 
      })

    DF_FunctionArguments_2 <-
      reactive ({
        as.data.frame (
          myResultList_2 () ["DF_FunctionArguments"]
        ) 
      })

        
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
    
    
    # HIER WEITER - HABE NICHT HINBEKOMMEN DIE VARIABLENNAMEN AUFZUNEHMEN
    # 
    # Formatieren hat funktioniert, aber dann blieben die Spaltennamen weg 
    # Mir ist es nicht gelungen die Spaltennamen mit data.frame () oder as.data.frame zuzuordnen
    # 
    # unten steht die Ausgabe ohne Spaltenformatierung
    
    # output$Table_HDD_Compact <-
    #   renderTable ({
    #     data.frame (
    #         cbind (
    #           formatC (DF_ClimCalc () [ , "DF_ClimCalc.Month"], digits = 0, format ="f"),
    #           formatC (DF_ClimCalc () [ , "DF_ClimCalc.TA"],    digits = 2, format = "f")
    #         )
    #     )
    #     # DF_ClimCalc () [ , "DF_ClimCalc.Month"] <- formatC (DF_ClimCalc () [ , "DF_ClimCalc.Month"], digits = 0)
    #     # DF_ClimCalc () [ , "DF_ClimCalc.TA"]    <- formatC (DF_ClimCalc () [ , "DF_ClimCalc.TA"],    digits = 2)
    #     # c(1:12),
    #       # c (
    #       #   "DF_ClimCalc.Month", # Platzhalter für die Nummer und den Namen des Monats, muss noch in CliDaMon ergänzt werden 
    #       #   "DF_ClimCalc.D",
    #       #   "DF_ClimCalc.HDD",
    #       #   "DF_ClimCalc.HD",
    #       #   "DF_ClimCalc.TA",
    #       #   "DF_ClimCalc.TA_HD",
    #       #   "DF_ClimCalc.CT"
    #       # )]
    #   },
    #   rownames = TRUE,
    #   align = "r",
    #   bordered = TRUE
    #   # digits = 0 # wirkt sich auf alle aus 
    #   # digits = c (0, 0, 0, 1, 1, 1, 2) # keine Auswirkung
    #   )
    

    
    
        
    # Beispiel aus dem Internet
    # output$table <- renderTable({
    #   data$x <- formatC(data$x, digits = 0)
    #   data$z <- formatC(data$z, digits = 0)
    #   data$y <- formatC(data$y, digits = 3)
    #   data
    # })
    
    
    output$Table_Result_Year <-
      renderTable ({
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
                      "DF_Evaluation.Year"
                      )],
                  DF_Evaluation_2 () [
                    1,
                    c (
                      "DF_Evaluation.Year"
                    )]
                ),
                rbind (
                  DF_Evaluation_1 () [
                    nrow (DF_Evaluation_1 ()),
                    c (
                      "DF_Evaluation.Year"
                    )],
                  DF_Evaluation_2 () [
                    nrow (DF_Evaluation_2 ()),
                    c (
                      "DF_Evaluation.Year"
                    )]
                ),
                rbind (
                  DF_Evaluation_1 () [
                    1,
                    c (
                      "DF_Evaluation.Month"
                    )],
                  DF_Evaluation_2 () [
                    1,
                    c (
                      "DF_Evaluation.Month"
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
                    "DF_ClimCalc.D",
                    "DF_ClimCalc.TA",
                    "DF_ClimCalc.HD",
                    "DF_ClimCalc.TA_HD",
                    "DF_ClimCalc.HDD",
                    "DF_ClimCalc.RHDD",
                    "DF_ClimCalc.CT",
                    "DF_ClimCalc.G_Hor",
                    "DF_ClimCalc.G_Hor_HD",
                    "DF_ClimCalc.G_E_HD",
                    "DF_ClimCalc.G_S_HD", 
                    "DF_ClimCalc.G_W_HD", 
                    "DF_ClimCalc.G_N_HD"
                  )],
                DF_ClimCalc_2 () [
                  13,
                  c (
                    "DF_ClimCalc.D",
                    "DF_ClimCalc.TA",
                    "DF_ClimCalc.HD",
                    "DF_ClimCalc.TA_HD",
                    "DF_ClimCalc.HDD",
                    "DF_ClimCalc.RHDD",
                    "DF_ClimCalc.CT",
                    "DF_ClimCalc.G_Hor",
                    "DF_ClimCalc.G_Hor_HD",
                    "DF_ClimCalc.G_E_HD",
                    "DF_ClimCalc.G_S_HD", 
                    "DF_ClimCalc.G_W_HD", 
                    "DF_ClimCalc.G_N_HD"
                  )]
              ),
              myRowNames = c ("Klima 1", "Klima 2"),
              myColNames = c ("D", "TA", "HD", "TA_HD", "HDD", "RHDD","CT",
                "G_Hor","G_Hor_HD","G_E_HD","G_S_HD", "G_W_HD", "G_N_HD"),
              myDigits   = c (  1,    2,    1,       2,     1,      1,   2,
                      0, 0, 0, 0, 0, 0)
            )
          )
          # Format_DataFrameForOutput (
          #   rbind (
          #     DF_ClimCalc_1 () [
          #       13,
          #       c (
          #         "DF_ClimCalc.D",
          #         "DF_ClimCalc.TA",
          #         "DF_ClimCalc.HD",
          #         "DF_ClimCalc.TA_HD",
          #         "DF_ClimCalc.HDD",
          #         "DF_ClimCalc.RHDD",
          #         "DF_ClimCalc.CT"
          #       )],
          #     DF_ClimCalc_2 () [
          #       13,
          #       c (
          #         "DF_ClimCalc.D",
          #         "DF_ClimCalc.TA",
          #         "DF_ClimCalc.HD",
          #         "DF_ClimCalc.TA_HD",
          #         "DF_ClimCalc.HDD",
          #         "DF_ClimCalc.RHDD",
          #         "DF_ClimCalc.CT"
          #       )]
          #   ),
          #   myRowNames = c ("Klima 1", "Klima 2"),
          #   myColNames = c ("D", "TA", "HD", "TA_HD", "HDD", "RHDD", "CT"),
          #   myDigits   = c (  1,    2,    1,       2,     1,     1,     2)
          # )
        )
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
        Format_DataFrameForOutput(
          DF_ClimCalc_1 () [
            c(1:13),
            c (
              "DF_ClimCalc.Month", 
              "DF_ClimCalc.D",
              "DF_ClimCalc.TA",
              "DF_ClimCalc.HD",
              "DF_ClimCalc.TA_HD",
              "DF_ClimCalc.HDD",
              "DF_ClimCalc.RHDD",
              "DF_ClimCalc.CT"
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
        Format_DataFrameForOutput(
          DF_ClimCalc_2 () [
            c(1:13),
            c (
              "DF_ClimCalc.Month", 
              "DF_ClimCalc.D",
              "DF_ClimCalc.TA",
              "DF_ClimCalc.HD",
              "DF_ClimCalc.TA_HD",
              "DF_ClimCalc.HDD",
              "DF_ClimCalc.RHDD",
              "DF_ClimCalc.CT"
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
    

    ## Das funktioniert, hat nur kein Spaltenformat
    #
    # output$Table_HDD_Compact <-
    #   renderTable ({
    #     #        t (DF_ClimCalc () [c(1:12), c( # transponieren funktioniert so
    #     DF_ClimCalc () [
    #       c(1:12),
    #       c (
    #         "DF_ClimCalc.Month", # Platzhalter für die Nummer und den Namen des Monats, muss noch in CliDaMon ergänzt werden
    #         "DF_ClimCalc.D",
    #         "DF_ClimCalc.HDD",
    #         "DF_ClimCalc.HD",
    #         "DF_ClimCalc.TA",
    #         "DF_ClimCalc.TA_HD",
    #         "DF_ClimCalc.CT"
    #       )]
    #   },
    #   rownames = TRUE,
    #   align = "r",
    #   bordered = TRUE
    #   # digits = 0 # wirkt sich auf alle aus
    #   # digits = c (0, 0, 0, 1, 1, 1, 2) # keine Auswirkung
    #   )
    
    
    
    
    
        
    # 2023-12-08 das ging
    # 
    # myClimateResultTable <- reactive ({
    #   ClimateData (
    #     input = input,
    #     "DF_ClimCalc")
    # })
    #
    # output$DF_ClimCalc <-
    #   renderTable ({
    #     myClimateResultTable () [c("DF_ClimCalc.D", "DF_ClimCalc.TA") , c(1:12)]
    #   },
    #   rownames = TRUE,
    #   align = "r",
    #   bordered = TRUE
    #   )
    
    
    
    
    
    ## Das geht!!
    #
    # output$DF_ClimCalc <-
    #   renderTable ({
    #       ClimateData (
    #         input = input,
    #         "DF_ClimCalc") [c("DF_ClimCalc.D", "DF_ClimCalc.TA") , c(1:12)]
    #   },
    #   rownames = TRUE,
    #   align = "r",
    #   bordered = TRUE
    #   )
    
 
    # 2023-12-08 abgeschaltet
    #    
    # output$DF_Evaluation <-
    #   renderTable ({
    #       ClimateData (
    #         input = input,
    #         "DF_Evaluation")
    #   },
    #   rownames = TRUE,
    #   align = "r",
    #   bordered = TRUE
    #   )
    # 
    
    
    
    
    
    
    
     
   ## Plot chart 


  y_Lim_Temperature <- reactive ({
    c (
      round (
        min (DF_ClimCalc_1 () [ , "DF_ClimCalc.TA"],
             DF_ClimCalc_2 () [ , "DF_ClimCalc.TA"],
             DF_Evaluation_1 () [ , "DF_Evaluation.TA"],
             DF_Evaluation_2 () [ , "DF_Evaluation.TA"],
             na.rm = TRUE),
        0
      ),
      round (
        max (DF_ClimCalc_1 () [ , "DF_ClimCalc.TA"],
             DF_ClimCalc_2 () [ , "DF_ClimCalc.TA"],
             DF_Evaluation_1 () [ , "DF_Evaluation.TA"],
             DF_Evaluation_2 () [ , "DF_Evaluation.TA"],
             na.rm = TRUE),
        0
      )
    )
  })
    
    
    
    # ## Das funktioniert! 
    # weitere Aufteilung in ChartLayout (), Chart_Year () und Chart_LongTerm () hat nicht funktioniert.
    # Warning: Error in <reactive:Chart_Year>: Cannot add <ggproto> objects together
    # i Did you forget to add this object to a <ggplot> object?
      
    Chart_1 <- reactive ({

      ggplot2::ggplot () +
        ggplot2::theme_light () +
        ggplot2::geom_point (
          data = DF_ClimCalc_1 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.ID"],
            y = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.TA"]
            ),
          colour = 'grey', 
          size = 1, 
          na.rm = TRUE,
          show.legend = TRUE
          ) +
        # ggplot2::geom_line (
        #   mapping = aes (x = (myDF_ClimCalc [1:12, "DF_ClimCalc.ID"]) ,
        #                  y = myDF_ClimCalc [1:12, "DF_ClimCalc.TA"], group = 1),
        #   colour = 'red', size = 0.1) +
        # ggplot2::geom_line (
        #   mapping = ggplot2::aes (
        #     x = 1:12,
        #     y = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.TA"]
        #   ),
        #   colour = 'black', linewidth = 0.5) +
        ggplot2::scale_x_discrete (
          name = "Monat",
          labels = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]) +
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
              x = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.ID"],
              y = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.TA"]
            ),
            colour = 'grey', 
            size = 1, 
            na.rm = TRUE,
            show.legend = TRUE
          ) +
        # ggplot2::geom_line (
        #   mapping = aes (x = (myDF_ClimCalc [1:12, "DF_ClimCalc.ID"]) ,
        #                  y = myDF_ClimCalc [1:12, "DF_ClimCalc.TA"], group = 1),
        #   colour = 'red', size = 0.1) +
        # ggplot2::geom_line (
        #   mapping = ggplot2::aes (
        #     x = 1:12,
        #     y = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.TA"]
        #   ),
        #   colour = 'black', linewidth = 0.5) +
        ggplot2::scale_x_discrete (
          name = "Monat",
          labels = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.Month"]) +
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
              x = DF_Evaluation_1     () [ , "DF_Evaluation.Index_EvalMonth"],
              y = DF_Evaluation_1     () [ , "DF_Evaluation.TA"],
              group = DF_Evaluation_1 () [ , "DF_Evaluation.Index_EvalYear"]
            ),
          colour = 'lightgrey', linewidth = 0.01, na.rm = TRUE) +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_1     () [ , "DF_Evaluation.Index_EvalMonth"],
              y = DF_Evaluation_1     () [ , "DF_Evaluation.TA_HD"],
              group = DF_Evaluation_1 () [ , "DF_Evaluation.Index_EvalYear"]
            ),
          colour = 'lightblue', linewidth = 0.01, na.rm = TRUE) +
        ggplot2::geom_point (
          data = DF_ClimCalc_1 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.ID"],
            y = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.TA"]
          ),
          colour = 'black', 
          size = 3, 
          na.rm = TRUE, 
          show.legend = TRUE
          ) +
        ggplot2::geom_line (
          mapping = ggplot2::aes (
            x = 1:12,
            y = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.TA"]
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
              y = as.numeric (input$Temperature_HDD_Base_1)  
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
                  input$Temperature_HDD_Room_1  
                } else {
                  input$Temperature_HDD_Base_1
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
              y = DF_ClimCalc_1     () [1:12, "DF_ClimCalc.TA_HD"]
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
              y = DF_ClimCalc_1     () [1:12, "DF_ClimCalc.TA_HD"]
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
                as.numeric (DF_ClimCalc_1     () [1:12, "DF_ClimCalc.TA_HD"]) ,
                rep (as.numeric (
                  if (input$Code_Type_DegreeDays == "RHDD") {
                    input$Temperature_HDD_Room_1  
                  } else {
                    input$Temperature_HDD_Base_1
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
              x = DF_Evaluation_2     () [ , "DF_Evaluation.Index_EvalMonth"],
              y = DF_Evaluation_2     () [ , "DF_Evaluation.TA"],
              group = DF_Evaluation_2 () [ , "DF_Evaluation.Index_EvalYear"]
            ),
          colour = 'lightgrey', linewidth = 0.01, na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = DF_Evaluation_2     () [ , "DF_Evaluation.Index_EvalMonth"],
              y = DF_Evaluation_2     () [ , "DF_Evaluation.TA_HD"],
              group = DF_Evaluation_2 () [ , "DF_Evaluation.Index_EvalYear"]
            ),
          colour = 'lightblue', linewidth = 0.01, na.rm = TRUE) +
        ggplot2::geom_point (
          data = DF_ClimCalc_2 () [1:12, ],
          mapping = ggplot2::aes (
            x = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.ID"],
            y = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.TA"]
          ),
          colour = 'black', size = 3, na.rm = TRUE) +
        ggplot2::geom_line (
          mapping = ggplot2::aes (
            x = 1:12,
            y = DF_ClimCalc_2 () [1:12, "DF_ClimCalc.TA"]
          ),
          colour = 'black', linewidth = 0.5, na.rm = TRUE) +
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (input$Temperature_HDD_Base_2)  
            ),
          colour = 'red', linewidth = 1.0, linetype = "dashed", na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c(1:12),
              y = as.numeric (
                if (input$Code_Type_DegreeDays == "RHDD") {
                  input$Temperature_HDD_Room_2  
                } else {
                  input$Temperature_HDD_Base_2
                }
                )  
            ),
          colour = 'red', linewidth = 1.0, linetype = "dotted", na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_2     () [1:12, "DF_ClimCalc.TA_HD"]
            ),
          colour = 'blue', linewidth = 1.0, linetype = "dashed", na.rm = TRUE) + 
        ggplot2::geom_point (
          mapping =
            ggplot2::aes (
              x = 1:12,
              y = DF_ClimCalc_2     () [1:12, "DF_ClimCalc.TA_HD"]
            ),
          colour = 'blue', size = 3.0, na.rm = TRUE) + 
        ggplot2::geom_line (
          mapping =
            ggplot2::aes (
              x = c (1:12, 1:12),
              y = c (
                as.numeric (DF_ClimCalc_2     () [1:12, "DF_ClimCalc.TA_HD"]) ,
                rep (as.numeric (
                  if (input$Code_Type_DegreeDays == "RHDD") {
                    input$Temperature_HDD_Room_2  
                  } else {
                    input$Temperature_HDD_Base_2
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
          limits = factor (DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]),
          labels = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]) +
        ggplot2::ylab ("Gradtagzahl [Kd/a]") +
        ggplot2::scale_fill_brewer (palette = "Dark2") +
        ggplot2::theme (legend.position = c (
          0.07 + 0.93 *
            ((as.numeric (7 - (DF_ClimCalc_1 () [1, "DF_ClimCalc.Month"])) / 12) %% 1), 
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
          limits = factor (DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]),
          labels = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]) +
        ggplot2::ylab ("Heizgradtage [Kd/a]") +
        ggplot2::scale_fill_brewer (palette = "Dark2") +
        ggplot2::theme (legend.position = c (
            0.07 + 0.93 *
              ((as.numeric (7 - (DF_ClimCalc_1 () [1, "DF_ClimCalc.Month"])) / 12) %% 1), 
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
          limits = factor (DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]),
          labels = DF_ClimCalc_1 () [1:12, "DF_ClimCalc.Month"]) +
        ggplot2::ylab ("Heiztage [d]") +
        ggplot2::scale_fill_brewer (palette = "Pastel2") +
        ggplot2::theme (legend.position = c (
            0.07 + 0.93 *
              ((as.numeric (7 - (DF_ClimCalc_1 () [1, "DF_ClimCalc.Month"])) / 12) %% 1), 
            0.8
          )) +
        ggplot2::theme (legend.title = ggplot2::element_blank ())
      
        #ggplot2::theme (legend.position = "bottom")

    })
    
    
    
    
    
    
    
    
    # output$TemperaturePlot <- renderPlot ({
    #   
    #   ggplot2::ggplot (
    #     data = DF_ClimCalc () [1:12, c("DF_ClimCalc.ID", "DF_ClimCalc.TA")], 
    #     mapping = ggplot2::aes (x = DF_ClimCalc.ID)
    #   ) +
    #     ggplot2::geom_point (
    #       mapping = ggplot2::aes (y = DF_ClimCalc.TA),
    #       colour = 'red', size = 3) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (x = c(1:12), y = DF_ClimCalc () [1:12, "DF_ClimCalc.TA"]),
    #       colour = 'red', size = 0.1) + 
    #     ggplot2::theme_light () + 
    #     ggplot2::scale_x_discrete (
    #       name = "Months", 
    #       labels = DF_ClimCalc () [1:12, "DF_ClimCalc.Month"]) + 
    #     ggplot2::ylab ("Temperature [°C]") +
    #     ggplot2::geom_hline (yintercept=0) +
    #     ggplot2::geom_line (
    #       mapping = 
    #         ggplot2::aes (
    #           x = DF_Evaluation () [ , "DF_Evaluation.Index_EvalMonth"],
    #           y = DF_Evaluation () [ , "DF_Evaluation.TA"], 
    #           variable = paste0 ("Var", DF_Evaluation () [ , "DF_Evaluation.Index_EvalYear"]) 
    #           # variable = AuxFunctions::Format_Integer_LeadingZeros (
    #           #   myInteger = DF_Evaluation () [ , "DF_Evaluation.Index_EvalYear"], 
    #           #   myWidth = 2, 
    #           #   myPrefix = "Var")
    #           ),
    #       colour = 'grey', size = 0.05) 
    #   
    # })
    # 

        
    
    
    ## 2023-12-22: Geht, aber sehr umständlich, leere Daten bringen Meldungen 
    # 
    # output$TemperaturePlot <- renderPlot ({
    #   
    #   ggplot2::ggplot (
    #     data = DF_ClimCalc () [1:12, c("DF_ClimCalc.ID", "DF_ClimCalc.TA")], 
    #     mapping = ggplot2::aes (x = DF_ClimCalc.ID)
    #   ) +
    #     ggplot2::geom_point (
    #       mapping = ggplot2::aes (y = DF_ClimCalc.TA),
    #       colour = 'red', size = 3) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (x = c(1:12), y = DF_ClimCalc () [1:12, "DF_ClimCalc.TA"]),
    #       colour = 'red', size = 0.1) + 
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (1 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (2 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (3 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (4 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (5 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (6 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (7 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (8 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (9 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (10 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (11 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (12 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (13 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (14 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (15 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (16 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (17 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (18 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (19 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = ggplot2::aes (y = DF_Evaluation_20years () [(1:12) + (20 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::theme_light () + 
    #     ggplot2::scale_x_discrete (
    #       name = "Months", 
    #       labels = DF_ClimCalc () [1:12, "DF_ClimCalc.Month"]) + 
    #     ggplot2::ylab ("Temperature [°C]") +
    #     ggplot2::geom_hline (yintercept=0)   
      
    
     
    # output$TemperaturePlot <- renderPlot ({
    #   
    #   ggplot2::ggplot (
    #     data = DF_ClimCalc () [1:12, c("DF_ClimCalc.ID", "DF_ClimCalc.TA")], 
    #     mapping = aes (x = DF_ClimCalc.ID)
    #   ) +
    #     ggplot2::geom_point (
    #       mapping = aes (y = DF_ClimCalc.TA),
    #       colour = 'red', size = 3) +
    #     ggplot2::geom_line (
    #       mapping = aes (x = c(1:12), y = DF_ClimCalc () [1:12, "DF_ClimCalc.TA"]),
    #       colour = 'red', size = 0.1) + 
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (1 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (2 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (3 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (4 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (5 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (6 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (7 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (8 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (9 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (10 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (11 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (12 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (13 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (14 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (15 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (16 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (17 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (18 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (19 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::geom_line (
    #       mapping = aes (y = DF_Evaluation () [(1:12) + (20 - 1) * 12, "DF_Evaluation.TA"], group = 1),
    #       colour = 'grey', size = 0.05) +
    #     ggplot2::theme_light () + 
    #     ggplot2::scale_x_discrete (
    #       name = "Months", 
    #       labels = DF_ClimCalc () [1:12, "DF_ClimCalc.Month"]) + 
    #     ggplot2::ylab ("Temperature [°C]") +
    #     ggplot2::geom_hline (yintercept=0)   
      
      # ggplot2::ggplot() +
      #   ggplot2::geom_point (
      #     data = DF_ClimCalc () [1:12, c("DF_ClimCalc.Month", "DF_ClimCalc.TA")], 
      #     mapping = aes(x = DF_ClimCalc.Month, y = DF_ClimCalc.TA),
      #     colour = 'red', size = 3) +
      #   ggplot2::geom_line (data = DF_ClimCalc () [1:12, c("DF_ClimCalc.Month", "DF_ClimCalc.TA")], 
      #                       mapping = aes (x = DF_ClimCalc.Month, y = DF_ClimCalc.TA),
      #                       colour = 'red', size = 0.1)+ 
      #   ggplot2::theme_light () + 
      #   ggplot2::scale_x_discrete (
      #     name = "Month", 
      #     #breaks = c(1:12), 
      #     #labels, 
      #     limits = factor (c(1:12))) +
      #   ggplot2::ylab ("Temperature [°C]") +
      #   ggplot2::geom_hline (yintercept=0)  
      # 
      


    
    
    
    ## Plot mit R Base package, einfaches Diagramm funktioniert
    # 
    # output$TemperaturePlot <- renderPlot ({
    #   plot (
    #     c (1:12),
    #     # DF_ClimCalc () [1:3, 3]
    #     DF_ClimCalc () [1:12, "DF_ClimCalc.TA"]
    #   ) 
    # })
    # 
    
    
    
    
    ## das geht!!!
    #
    # output$TemperaturePlot <- renderPlot ({
    #   plot (
    #     c (1:12),
    #     # c (1:12)
    #     ClimateData (
    #       input = input,
    #       "DF_ClimCalc") ["DF_ClimCalc.TA", 1:12]
    #     
    #   ) 
    # })
    
    
    
    
    
    
    # output$TemperaturePlot <- renderPlot ({
    #   plot (
    #     c (1:12),
    #     ClimateData (
    #       input = input,
    #       "DF_ClimCalc") [1:12, "TA"]
    #     
    #   ) 
    # })
    # 
        
    # output$TemperaturePlot <- renderPlot ({
    #       plot (
    #         ClimateData (
    #           input = input,
    #           "DF_ClimCalc") [1:12, "ID"],
    #         ClimateData (
    #           input = input,
    #           "DF_ClimCalc") [1:12 ,"TA"]
    #         
    #       ) 
    #     })
    
    
    
    # geht:
    # output$DF_ClimCalc <- 
    #   renderTable ({
    #     CliDaMon::ClimateByMonth (
    #       myClimateData_PostCodes     = as.data.frame (clidamonger::tab.stationmapping),
    #       myClimateData_StationTA     = as.data.frame (clidamonger::list.station.ta),
    #       myClimateData_TA_HD         = as.data.frame (clidamonger::data.ta.hd),
    #       myClimateData_Sol           = as.data.frame (clidamonger::data.sol),
    #       myParTab_SolOrientEst       = as.data.frame (clidamonger::tab.estim.sol.orient),
    #       Indicator_Type_LocationBuilding = 
    #         ifelse (input$Code_Type_LocationBuilding == "ID_Station", 2, 1),
    #       Indicator_Type_AssignStationToPostcode = 
    #         ifelse (input$Code_Type_LocationBuilding == "Postcode_1Station", 1, 2),
    #       PostCode = input$PostCode,
    #       Code_ClimateStation = input$Code_ClimateStation,
    #       Indicator_ExcludeSelectedStation = 0,
    #       Month_Start                 = input$Month_Start,
    #       # Year_Start                  = input$RangeYear [1],
    #       # n_Year                      = input$RangeYear [2] - input$RangeYear [1] + 1,
    #       Year_Start                  = input$Year_Start,
    #       n_Year                      = input$n_Year,
    #       Temperature_HDD_Base = 15,
    #       Temperature_HDD_Room = 20,
    #       Degree_Inclination_Solar = 45
    #     )  ["DF_ClimCalc"] #                     ["DF_Evaluation"]               
    #   }, bordered = TRUE)
    # 
    
    
    
    ## Das funktioniert
    #
    # output$ClimateMonthlyOneYear <- 
    #   renderTable (t (input$PostCode : (input$PostCode+10)))
    
    
    
    
    ## geht nicht 
    # 
    # 
    # Temp1 <- reactive ({
    #   mean (input$PostCode, input$Year_Start, input$Month_Start)
    # })
    #
    #
    #
    # MyResult1 <- reactive ({
    #   average (input$Input_Postcode)
    # })
    # output$ClimateMonthlyOneYear <- 
    #   renderTable (MyResult)
    
    # output$ClimateMonthlyOneYear <- 
    #   renderTable (c(c(1,2),c(3,4)))
    
    
    
    
    ## Das folgende geht nicht 
    #
    # Label_Info_Result <- reactive ({
    #   paste0 ("Test: ", input$Code_Type_LocationBuilding)
    # })
    # output$Text <- renderText ({
    #     Label_Info_Result
    #   })
    
    # output$Text <- renderText ({
    #   paste0 (Label_Info_Result)
    #   })
    
}


  

shinyApp(ui = ui, server = server)






