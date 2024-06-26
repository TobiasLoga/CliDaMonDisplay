% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/app.R
\name{CliDaMonDisplay}
\alias{CliDaMonDisplay}
\title{Shiny App for calculating and displaying monthly climate data - heating degree days and solar radiation by postcode}
\usage{
CliDaMonDisplay(
  Year_SelectionList_Last = 2023,
  Year_SelectionList_First = 1995,
  Year_Selected = 2023,
  Month_Selected = 12
)
}
\arguments{
\item{Year_SelectionList_Last}{an integer indicating the last year to be included
in the selection list of the user interface. The input should correspond
to the data included in the used R data package 'clidamonger'.}

\item{Year_SelectionList_First}{an integer indicating the first year to be included
in the selection list of the user interface. The input should correspond
to the data included in the R data package 'clidamonger'. The default value is 1995.}

\item{Year_Selected}{an integer indicating the preselected year in the selection list.}

\item{Month_Selected}{an integer indicating the preselected momth in the selection list.}
}
\value{
Run the interactive shiny app.
The input and output is assigned to climate 1 (default: 1 year)
and to climate 2 (default: long term average).
The output dataframes for each of both climates are:

DF_ClimCalc:          a dataframe containing climate data for 12 months and the complete year.
If more than one year is evaluated the resulting values of each month
is the average of this month and the result data of the year
is an average year.

DF_Evaluation:        a dataframe containing climate data for all considered months.

DF_StationInfo:       a dataframe containing information about the used climate stations.

DF_FunctionArguments: a dataframe containing the values of all function arguments (one row).
}
\description{
Monthly and annual climate data are provided for specific periods
allocated to German postcode zone. The climate data can be used to calculate
the energy demand for space heating. Temperature and solar radiation during heating days
as well as degree days are provided for specific base temperatures (10°C, 12°C, and 15°C).

The package "CliDaMonDisplay" is using the data package 'clidamonger' containing monthly values for more than 800 German weather stations (starting from 1995) and the calculation package 'CliDaMon' for finding the three closest weather stations to a postcode and for calculating the degree days. The packages are based on the IWU Excel Workbook 'Gradtagzahlen-Deutschland' (available at https://www.iwu.de/publikationen/fachinformationen/energiebilanzen/gradtagzahltool/ - the algorithms are similar but not identical)
}
\examples{
CliDaMonDisplay (
   Year_SelectionList_Last = 2023,
   Year_Selected           = 2023,
   Month_Selected          = 12
   )


}
