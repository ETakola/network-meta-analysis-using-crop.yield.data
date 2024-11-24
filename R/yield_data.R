#' @title yield data
#' @description Meta_analysis studies that assess published trials of interventions applied in the production of various crop yields. 
#' @format A data frame with 3733 rows and 15 variables:
#' \describe{
#'   \item{\code{DatasetID}}{character A character string containing the identification for each of the three meta-analytical studies that our data was retrieved from}
#'   \item{\code{Country}}{character A character string for the country in which each independent study was conducted}
#'   \item{\code{Study No.}}{double Study number}
#'   \item{\code{Source}}{character A character string with sources of each independent study}
#'   \item{\code{pr_yield_control_kgha}}{double effect sizes for control}
#'   \item{\code{control_SD}}{double standard deviation of the effect sizes}
#'   \item{\code{n_control}}{double number of samples}
#'   \item{\code{pr_yield_treatm_kgha}}{double effect sizes for treatment/intervention}
#'   \item{\code{treatment_SD}}{double standard deviation of the effect sizes}
#'   \item{\code{n_treatment}}{double number of samples}
#'   \item{\code{pr_Treatment}}{character name of intervention}
#'   \item{\code{pr_control}}{character control group (no intervention)}
#'   \item{\code{pr_Croptype}}{character the type of crop from which yield production values were assessed}
#'   \item{\code{pr_Longitude}}{double GPS coordinates of the location in which the study was conducted}
#'   \item{\code{pr_Latitude}}{double GPS coordinates of the location in which the study was conducted} 
#'}
#' @details DETAILS
"yield_data"