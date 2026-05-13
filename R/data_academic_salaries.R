#' Academic Salaries Dataset for U.S. College Professors
#'
#' @description Real data on 9-month academic salaries for assistant professors,
#'   associate professors, and full professors at a U.S. college. This dataset
#'   is provided for educational purposes to demonstrate regression modeling,
#'   ANOVA, and logistic regression with \code{\link{paper_engine}}.
#'
#' @format A data frame with 397 observations and 7 variables:
#' \describe{
#'   \item{rank}{Factor with 3 levels: \code{"AsstProf"} (Assistant Professor),
#'     \code{"AssocProf"} (Associate Professor), \code{"Prof"} (Full Professor).
#'     Represents academic rank.}
#'   \item{discipline}{Factor with 2 levels: \code{"A"} (theoretical departments,
#'     e.g., mathematics, physics) and \code{"B"} (applied departments, e.g.,
#'     engineering, business). Represents academic discipline category.}
#'   \item{years_since_phd}{Numeric. Number of years since the faculty member
#'     earned their PhD.}
#'   \item{years_service}{Numeric. Number of years the faculty member has served
#'     at this institution.}
#'   \item{sex}{Factor with 2 levels: \code{"Female"} and \code{"Male"}.}
#'   \item{salary}{Numeric. Nine-month academic salary in U.S. dollars (2008-09
#'     academic year).}
#'   \item{high_earner}{Integer. Binary indicator (0 = No, 1 = Yes) marking
#'     faculty in the top 33\% of salaries. Created for logistic regression
#'     demonstrations.}
#' }
#'
#' @details
#' This dataset enables demonstration of OLSengine's three core methods:
#' \itemize{
#'   \item \strong{OLS Regression}: Modeling salary as a function of rank,
#'     discipline, experience, and sex to assess wage determinants and potential
#'     gender disparities.
#'   \item \strong{ANOVA}: Comparing mean salaries across academic ranks or
#'     disciplines.
#'   \item \strong{Logistic Regression}: Predicting the probability of being a
#'     high earner based on experience, rank, and discipline.
#' }
#'
#' The data were collected in the 2008-09 academic year and reflect institutional
#' salary structures at that time. Gender wage gap research in academia remains
#' an active area of inquiry (Ginther & Kahn, 2021).
#'
#' @source
#' This dataset is adapted from the \code{Salaries} dataset in the \pkg{carData}
#' package (Fox & Weisberg, 2019), which was originally compiled for the textbook
#' \emph{An R Companion to Applied Regression} (Fox & Weisberg, 2011). The
#' original data source is a U.S. college during the 2008-09 academic year.
#'
#' Licensed under GPL (>= 2), consistent with the \pkg{carData} package license.
#'
#' @references
#' Fox, J., & Weisberg, S. (2011). \emph{An R Companion to Applied Regression}
#' (2nd ed.). Thousand Oaks, CA: Sage.
#' \url{https://socialsciences.mcmaster.ca/jfox/Books/Companion/}
#'
#' Fox, J., & Weisberg, S. (2019). \emph{carData: Companion to Applied Regression
#' Data Sets}. R package version 3.0-3.
#' \url{https://CRAN.R-project.org/package=carData}
#'
#' Ginther, D. K., & Kahn, S. (2021). Women in academic science: A changing
#' landscape. \emph{Psychological Science in the Public Interest, 22}(1), 3-65.
#'
#' @examples
#' # Load the dataset
#' data(academic_salaries)
#'
#' # Explore structure
#' str(academic_salaries)
#' summary(academic_salaries)
#'
#' # OLS: Modeling salary determinants
#' ols_model <- paper_engine(
#'   salary ~ rank + discipline + years_since_phd + sex,
#'   data = academic_salaries,
#'   model = "ols",
#'   robust = "auto"
#' )
#' print(ols_model$tables$Table2_OLS_Estimation)
#' print(ols_model$messages)
#'
#' # ANOVA: Salary differences across academic ranks
#' anova_model <- paper_engine(
#'   salary ~ rank,
#'   data = academic_salaries,
#'   model = "anova"
#' )
#' print(anova_model$tables$Descriptive_Means)
#'
#' # Logit: Predicting high earner status
#' logit_model <- paper_engine(
#'   high_earner ~ years_since_phd + rank + discipline,
#'   data = academic_salaries,
#'   model = "logit"
#' )
#' print(logit_model$tables$Table2_Logit_Estimation)
#'
#' # Visualization
#' plot_engine(ols_model)
#'
#' @keywords datasets
"academic_salaries"