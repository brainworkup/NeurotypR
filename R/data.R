#' CARS2 High Functioning Dataset
#'
#' A dataset containing scores from the Childhood Autism Rating Scale, Second Edition (CARS2)
#' for high-functioning individuals.
#'
#' @format A data frame with the following variables:
#' \describe{
#'   \item{test}{Test identifier, always "cars2_hf" for this dataset}
#'   \item{test_name}{Full name of the test, "CARS 2-HF"}
#'   \item{scale}{Name of the specific scale being measured}
#'   \item{raw_score}{Raw score for the scale}
#'   \item{score}{Processed score value}
#'   \item{ci_95}{95% confidence interval (if available)}
#'   \item{percentile}{Percentile rank (if available)}
#'   \item{range}{Descriptive range of the score}
#'   \item{domain}{Domain category, typically "Social Cognition"}
#'   \item{subdomain}{Subdomain category, typically "ASD/Asperger's"}
#'   \item{narrow}{Specific narrow category being assessed}
#'   \item{pass}{PASS model classification (if applicable)}
#'   \item{verbal}{Verbal component indicator (if applicable)}
#'   \item{timed}{Whether the assessment is timed (if applicable)}
#'   \item{test_type}{Type of test, typically "rating_scale"}
#'   \item{score_type}{Type of score, e.g., "raw_score" or "t_score"}
#'   \item{absort}{Unique identifier for the scale}
#'   \item{description}{Detailed description of what the scale measures}
#'   \item{result}{Interpretation of the results for the individual}
#' }
#' @source Childhood Autism Rating Scale, Second Edition (CARS2)
"cars2_hf"

#' Dot Counting Task from NIH EXAMINER
#' A dataset containing dot counting task data.
#' The variables are as follows:
#' raw age percentile n m md sd normValue
#' @format A data frame with several rows and 8 variables:
#' \describe{
#'   \item{raw}{raw score}
#'   \item{age}{age in years}
#'   \item{percentile}{percentile rank}
#'   \item{n}{sample size per group}
#'   \item{m}{mean score}
#'   \item{md}{median score}
#'   \item{sd}{standard deviation of score}
#'   \item{normValue}{T score fot dot counting task}
#' }
"dots"

#' neurocog dataset
#'
#' A dataset containing neuropsych data.
#'
#' @format A data frame with unknown rows and 26 or so variables:
#' \describe{
#'   \item{filename}{Name of test file.}
#'   \item{scale}{Age in years.}
#'   \item{score}{Percentile rank.}
#'   \item{percentile}{Sample size per group.}
#'   \item{range}{Mean score.}
#'   \item{ci_95}{Median score.}
#'   \item{raw_score}{Standard deviation of score.}
#'   \item{test}{Test name.}
#'   \item{test_name}{Specific name of the test.}
#'   \item{domain}{Domain of the test.}
#'   \item{subdomain}{Subdomain of the test.}
#'   \item{narrow}{Narrow category of the test.}
#'   \item{domain_broad}{Broad domain of the test.}
#'   \item{domain_test}{Specific test within the broad domain.}
#'   \item{test_type}{Type of the test.}
#'   \item{score_type}{Type of the score.}
#'   \item{timed}{Whether the test is timed.}
#'   \item{verbal}{Verbal/nonverbal score.}
#'   \item{pass}{PASS model.}
#'   \item{absort}{Absolute sort score.}
#'   \item{z}{Z-score.}
#'   \item{z_mean_dom}{Mean Z-score for domain.}
#'   \item{z_sd_dom}{Standard deviation of Z-score for domain.}
#'   \item{pct_mean_dom}{Mean percentile for domain.}
#'   \item{pct_sd_dom}{Standard deviation of percentile for domain.}
#'   \item{z_mean_sub}{Mean Z-score for subdomain.}
#'   \item{z_sd_sub}{Standard deviation of Z-score for subdomain.}
#'   \item{pct_mean_sub}{Mean percentile for subdomain.}
#'   \item{pct_sd_sub}{Standard deviation of percentile for subdomain.}
#'   \item{z_mean_narrow}{Mean Z-score for narrow category.}
#'   \item{z_sd_narrow}{Standard deviation of Z-score for narrow category.}
#'   \item{pct_mean_narrow}{Mean percentile for narrow category.}
#'   \item{pct_sd_narrow}{Standard deviation of percentile for narrow category.}
#'   \item{z_mean_verbal}{Mean Z-score for verbal score.}
#'   \item{z_sd_verbal}{Standard deviation of Z-score for verbal score.}
#'   \item{z_mean_pass}{Mean Z-score for passing score.}
#'   \item{z_sd_pass}{Standard deviation of Z-score for passing score.}
#'   \item{z_mean_timed}{Mean Z-score for timed score.}
#'   \item{z_sd_timed}{Standard deviation of Z-score for timed score.}
#' }
"neurocog"



#' pegboard_dom dataset
#'
#' A list of multiple data frames representing pegboard dominance data. Each data frame corresponds to a different age group.
#' There are 45 data frames in the list, each with 7 variables.
#'
#' @format A list of 45 data frames, each with the following 7 variables:
#' \describe{
#'   \item{raw}{Character vector of raw scores.}
#'   \item{norm}{Numeric vector of norm scores.}
#'   \item{percentile}{Numeric vector of percentile scores.}
#'   \item{lowerCI}{Numeric vector of lower confidence interval scores.}
#'   \item{upperCI}{Numeric vector of upper confidence interval scores.}
#'   \item{lowerCI_PR}{Numeric vector of lower confidence interval PR scores.}
#'   \item{upperCI_PR}{Numeric vector of upper confidence interval PR scores.}
#' }
#' @source \url{[INSERT SOURCE URL HERE]}
#' @references \url{[INSERT REFERENCES URL HERE]}
"pegboard_dom"
