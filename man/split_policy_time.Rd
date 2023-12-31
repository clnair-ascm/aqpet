% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_policy_time.R
\name{split_policy_time}
\alias{split_policy_time}
\title{Split Data Frame By Policy Time Periods}
\usage{
split_policy_time(df, start_times = NULL, end_times = NULL)
}
\arguments{
\item{df}{A data frame. It must contain a column named 'datetime'.}

\item{start_times}{A vector containing the start times of each policy period. Should be in POSIXct format or NULL. If NULL, the function will return the original data frame.}

\item{end_times}{A vector containing the end times of each policy period. Should be in POSIXct format or NULL. If NULL, the end time for each period will be set to the maximum datetime value in the data.}
}
\value{
A list of data frames, each representing a policy period. The names of the list elements are "df_policy_" followed by the
        policy period number. Any data outside the provided policy periods will be returned in a separate data frame labeled "df_out_of_period".
}
\description{
This function splits a data frame into separate data frames based on
given start and end times. Each resulting data frame represents a
policy period. Any data that falls outside of the provided policy
periods will be returned in a separate data frame labeled "out_of_period".
}
\examples{
\dontrun{
data <- data.frame(datetime = seq(as.POSIXct("2021-01-01"),
                                  as.POSIXct("2021-12-31"), by = "day"),
                   value = rnorm(365))
start_times <- as.POSIXct(c("2021-02-01", "2021-05-01", "2021-09-01"))
end_times <- as.POSIXct(c("2021-04-30", "2021-08-31", "2021-12-31"))
policy_periods <- split_policy_time(data, start_times, end_times)
}

}
\author{
[Yuqing Dai]
}
