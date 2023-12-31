% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ml_scm.R
\name{ml_scm}
\alias{ml_scm}
\title{Machine Learning Enhanced Synthetic Control Method (ML-SCM)}
\usage{
ml_scm(df, params, cpd = F, window = 10, added_time = "unixtime", ...)
}
\arguments{
\item{df}{A dataframe containing the time series data.}

\item{params}{A list containing parameters that dictate the behavior of the model.}

\item{cpd}{Logical flag indicating if residual smoothing and decomposition should be done. Default is FALSE.}

\item{window}{Numeric value indicating the window size for smoothing. Default is 10.}

\item{added_time}{Character string specifying which date related columns to add. Default is "unixtime".}

\item{...}{Further arguments passed to other methods.}
}
\value{
A list containing the processed dataframe, differences between observed and predicted values, efficiency,
and the machine learning model used.
}
\description{
This function implements a variation of the synthetic control method (SCM) for time series data,
enhanced with machine learning for better prediction accuracy.
}
\details{
The function processes the input dataframe, optionally decomposing and smoothing the residuals.
The core of the function applies a machine learning model to predict outcomes and then computes the differences
between the actual and predicted values. The efficiency of the model is also calculated.
The function returns a list containing the processed dataframe, differences, efficiencies, and the machine learning model.
}
\examples{
\dontrun{
# Create a sample dataframe
df <- data.frame(
  datetime = seq(from = as.POSIXct("2020-01-01"), to = as.POSIXct("2020-01-10"), by = "hour"),
  metric1 = rnorm(240),
  metric2 = rnorm(240)
)

# Define params list
params <- list(
  treatment_group = "A",
  start_time = as.POSIXct("2020-01-03 00:00"),
  end_time = as.POSIXct("2020-01-07 23:00"),
  split_proportion = 0.7,
  max_models = 10,
  max_runtime_secs = 60,
  algorithm = "GBM",
  criterion = "RMSE"
)

results <- ml_scm(df, params, rsd = TRUE, window = 5)
}

}
\author{
[Yuqing Dai]
}
