# aqpet — An R package for air quality policy evaluation <img src="inst/aqpet.png" align="right" height="100" /></a>
'**aqpet**' is a toolkit that integrates auto-machine learning and augmented synthetic control method for air quality policy evaluation.
'**aqpet**' offers user-friendly functions for non-specialists for rapid assessment. It also provides advanced functions for data interpretation and expert analysis.
'**aqpet**' greatly streamlines the policy evaluation process, offering a time-efficient alternative to traditional process-based models.

First year available: 2023.

Hardware requirements: PC/Mac.

Program language: R.

## The conceptual framework of aqpet package.
![1-s2 0-S1364815224001130-gr1_lrg](https://github.com/clnair-ascm/aqpet/assets/43085757/0f508883-c4f7-4794-beaf-be144af574f5)

## Installation

Installation of **aqpet** from GitHub is convenient using the `{devtools}` package.

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github('clnair-ascm/aqpet')
library(aqpet)
load_envir()

# gfortran must be installed for 'bcp' package
on macOS:
brew install gcc
mkdir -p ~/.R
nano ~/.R/Makevars
Add the following lines to the Makevars file. Adjust paths if necessary:
FC = gfortran
F77 = gfortran
FLIBS = -lgfortran

```

## Description

Evaluating the effectiveness of clean air policies is important in the cycle of air quality management, ensuring policy accountability and informing future policy-making processes. However, such evaluations are challenging due to complex confounding factors such as varying weather conditions or seasonal or annual changes in air quality unrelated to the policy implementation. 

To address this challenge, we developed '**aqpet**', a package designed to streamline the quantification of policy effects on air quality using observational data. 

The package '**aqpet**' includes: 
(1) automated-machine learning to predict air pollutants under average weather conditions – a process term as "weather normalisation"; 
(2) augmented synthetic control method (ASCM) to quantify the actual policy impact on air pollution. 

'**aqpet**' offers functions for data collection and preparation, building auto-machine learning models, conducting weather normalisation, model performance evaluation and explanation, and causal impact analysis using ASCM. 'aqpet' enables fast, efficient, and interactive policy analysis for air quality management.

## **aqpet** Working flow chart

![1-s2 0-S1364815224001130-gr5_lrg](https://github.com/clnair-ascm/aqpet/assets/43085757/6b2cb0bf-8e08-4cf0-9b67-0c38c8839f81)

## Environment setup, data collection and import

### Prepare working environment

This function installs and loads the necessary packages.
It provides the user with an option to select their preferred CRAN mirror for package installations.

```r
load_envir()
```

### Read data set

Function ('read_data') identifies and processes data files from a specific directory. It requires a `params` argument, a list containing specific key-value pairs. 

• `data_dir`: the directory path from which the data files are to be retrieved. The function automatically corrects any backslashes to forward slashes in the path.

• `file_pattern`: files stored within the working directory, aqpet supports both the csv and xlsx formats.

• `datetime_format`: the format of the 'datetime' column in the files.

• `dependent_variable`: the designation for the dependent variable within the dataset generally corresponds to air pollutants. 

• `data_timerange`: a vector of two date elements specifying the range of dates to filter the data. It should cover enough data (e.g., one year) before and after policy interventions.

• `wenormed`: a logical value indicating if the data has been weather normalised.

```r
 params <- list(
    data_dir = "path/to/data/",
    file_pattern = "*.csv",
    datetime_format = "%Y-%m-%d %H:%M:%S",
    data_timerange = as.Date(c("2022-01-01", "2022-12-31")),
    dependent_variable = "target",
    deweathered = FALSE
 )

 result <- read_data(params)
 head(result$df_origin)
 }
```
Retrieve and load all RDS (R data storage) files with the appropriate format into a list. 

```r
# Assume there are two .rds files in the "data/" directory: "file1.rds" and "file2.rds".
params <- list(data_dir = "data/")
result <- read_rds(params)
print(names(result))  # Should output: "file1" "file2"
```

## Auto-ML and weather normalisation

### Auto-ML

**aqpet** leverages the capabilities of the H2O automated machine learning (H2O AutoML) engine (LeDell and Poirier, 2020), which provides a comprehensive suite of algorithms and computational tools. The **aqpet** function for building the model is 'autoMod(.)'. 

• `response_variable`&`predictor_variables`: A predictor variable is an independent variable used in statistical models to forecast or predict the values of a dependent response variable.

• `max_runtime_secs`: Setting an upper bound on the model's runtime to ensure computational efficiency.

• `max_models`: Capping the number of models to be generated. 

• `split_proportion`: Determining the division of data into training and evaluation subsets.

Below is an example that demonstrates how to build a GBM (Gradient Boosting Machine) model for NO2 predictions using time variables (e.g., trend, dow, doy) and meteorological variables including wd, ws, temp, and RH.

```r
autoMod(data,
  response_variable = “no2”,
  predictor_variables = c(time_variables, meteorological_variables),
  max_runtime_secs = 120,
  max_models = 10,
  split_proportion = 0.8,
  algorithm = “gbm”,
  criterion = “AUTO”
)
```
### Weather normalisation

In the **aqpet**, the weather normalisation adheres to the approach similar to that of Grange et al. (2018), yet it operates independently of the initial model construction phase — once the model is formulated through an automated machine learning procedure (i.e., `autoMod`), the weather normalisation function takes a step further.

This function, denoted as `wenorm`, is executed using the following syntax.

• `constant_variables`: Refers to the set of variables that are held fixed throughout the resampling process. 
• `num_interations`: Specifies the number of resampling iterations. 

```r
wenorm(data, model,
  response_variable = “no2”,
  predictor_variables = c(time_variables, meteorological_variables),
  constant_variables = time_variables,
  num_iterations = 1000
)
```

To streamline this workflow, the **aqpet** provides an ensemble function to efficiently process data from multiple sites:

• `list`: contains air quality and meteorological data across all sites.

```r
data_WeNorm < - buildMod(list, params)
```

## Model performance and interoperability

The `mod_stats` function offers a set of commonly used numeric model evaluation metrics. While it encompasses model statistics addressed by the openair package 'modStats' function, such as a factor or two (FAC2), normalised mean bias (NMB), root mean squared error (RMSE), the refined index of agreement (IOAr) (Willmott et al., 2012) — it further delineates systematic RMSE and unsystematic RMSE. 

• `mod`: A character string specifying the name of the model data column in `mydata`. Default is `mod`.

• `obs`: A character string specifying the name of the observed data column in `mydata`. Default is `obs`.

• `stats`: A character vector specifying the evaluation metrics to be computed. Available metrics include `n`, `FB`,`MG`, `FAC2`, `VG`, `r`, `RMSEs`, `RMSEu`, `RMSE`, `COE`, `IOA`, `IOAr`. If `default` is included in `stats` or if `stats` is not provided, all metrics will be computed. Default is `default`.


```r
mod_stats(data, mod = "mod", obs = "obs", stats = "default")
```


The **aqpet** provides the 'mod_explain' function, catering to various analytical requirements. As its core, this function is designed for evaluating and interpreting models, particularly those generated using the H2O framework (LeDell and Poirier, 2020). Here below is an example:

• `model`: The model that you want to explain.

• `data`: The data frame used for generating explanations. 

• `output`': The types of output you want to generate. This is a vector of character strings, where each string represents a different plot type. The default setting includes several types of output.
  
```r
mod_explain(model, data, output)
```

![1-s2 0-S1364815224001130-gr2_lrg](https://github.com/clnair-ascm/aqpet/assets/43085757/170c72c4-6e5f-4d3c-9495-27d0286ffccf)

Explanations for a Gradient Boosting Machine model for NO2 prediction at North Kensington, London: (a) variable importance, (b) partial dependence plot on "hour", (c) global SHAP values, and (d) local SHAP values.


## Application of ASCM
The synthetic control method (SCM) creates a predicted "business-as-usual" or "counterfactual" concentration of an air pollutant, which is a weighted average of its concentration of the control units. The Augmented Synthetic Control Method (ASCM) was developed to integrate a ridge regression model that formulates the estimator as a weighted average of control unit outcomes. It allows negative weights, facilitating an improved pre-treatment fit through controlled extrapolation (Ben-Michael et al., 2021).

The function for using the ASCM to construct counterfactual trends for treated units in relation to the control units is:


• `data`: A data frame comprising timeseries of weather-normalised data for both treated and control units.

• `params`: A list, which includes a vector of `treatment_group`, as well as `start_time` and `end_time` of the policy intervention. 

```r
a_scm (data, params)
```

## Model results and visualisation

Based on the `{ggplot2}`, **aqpet** provides a main function for timeseries plots that are suitable for different purposes:

```r
time_trend(data, time_resolution = "3 day", highlight = "North_Kensington")
```

![1-s2 0-S1364815224001130-gr3](https://github.com/clnair-ascm/aqpet/assets/43085757/1c0782b2-25be-4d55-a50b-9c2903af23fe)

Timeseries data of NO2 concentrations at monitoring sites across the UK. (a) Observed concentration; (b) Weather-normalised concentration. The red dashed line in each panel indicates the announcement date of the policy, the blue dashed line indicates the implementation date of the policy.


The ASCM results can be visualised through a series of plots by using the following function:

```r
ascm_trend(data, y_variable, add_ribbon = T, start_times)
```
![1-s2 0-S1364815224001130-gr4_lrg](https://github.com/clnair-ascm/aqpet/assets/43085757/ea811d57-a549-495d-ada8-afdf8cbb2a6f)

(a) Weather-normalised NO2 concentrations at North Kensington, London, and its synthetic trend generated from the control unit data. (b) Estimated policy effect of ULEZ on NO2 concentrations (difference in concentration between North Kensington and its synthetic control). (c) Estimated percentage change in NO2 concentration (positive means a decreases in concentration, the intended policy effect) due to ULEZ. The red dashed line in each panel indicates the announcement date of the policy, the blue dashed line indicates the implementation date of the policy, and the shaded areas represent the uncertainty.


#### Please find the published [paper](https://doi.org/10.1016/j.envsoft.2024.106052) for more details.


## References

- Ben-Michael, E.; Feller, A.; Rothstein, J.
  The Augmented Synthetic Control Method. Journal of the American Statistical Association; 2021; 116 (536): 1789–1803.

- Grange, S.K.; Carslaw, D.C.; Lewis, A.C.; Boleti, E.; Hueglin, C.
  Random forest meteorological normalisation models for Swiss PM 10 trend analysis. Atmospheric Chemistry and Physics; 2018; 18:6223-6239

- LeDell, E.; Poirier, S.
  H2o automl: Scalable automatic machine learning. Proceedings of the AutoML Workshop at ICML: ICML; 2020

- Willmott, C.J.; Robeson, S.M.; Matsuura, K.
  A refined index of model performance. International Journal of climatology; 2012; 32:2088-2094



