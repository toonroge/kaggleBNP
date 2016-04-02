library(ggplot2)
library(moments)
library(dplyr)
library(readr)
library(data.table)
library(corrplot)
library(gridExtra)
library(axaml)
library(shiny)
library(mi)
library(assertthat)
library(highcharter)
library(mgcv)

load("../../data/data.RData")


set.seed(1)

data0 = train
data_missing <- data0
data_missing$rand <- runif(nrow(data_missing))
data_missing <- subset(data_missing, rand < 0.005)
data_missing[, rand:=NULL]
data_missing <- missing_data.frame(data_missing) # image() on this class will give missing pattern plot

## Information about each column of the dataset
ci = axaml::info(DT = data0, global = F)

## types of variables
numeric_vars <- ci[(ci$R_TYPE %in% c("integer", "numeric")) &
                  !(ci$COL %in% c("ID","target"))]$COL
categorical_vars <- ci[(ci$R_TYPE %in% c("character"))]$COL

assertthat::are_equal(length(numeric_vars)+length(categorical_vars)+2,
                      ncol(train))

## axaml
dataAxaml = data.table(train)
dataAxaml$Exposure = 1
data.axaml(dataAxaml)
set_attributes(DT = dataAxaml, binaryTarget = "target", exposure = "Exposure")
st1 = stats(DT = dataAxaml, varnames = "v3", elements = "binaryTarget")
attr(st1$v3, "global")

