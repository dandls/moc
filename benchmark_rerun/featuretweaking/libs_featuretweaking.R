### NEEDED PACKAGES ####

packages = c("jsonlite")

new.packages = packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(packages, require, character.only = TRUE)

if ("featureTweakR" %in% installed.packages()) {
    library("featureTweakR")
} else {
    devtools::install_github("https://github.com/katokohaku/featureTweakR.git")
}


if ("pforeach" %in% installed.packages()) {
    library("pforeach")
} else {
    devtools::install_github("https://github.com/hoxo-m/pforeach.git")
}

source("../helpers/libs_mlr.R")
source("helpers_evaluate.R")