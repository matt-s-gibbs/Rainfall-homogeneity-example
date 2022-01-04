# Rainfall-homogeneity-example

This code produces the analysis undertaken in a draft manuscript.

# Required files

Three files are required:

- R/Main.R is the script to run, once the .Rproj file is opened in Rstudio.
- R/CalibRModel_Penrice.R has functions to do model calibration.
- GIS/A5050517Thiessen.csv has the area weightings to create catchment averaged rainfall and PET for input to the rainfall runoff modelling.

# Outputs
Files that are downloaded or output are saved to a number of folders:

- Discharge: the downloaded streamflow data
- SILO: the downloaded SILO Data
- Checks/Before: Result from running SILOCheckConsistency() on the original SILO data
- Checks/After: Result from running SILOCheckConsistency() after running SILOCorrectSite
- Outputs: Other outputs presented.
