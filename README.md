# irrkappa
SignLab R package with functions to calculate the inter-rater reliability of ELAN annotations

# How to install
This package can be installed using devtools.

If you do not already have devtools installed:
```
install.packages("devtools")
```

Then:
```
devtools::install_github("Amsterdam-Humanities-Labs/irrkappa")
```

A folder should be installed in your directory; navigate to the folder, set it as your working directory, then run this command to load all functions from the package:
```
devtools::load_all()
```
*After this step, don't forget to navigate back to your original working directory, where you may have your data files stored, and set it as your working directory again.

# Preview tutorial
A vignette is available in `vignette/tutorial.Rmd`. The analysis steps using the functions from the package can be found and run in the file.
