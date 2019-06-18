# Niche-construction-affects-natural-selection

This repository contains the scripts to reproduce all results and plots in Clark, Deffner, Laland, Odling-Smee & Endler (2019): "Niche construction affects the variability and strength of natural selection".
Data and coding explanations can be found on Dryad (DOI: https://doi.org/10.5061/dryad.g66n3h5).
The files "Temporal Analysis.r" and "Spatial Analysis.r" contain the code for our analyses of temporal/spatial variability in natural selection including all robustness checks. "DoubleHierPrep.r" prepares the data and calls the Stan file "DoubleHier.stan"
to run the double-hierarchical model described in the paper. "Magnitude Analysis" contains the code for the analysis of the strength of selection. Finally, "Plotting code" lets you reproduce all figures in the paper.