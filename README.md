# Niche-construction-affects-natural-selection

This repository contains the scripts to reproduce all results and plots in Clark, Deffner, Laland, Odling-Smee & Endler (2019): "Niche construction affects the variability and strength of natural selection".
Data and coding explanations can be found on Dryrad (DOI: https://doi.org/10.5061/dryad.g66n3h5).
The files "Temporal Analysis.r" and "Spatial Analysis.r" contain the code for our main analyses of tmeporal/spatial variability in natural selection. "DoubleHierPrep.r" prepares the data and calls the Stan file "DoubleHier.stan"
to run the double-hierarchical model described in the paper. "Magnitude Analysis" contains the code for the analysis of strength of selection. Finally, "Plotting code" lets you reproduce all figures contained in the paper.