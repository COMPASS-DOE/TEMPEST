# README.md

This folder contains

* Licor output files from 2021-2025, organized into two folders: `evan_licor_files` and `flood_events_licor_files`;
* Metadata for those measurement campaigns;
* A line-by-line list of measurements groups to process (`treeflux-processing-info.csv`)
* Temporary outputs (`temporary_data`) and final putput and diagnostics (`processing_outputs/`) folders

Two scripts in the root `R_scripts/` directory handle processing of the tree flux
datasets.

# `process_treeflux.R`

This script works its way through the rows in `treeflux-processing-info.csv`.
FOr each, it finds the requested [LI-7810](https://www.licor.com/products/trace-gas/LI-7810)
file, finds the relevant rows of metadata (i.e., originally entered as field
notes by the folks making the measurements), and combines the two, isolating the 
LI-7810 corresponding to the measurement period.

This script's outputs are written to `temporary_data`. Note that the script can
optionally skip (instead of overwrite) data already in this folder, giving you
the ability to restart or only do part of the 300+ rows. Visual diagnostics are
written to  `processing_outputs/`.

# `finalize_treeflux.R`

This script reads in all the `temporary_data` files; combines it with chamber
metadata providing volume, surface area, and species information; and computes
CH4 (nmol/m2/s) and CO2 (µmol/m2/s) fluxes. Output data and graphs  are
written to  `processing_outputs/`.
