Sources and data of the CoWAT coupled model
# Principle
The CoWAT model (Coupling Water and Agent Trajectories) has been developped by Bastien Richard, Bruno Bonté, Julien Veyssier, Isabelle braud and Olivier Barreteau during the Radhy Buëch project to represent irrigation and hydrology in the Buëch river catchment in southern France. 

It uses R statistical software, the jams plateforme (http://jams.uni-jena.de/) and the Cormas plateforme (http://cormas.cirad.fr/). It couples the J2K-Buëch jams model and the WatAsit Cormas model.

# Instalation of CoWAT model on your computer
You need to follow all the following steps to use the model on your computer.

## Create a directory COWAT-ALL
- To simplify the operation we will install WatASit, Jams-J2k, data and Rscripts in the same place on your computer in a directory named COWAT-ALL

## Get the last release 
- Go to the "Releases" entry in the left menu: (Project overview/Releases) and download the last release in the COWAT-ALL directory

## Install Cormas 2020 and load Communication Add-ons
- Download cormas 2020 and unzip it on your computer in the COWAT-ALL directory (http://cormas.cirad.fr/logiciel/cormas2020_package.zip)
- Copy the watasitrcoupler-master/cormas-add-ons folder in the cormas2020/Add-ons folder
- Open Cormas (cormas2020/ and "wine cormas.exe" (terminal linux) or cormas.exe (windows)) and load all add-ons (tools/Add-ons/add-on_manager) and then click "select all" and "Load-Close".
- Save cormas software: open the main interface of visual works (clik tools/open_vw_main_interface) and save it (File/Save_image)
- Exit cormas and visual works (Cormas interface: File -> Exit)

## Get the right version of the COWAT model
- Download the right version of the WatASit model: (https://gitlab.irstea.fr/watasitdev/watasit/-/releases) 
- Unzip it in the cormas model folder (cormas2020/Model/) under the name COWAT. Your pcl file must be at the following location on your disk:  COWAT-ALL/cormas2020/Models/COWAT/WatASit.pcl

## Set up your configuration file
- If you followed the instructions above, modify the config file rcoupler_template.cfg at the root of watasitrcoupler folder as the follows and rename it: rcoupler.cfg
 \[tools\]
\# accept absolute path or relative ones (relative to this config file location)
jamsRoot = superjams
cormasRoot = ../cormas2020
- if you use another install of Cormas (be carefull you need to add add-ons to this version if you do so), replace the line "cormasRoot = ../cormas2020" by the correct absolute path of cormas on your computer.

## Install RStudio and packages
- Install the version of RStudio corresponding to the watasitRcoupler release.
- Install all required R packages (Rstudio/packages/install)


# Run the model
- Open the WatASit_Rcoupler project in RSutdio (click on WatASit_Rcoupler.Rproj)
- Source the Rcoupler\[WatASit=J2K\].R file

