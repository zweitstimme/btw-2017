				 
# Replication archive for  "Zweitstimme.org. Ein strukturell-dynamisches Vorhersagemodell für Bundestagswahlen."        
										 
### published in Politische Vierteljahresschrift 58 (3): 418-442					                                                          
											 
### by Simon Munzert, Lukas F. Stoetzer, Thomas Gschwend, Marcel Neunhoeffer, Sebastian Sternberg 


We ran this code using `R 3.4.1` and `RStudio 1.0.153`. We last checked it on September 13, 2017.

The archive consists of three folders - code, data, and graphs. 

  1. Replication Archive/code. The folder contains the .R scripts required for estimation and visualization of the results as in the paper (both figures and tables).
	-  estimation.R - contains the code for the structural and dynamic models estimation. Results are saved in "Replication Archive/data/model_output".
	-  visualization.R - contains the code to reproduce Figures 1-7 and Tables 1 and 2 in the paper. Graphs and tables are saved in "Replication Archive/graphs".
	-  functions.R - includes all the functions used in both estimation and visualization. File is sourced in the main scripts.
	-  packages.R - includes all the packages used in both estimation and visualization. File is sourced in the main scripts.
	-  structural_model.jags - Structural model file
	-  polling-model.jags - Dynamic model file 
    
  2. Replication Archive/data. This folder contains the data required for estimation as well as models output.
 	- Replication Archive/data/model_input 
		- ger_model_df.RData - dataset with election data from 1949 - 2017 
		- polls_comb_results - dataset with poll results from seven agencies 
   - Replication Archive/data/model_output
		- structural_forecasts.rds - results of structural model estimation 
		- structural_jags_simulations.rds - results of structural model estimation 
		- structural_summary.rds -  results of structural model estimation, selected quintiles from draws 
		- Replication Archive/data/model_output/dynamic_model - contains the .rds files with results required for exact replication of the figures and tables 
      
  3. Replication Archive/graphs - the folder contains the figures and table data from the paper. Those are re-written if the model is re-estimated.

## More information

[http://zweitstimme.org/](http://zweitstimme.org/)

