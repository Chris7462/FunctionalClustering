
 - originalData:
This folder contains text files with the WOMAC pain scores (pain.txt) and joint space width measurements (cartilage.txt) for all the subjects that were part of the incidence subcohort in the Osteoarthritis Initiative Study (http://www.oai.ucsf.edu/datarelease/). In each of these files, columns 1-8 contain data for the right knee and columns 9-16 contain data for the left knee, for years 0 - 7. NaNs represent missing data. 

AllPatientCharCleaned.txt contains all the data collected at the baseline visit (including medical history, clinical evaluations, nutritional information, etc.). Variables.pdf is the accompanying data dictionary provided by the OAI.

patientIDs.txt contains the subject IDs for all the other files (order aligned).


 - code:
This folder contains code and data for (1) functional clustering of OA progression and (2) cluster prediction. 

(1) The main files for functional clustering analysis are:
functionalClusteringCartilageOnly.R
functionalClusteringCartilageOnlyRightKnee.R
functionalClusteringCartilageOnlyLeftKnee.R
functionalClusteringPainOnly.R
functionalClusteringPainOnlyLeftKnee.R
functionalClusteringPainOnlyRightKnee.R

(2) The main files for cluster prediction are:
analyzePatientCharacteristicsCartilageOnly.R
analyzePatientCharacteristicsCartilageOnlyRightKnee.R 
analyzePatientCharacteristicsCartilageOnlyLeftKnee.R  
analyzePatientCharacteristicsPainOnly.R
analyzePatientCharacteristicsPainOnlyRightKnee.R 
analyzePatientCharacteristicsPainOnlyLeftKnee.R  


 - fclustResults:
This folder contains results from the functional clustering analysis.


 - predResults:
This folder contains results from the predictive analysis.


 - figure_code:
This folder contains code to generate good-looking figures using the saved rds files from previous analyses (saved in the folder figure_rds_files)



