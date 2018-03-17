# Functional Clustering of Osteoarthritis Progression

These tools are adapted from the following publication:

James GM, Sugar CA. Clustering for Sparsely Sampled Functional Data. J Am Stat Assoc. 2003;98(462):397-408.

James GM, Hastie TJ. Functional linear discriminant analysis for irregularly sampled curves. J R Stat Soc Ser B Stat Methodol. 2001;63(3):533-550. 

James GM, Hastie TJ, Sugar CA. Principal component models for sparse functional data. Biometrika. 2000;87(3):587-602. 


Folder Contents:

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



