Overall Survival Prediction Model for mCRPC Patients
Authors: Spencer Evans, Daniel Yip, Amreet Ghuman

This project uses various machine learning models implemented in R to predict
the overall survival of metastatic castration resistant prostate cancer
patients.

The models are trained on a dataset acquired from the dreamproject.org. The
original dataset contains 200 clinical variables from over 1600 patients across
4 clinical trials. This data is reduced to a set of 39 covariates that are used
to train a linear discriminant analysis model for death prediction. Cox
proportional hazards is also used as a method to estimate the approximate time
to death of patients. Time to death is approximated at the point where survival
probability reaches 75%.

Project Structure:
FOLDER      CONTENTS<br>
data ------ Contains original dataset, and all processed data used for training
            in csv format. Also holds new data to be used for prediction.<br>
train ----- Contains the R scripts used for variable selection and training the
            models. The included scripts also provide evaluation metrics on the
            performance of each of the models.
models ---- Contains trained ML models saved in .rda format.
output ---- Stores the output of the models when making predictions on new data.
preproc --- Contains scripts used for preprocessing data.
