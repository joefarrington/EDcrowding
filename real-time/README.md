# About this folder

This is a script to make real-time predictions

Currently all done in one file 

get-real-time-patients-from-Star.R

Needs as input:
- lab_orders_to_include (created by predict-admission/clean-lab-data)
- obs_to_include (created by predict-admission/clean-obs-data)
- saved features used by each learner (created by predict-admission/run-ML)
- learners themselves (created by predict-admission/run-ML)
- tta_prob - probability of being admitted in a certain number of hours
- poisson_means_not_yet_arrived - for time-varying arrivals


Saves
- predictions