# simulation

R\data_samulation\cluster_simulation_3.R

# simulation output

data\simulation_categories_n_cluster_2.csv

include logic:
- normalise probs by pi

# prediction

R\prediction_baseline\prediction_3_1.R

include logic:
- load data from simulation output csv
- training and test split
- call clsord for clusting training dataset
- load clsord model parameters
- prediction, same logic as simulation
- eval,
  - accuracy
  - confusion matrix