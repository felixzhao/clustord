# During 489 Catch Up 4

in Catch up 4, Louise help me fixed this issue.
The reason is the PI times in wrong place, leading to PI^n be times.
That is why cluster with larger PI, tend to be predicted always in the code with this bug.

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 162  22
         2  28 388
                                          
               Accuracy : 0.9167          
                 95% CI : (0.8916, 0.9375)

# After 489 catch up 3

## summarise

- fininal accruay rate is higher, but due to all prediction is in one cluster.
- from confusion matrix, the model not given good result
            Reference
Prediction   1   2
         1   0   0
         2 190 410
                                          
         Accuracy : 0.6833 
- The reason is in probs matrix, all prob g2 is larger than g1 for all k value of y
  > probs
            [,1]      [,2]      [,3]
  [1,] 0.02720466 0.1266024 0.1155895
  [2,] 0.31550974 0.2579691 0.1571246

- Nothing improve in experiment with more clusters and categories.
- No improve with more clusters and categories and balance prob.

## simulation
R\catchup_3\cluster_simulation_c3_1.R

updates:
- normalized prob by cluster
- simulate data with 10 Y

## simulation data
data\simulation_categories_n_cluster_c3_1.csv # try 2 cluster and 3 categories
data\simulation_categories_n_cluster_c3_2.csv # try 3 cluster and 5 categories
data\simulation_categories_n_cluster_c3_3.csv # try 3 cluster and 5 categories, with balance prob.

## prediction

R\catchup_3\prediction_way_1_c3_1.R
R\catchup_3\prediction_way_1_c3_2.R
R\catchup_3\prediction_way_1_c3_3.R

updates:
- call clusOrd with 10 Y, rows cluster converges
- normalized logic align with simulation
- generate z matrix based on probs
  - each y get prob of each cluster from probs matrix
    e.g.
    prob matrix is
        g1,  g2
    k=1 0.1, 0.2  
    k=2 0.5, 0.6

    prob for each y_j is, j is col number
           g1, g2
    y1 = 1 0.1 0.2
    y2 = 2 0.5 0.6
    y3 = 1 0.1 0.2
  - multipling prob of cluster of each y_j to get z, due to all y are indepdent
    z vector is
       g1,          g2
    z  0.1*0.5*0.1, 0.2*0.6*0.2

    thus z vector is [0.005, 0.024]
- prediction based on z vector for one y_i, i is row
  predition of number of cluster = which.max(z)

# After 489 catch up 2

## simulation

R\data_samulation\cluster_simulation_3.R

### simulation output

data\simulation_categories_n_cluster_2.csv

include logic:
- normalise probs by pi

## prediction

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

R\prediction_baseline\prediction_4.R

logic:
- based on 3_1, updated prediction logic
  - replace select max prob cluster as prediction, 
  - replace with normalize category subset cluster probs
  - then, generate obs from this dist as prediction.