# Catch up 5

# experiment 1

## code 
/R/catchup_5/experiment_1/cluster_simulation_c5_1.R
/R/catchup_5/experiment_1/prediction_c5_1.R

## desc
cluster 5, Y 20, rows 2.5K

## result
Confusion Matrix and Statistics

          Reference
Prediction    1    2
         1 1077 2475
         2   34  417
                                          
               Accuracy : 0.3732          
                 95% CI : (0.3582, 0.3884)

# experiment 2

## code
/R/catchup_5/experiment_2/cluster_simulation_c5_1.R
/R/catchup_5/experiment_2/prediction_c5_1.R

## desc
20 Ys, same normal dist, same cut

## result
Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 147 153
         2   0   0
                                          
               Accuracy : 0.49            
                 95% CI : (0.4321, 0.5481)


# experiment 3

## code
/R/catchup_5/experiment_3/generate_ordinal_from_normal_dist.R
/R/catchup_5/experiment_3/cluster_simulation_c5_3.R
/R/catchup_5/experiment_3/prediction_c5_3.R

## desc
20 Ys, same normal dist, random cut

## result
Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 146 154
         2   0   0
                                          
               Accuracy : 0.4867          
                 95% CI : (0.4288, 0.5448)
