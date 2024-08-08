# Catchup 6

# experiment 1

## desc

- more cluster than 2, in this round try 3 clusters.
- fixed sum of pi is 1
- update alpha, with larger difference, c(1.5, 0, -1.5)
- sample size 2500
- number of Y is 20
- after sort alpha, label switching resolved.

## results

Confusion Matrix and Statistics

          Reference
Prediction    1    2    3
         1    0   31 1213
         2   97  624  150
         3  124   11    0

Overall Statistics
                                          
               Accuracy : 0.2773          
                 95% CI : (0.2589, 0.2963)
    No Information Rate : 0.6058          
    P-Value [Acc > NIR] : 1               
                                          
                  Kappa : 0.0907          
                                          
 Mcnemar's Test P-Value : <2e-16          

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity           0.00000   0.9369   0.0000
Specificity           0.38689   0.8441   0.8478
Pos Pred Value        0.00000   0.7164   0.0000
Neg Pred Value        0.78032   0.9695   0.3556
Prevalence            0.09822   0.2960   0.6058
Detection Rate        0.00000   0.2773   0.0000
Detection Prevalence  0.55289   0.3871   0.0600
Balanced Accuracy     0.19345   0.8905   0.4239 

## analysis

- It looks like part of your problem is probably label switching. Try reordering the fitted clusters in order of decreasing alpha then do predictions based on the reordered clusters. (from Louise)
  - resolved, by sort alpha

- the acc still not idea


## c6 e1-2

### desc

- 5 clusters
- fixed label switching

### result

> print(conf_matrix)
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3
         1   0   0  15
         2   0   1 104
         3   9  65 383

Overall Statistics
                                          
               Accuracy : 0.6655          
                 95% CI : (0.6254, 0.7039)
    No Information Rate : 0.87            
    P-Value [Acc > NIR] : 1               
                                          
                  Kappa : -0.1546         
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity            0.0000 0.015152 0.762948
Specificity            0.9736 0.796477 0.013333
Pos Pred Value         0.0000 0.009524 0.838074
Neg Pred Value         0.9840 0.862288 0.008333
Prevalence             0.0156 0.114385 0.870017
Detection Rate         0.0000 0.001733 0.663778
Detection Prevalence   0.0260 0.181976 0.792028
Balanced Accuracy      0.4868 0.405815 0.388141

## c6 e1-3

### desc

- 8 clusters
- fixed label switching

### result

In Progress

# experiment 2

## iterator 1

### desc

Add col effect
- generate col effect beta by unif, with -20 to 0
- update formula
- add prediction to "clustord(Y~ROWCLUST+COL ..."

### results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1   0 281
         2 190 129
                                          
               Accuracy : 0.215           
                 95% CI : (0.1828, 0.2501)
    No Information Rate : 0.6833          
    P-Value [Acc > NIR] : 1         


## iterator 2

### desc

- same logic as iterator 1
- beta generate random unif, with 0 to 1

### results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1  83   3
         2 107 407
                                          
               Accuracy : 0.8167          
                 95% CI : (0.7833, 0.8468)

# experiment 3

## desc
2 cluster, 20 Ys and 1000 rows
ClstOrd include Col effect

### simulation steps

- each Ys in same cluster generated from same Normal Distrion with same argus
- from the inveistgation of two cluster dist and cuts
  - cluster 1, from N(0, 6)
  - cluster 2, from N(3, 8)
  - all cluster cuts at same value to 3 catgories, which is 0 and 2.5
- dataset combine all cluster dataframe to one

### prediction step

- Call ClstOrd with COL effect
- load col argus from ClstOrd result

### fix label switching

- after sort alpha (rowc), 2 cluster label switching solved.

## code

- dist plot: ./experment_3/two_normal_dist_plot.R
- cuts code invesigation: ./experment_3/invistigation_cut_normal_dist.R
- distribution_simulation: ./experment_3/cluster_dist_simulation_c6_3.R
- predict_dist: ./experment_3/prediction_c6_3.R

## data

../data/dist_simulation_y_20_c6_3.csv

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 144   0
         2   3 153
                                          
               Accuracy : 0.99            
                 95% CI : (0.9711, 0.9979)
    No Information Rate : 0.51            
    P-Value [Acc > NIR] : <2e-16          
                                          
                  Kappa : 0.98  

# expereiment 3 - 2

## desc

- same logic as e3
- two cluster, cut from Normal dist,
  - C1 ~ N(0,1)
  - C2 ~ N(4,2)
  - same cuts, 1, 2.5
- Acc:1, is this overfitting?

## code

- under
./experment_3/e3_2

## result

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 147   0
         2   0 153
                                     
               Accuracy : 1          
                 95% CI : (0.9878, 1)
    No Information Rate : 0.51       

# expereiment 3 - 3

## desc

- same logic as e3
- two cluster, cut from Normal dist,
  - C1 ~ N(-1,1)
  - C2 ~ N(1.5,2)
  - same cuts, -1, 1
- Acc:1, is this overfitting?

## code

- under
./experment_3/e3_3

## result

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 147   0
         2   0 153
                                     
               Accuracy : 1          
                 95% CI : (0.9878, 1)
    No Information Rate : 0.51       
    P-Value [Acc > NIR] : < 2.2e-16  
                                     
                  Kappa : 1          
                                     
 Mcnemar's Test P-Value : NA 
