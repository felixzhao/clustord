# Catchup 6

# experiment 1

## desc

- more cluster than 2, in this round try 3 clusters.
- fixed sum of pi is 1
- update alpha, with larger difference, c(1.5, 0, -1.5)
- sample size 2500
- number of Y is 20

## results

Confusion Matrix and Statistics

          Reference
Prediction    1    2    3
         1   77  567   62
         2    0   78 1301
         3  144   21    0

Overall Statistics
                                          
               Accuracy : 0.0689          
                 95% CI : (0.0588, 0.0801)
    No Information Rate : 0.6058      

## analysis

It looks like part of your problem is probably label switching. Try reordering the fitted clusters in order of decreasing alpha then do predictions based on the reordered clusters. (from Louise)

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
         1   3 153
         2 144   0
                                          
               Accuracy : 0.01            
                 95% CI : (0.0021, 0.0289)
    No Information Rate : 0.51            
    P-Value [Acc > NIR] : 1.0000          
                                          
                  Kappa : -0.9784  