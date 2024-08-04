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
