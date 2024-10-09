# experiment 1

- 100 Ys  in Unif(-1,1)
- alpha (-2,-1,0,1,2)
- 5 clusters
- 2500 obs in each cluster
- default parameter as paper
- col effect simulation and prediction
- call funtion to generate prob matrix

## result

start at 24th Sep
end at 29th Sep

take 6 days to run the ClstOrd EM Algo

Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1 713  36   0   0   0
         2  35 680  18   0   0
         3   0  28 762  20   0
         4   0   0  19 682  36
         5   0   0   0  26 695

Overall Statistics
                                          
               Accuracy : 0.9419          
                 95% CI : (0.9339, 0.9491)
    No Information Rate : 0.2131          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.9273          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity            0.9532   0.9140   0.9537   0.9368   0.9508
Specificity            0.9880   0.9824   0.9837   0.9818   0.9914
Pos Pred Value         0.9519   0.9277   0.9407   0.9254   0.9639
Neg Pred Value         0.9883   0.9788   0.9874   0.9847   0.9881
Prevalence             0.1995   0.1984   0.2131   0.1941   0.1949
Detection Rate         0.1901   0.1813   0.2032   0.1819   0.1853
Detection Prevalence   0.1997   0.1955   0.2160   0.1965   0.1923
Balanced Accuracy      0.9706   0.9482   0.9687   0.9593   0.9711
Warning messages:
1: Quick-TRANSfer stage steps exceeded maximum (= 437500) 
2: Quick-TRANSfer stage steps exceeded maximum (= 437500) 
3: Quick-TRANSfer stage steps exceeded maximum (= 437500) 


# experiment 2

- 100 Ys
- 5 cluster
- 2500 obs in each cluster
- mean = -3,-1.5,0,1.5,3
- var = 1.5
- cut = -1, 1

## result

Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1 748   0   0   0   0
         2   0 744   0   0   0
         3   0   0 799   0   0
         4   0   0   0 728   0
         5   0   0   0   0 731

Overall Statistics
                                    
               Accuracy : 1         
                 95% CI : (0.999, 1)
    No Information Rate : 0.2131    
    P-Value [Acc > NIR] : < 2.2e-16 
                                    
                  Kappa : 1         
                                    
 Mcnemar's Test P-Value : NA        

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
Prevalence             0.1995   0.1984   0.2131   0.1941   0.1949
Detection Rate         0.1995   0.1984   0.2131   0.1941   0.1949
Detection Prevalence   0.1995   0.1984   0.2131   0.1941   0.1949
Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000

# experiment 3

- Replicate
- base on catchup 9 experiment 1
- use same parameters
- random generate seed 
- repeat experiment for 10 times
- calculate std(ACC) to indicate replicate

## comments

- code: cluster_simulation_c10_3.R and prediction_c10_3.R are same as catchup_c9_1
- replicate_c10_3.R is 
  - combine the simulation and prediction code,
  - skip save data to file
  - add std Acc calcuate

## code

/R/catchup_10/experiment_3/replicate_c10_3.R

## results

> print(accuracy_values)
 [1] 0.9500000 0.9733333 0.9533333 0.9666667 0.9566667 0.9650000 0.9583333 0.9583333 0.9466667 0.9516667

> # Print the standard deviation
> print(accuracy_std)
[1] 0.008270131