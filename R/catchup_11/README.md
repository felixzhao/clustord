# experiment 1

- 30 Ys
- 3 clusters
- sample size 1500
- same code and other parameters as catchup 9 exp 1-3

## code
./experiment_1/*

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2   3
         1 424 121   1
         2  21 304  69
         3   0  60 350

Overall Statistics
                                          
               Accuracy : 0.7985          
                 95% CI : (0.7761, 0.8196)
    No Information Rate : 0.3593          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.6981          
                                          
 Mcnemar's Test P-Value : 1.553e-15       

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity            0.9528   0.6268   0.8333
Specificity            0.8652   0.8960   0.9355
Pos Pred Value         0.7766   0.7716   0.8537
Neg Pred Value         0.9739   0.8107   0.9255
Prevalence             0.3296   0.3593   0.3111
Detection Rate         0.3141   0.2252   0.2593
Detection Prevalence   0.4044   0.2919   0.3037
Balanced Accuracy      0.9090   0.7614   0.8844


# experiment 2

- 30 Ys
- 5 clusters 
- sample size 2.5K
- same logic as previous expeirments

## code
./experiment_2/*

## result
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1 725 546 276  50   2
         2   0   0   0   0   0
         3  23 197 508 562 363
         4   0   0   0   0   0
         5   0   1  15 116 366

Overall Statistics
                                          
               Accuracy : 0.4264          
                 95% CI : (0.4105, 0.4424)
    No Information Rate : 0.2131          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.2786          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity            0.9693   0.0000   0.6358   0.0000   0.5007
Specificity            0.7089   1.0000   0.6120   1.0000   0.9563
Pos Pred Value         0.4534      NaN   0.3073      NaN   0.7349
Neg Pred Value         0.9893   0.8016   0.8612   0.8059   0.8878
Prevalence             0.1995   0.1984   0.2131   0.1941   0.1949
Detection Rate         0.1933   0.0000   0.1355   0.0000   0.0976
Detection Prevalence   0.4264   0.0000   0.4408   0.0000   0.1328
Balanced Accuracy      0.8391   0.5000   0.6239   0.5000   0.7285

# experiment 3

- 50 Ys
- 3 clusters
- sample size 1500
- same code and other parameters as catchup 9 exp 1-3

## code
./experiment_1/*

## results
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3
         1 417  30   0
         2  28 424  47
         3   0  31 373

Overall Statistics
                                         
               Accuracy : 0.8993         
                 95% CI : (0.882, 0.9148)
    No Information Rate : 0.3593         
    P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  Kappa : 0.8485         
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: 1 Class: 2 Class: 3
Sensitivity            0.9371   0.8742   0.8881
Specificity            0.9669   0.9133   0.9667
Pos Pred Value         0.9329   0.8497   0.9233
Neg Pred Value         0.9690   0.9283   0.9503
Prevalence             0.3296   0.3593   0.3111
Detection Rate         0.3089   0.3141   0.2763
Detection Prevalence   0.3311   0.3696   0.2993
Balanced Accuracy      0.9520   0.8938   0.9274


# experiment 4

- 50 Ys
- 5 clusters 
- sample size 2.5K
- same logic as previous expeirments

## code
./experiment_2/*

## result
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1 278  35   1   0   0
         2 428 389  88   2   0
         3  42 318 586 252  30
         4   0   2 121 423 354
         5   0   0   3  51 347

Overall Statistics
                                          
               Accuracy : 0.5395          
                 95% CI : (0.5234, 0.5555)
    No Information Rate : 0.2131          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.423           
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
Sensitivity           0.37166   0.5228   0.7334   0.5810  0.47469
Specificity           0.98801   0.8277   0.7824   0.8422  0.98211
Pos Pred Value        0.88535   0.4289   0.4772   0.4700  0.86534
Neg Pred Value        0.86321   0.8751   0.9155   0.8930  0.88534
Prevalence            0.19947   0.1984   0.2131   0.1941  0.19493
Detection Rate        0.07413   0.1037   0.1563   0.1128  0.09253
Detection Prevalence  0.08373   0.2419   0.3275   0.2400  0.10693
Balanced Accuracy     0.67983   0.6753   0.7579   0.7116  0.72840