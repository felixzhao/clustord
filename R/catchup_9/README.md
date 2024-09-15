# experiment 1

- 20 Ys
- default parameter as paper
- col effect simulation and prediction
- call funtion to generate prob matrix

## result

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 283   9
         2  15 293
                                          
               Accuracy : 0.96 


# experiment 2

- 30 Ys
- use same code as exp 1
- same parameters
- beta uni(30, -1, 1)

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 289   4
         2   9 298
                                          
               Accuracy : 0.9783          
                 95% CI : (0.9632, 0.9884)


# expereimnt 3

- 50 Ys
- same code as exp 1 & 2

## code
./experiment_1_2_3/*

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 297   0
         2   1 302
                                     
               Accuracy : 0.9983     
                 95% CI : (0.9907, 1)
                 
# experiment 4

- 20 Ys
- 3 clusters
- sample size 1500
- same code and other parameters as previous exps

## code
./experiment_4/*

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2   3
         1 334  63   0
         2 110 383 152
         3   1  39 268

Overall Statistics
                                          
               Accuracy : 0.7296          
                 95% CI : (0.7051, 0.7532)


# experiment 5

- 20 Ys
- 5 clusters 
- sample size 2.5K
- same logic as e4

## code
./experiment_5/*

## result

Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5
         1   0   0   0   0   0
         2 505 201  69   6   0
         3   0   0   0   0   0
         4 243 543 730 722 731
         5   0   0   0   0   0

Overall Statistics
                                          
               Accuracy : 0.2461          
                 95% CI : (0.2324, 0.2602)

# experiment 6

- 20 Ys
- 8 clusters 
- sample size 5K
- nstart 20
- startEMcycles 2

## code
./experiment_6/*

## result

Confusion Matrix and Statistics

          Reference
Prediction    1    2    3    4    5    6    7    8
         1 1410 1227 1077  850  328  153   71   31
         2    0    0    0    0    0    0    0    0
         3    0    0    0    0    0    0    0    0
         4    0    0    0    0    0    0    0    0
         5    0    0    0    0    0    0    0    0
         6    0    0    0    0    0    0    0    0
         7  100  208  404  668  898  848  664  506
         8    2    4   19   59  265  482  751  975

Overall Statistics
                                         
               Accuracy : 0.2541         
                 95% CI : (0.2463, 0.262)
    No Information Rate : 0.1314         
    P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  Kappa : 0.1473         
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8
Sensitivity            0.9325   0.0000    0.000   0.0000   0.0000   0.0000  0.44684  0.64484
Specificity            0.6437   1.0000    1.000   1.0000   1.0000   1.0000  0.65456  0.84916
Pos Pred Value         0.2739      NaN      NaN      NaN      NaN      NaN  0.15456  0.38131
Neg Pred Value         0.9851   0.8801    0.875   0.8686   0.8758   0.8764  0.89330  0.94313
Prevalence             0.1260   0.1199    0.125   0.1314   0.1242   0.1236  0.12383  0.12600
Detection Rate         0.1175   0.0000    0.000   0.0000   0.0000   0.0000  0.05533  0.08125
Detection Prevalence   0.4289   0.0000    0.000   0.0000   0.0000   0.0000  0.35800  0.21308
Balanced Accuracy      0.7881   0.5000    0.500   0.5000   0.5000   0.5000  0.55070  0.74700