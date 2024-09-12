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

## results

Confusion Matrix and Statistics

          Reference
Prediction   1   2
         1 388 264
         2  12 306
                                          
               Accuracy : 0.7155          
                 95% CI : (0.6859, 0.7437)