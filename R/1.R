set.seed(1)
long.df <- data.frame(Y=factor(sample(1:3,5*20,replace=TRUE)),
                      ROW=factor(rep(1:20,times=5)),COL=rep(1:5,each=20))
#'
# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
clustord(Y~ROWCLUST,model="OSM",3,long.df=long.df,
         EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)
# #'
# # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*(rowc_coef_r + col_coef_j) with 3 row clustering groups:
# clustord(Y~ROWCLUST+COL,model="OSM",3,long.df=long.df,
#          EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)
# #'
# # Model Logit(P(Y <= k))=mu_k-rowc_coef_r-col_coef_j-rowc_col_coef_rj with 2 row clustering groups:
# clustord(Y~ROWCLUST*COL,model="POM",nclus.row=2,long.df=long.df,
#          EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)
# #'
# # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*(colc_coef_c) with 3 column clustering groups:
# clustord(Y~COLCLUST,model="OSM",nclus.column=3,long.df=long.df,
#          EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)
# #'
# # Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*(colc_coef_c + row_coef_i) with 3 column clustering groups:
# clustord(Y~COLCLUST+ROW,model="OSM",nclus.column=3,long.df=long.df,
#          EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)