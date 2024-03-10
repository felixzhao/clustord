set.seed(1)
long.df <- data.frame(Y=factor(sample(1:3,5*20,replace=TRUE)),
                ROW=factor(rep(1:20,times=5)),COL=rep(1:5,each=20))

# Model Log(P(Y=k)/P(Y=1))=mu_k+phi_k*rowc_coef_r with 3 row clustering groups:
clustord(Y~ROWCLUST,model="OSM",3,long.df=long.df, EM.control=list(EMcycles=2,startEMcycles=2), nstarts=2)