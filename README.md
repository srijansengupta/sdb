# sdb
Please cite the following paper if you use the code.

Sengupta, S., Volgushev, S., and Shao, X. (2016).  A subsampled double bootstrap for massive data.                                        Journal of the American Statistical Association (Theory and Methods), 111, 1222-1232    

Sengupta, S. and Chen, Y. (2018). A blockmodel for node popularity in networks with community structure. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 80 (2), 365–386

IID simulationss: SDBmultreg.R is the R code for multiple regression simulations (Section 4.1), and SDB_logreg.R for logistic regression (Section 4.2). SDBplot.R is the R code for producing error plots from the output of the previous codes.

Time series simulations: SDBts_Mfun.R, SDBts_RegfunQ.R, tsregsimsQ.R, and Qsims.R have codes for performing the simulations, and SDBts_Manalysis.R has code for generating error plots.

Data analysis: The Central England Temperature dataset is in CentralEnglandTemperatures.txt and SDB_tsdata.R has R code for data analysis while SDB_Dataplot.R has R code for generating error plots.
