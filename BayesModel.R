# 1. League Yardage / Completed Passes
x <- sum(qbdata$TotalYards[qbdata$PassOutcome=="Complete"])/sum(qbdata$PassOutcome=="Completed")
# 		y <- log(x + 1)
y <- log(x+1)
# Prior data? (? significance of the vinculum over y)
Mo <- ???
# Weight of prior
Ko <- 1
# Degree of freedom (Why 3?)
Vo <- 3
# Scale Parameter
S2o <- var(y)
#
# # Need values of nj & Yj
# 2.	Vn <- Vo + nj
# 		S2n <- Vo*S2o + (nj-1)S2j + [(Ko*nj)/(Ko+nj)](Yj - Mo)^2
# 		Mn <- (Ko/Ko+nj)*Mo + (nj/Ko+nj)*(Yj)
# 		Kn <- Ko + nj
#
# # Confused on the equations written for this section
# 3.	Draw S2i
S2i <- Yj * rinvchisq(Vn, S2n, scale=1/df)
#
# 4.	Draw Mi
# 		Mi <- S2i * Yj * N(Mn, S2i/Kn)
#
# 5.	Draw Yi
# 		???
#
# 6.	yards <- exp(Yi - 1)
y <- exp(Yi - 1)