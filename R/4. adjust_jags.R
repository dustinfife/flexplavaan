full.file = "lavExport/sem.jag"
fl = readChar(full.file, file.info(full.file)$size)

  ## insert new text
old.exp = "mu[i,3] <- nu[3,1,g[i]] + lambda[3,1,g[i]]*eta[i,1]"
new.exp = "mu[i,3] <- nu[3,1,g[i]]^eta[i,1]"

old.prior = "x3a[i] ~ dnorm(mu[i,3], 1/theta[3,3,g[i]])"
new.prior = "x3a[i] ~ dchisq(mu[i,3])"

fl_new = fl
fl_new = gsub(old.exp, new.exp, fl_new, fixed=T)
fl_new = gsub(old.prior, new.prior, fl_new, fixed=T)

fileConn<-file("lavExport/sem_nonlinear.jag")
writeLines(fl_new, fileConn)
close(fileConn)

