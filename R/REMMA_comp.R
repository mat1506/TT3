##phi <- convert.units(mu = mu)
phi <- as.numeric(colnames(X))
## get l
l <- get.l(X = X, max = 0.95, n = 20)
## get q
q <- get.q(X = X, l = l)
l <- as.numeric(rownames(q))
## model EMs
em.pot <- model.EM(X = X, q = q, l = l, plot = F)
## set limits
limits <- get.limits(loadings = em.pot)
## get robust EMs
library(RColorBrewer)
em.rob <- robust.EM(em = em.pot,classunits = phi,limits = limits,
                    plot = F,
                    legend = "topright",
                    cex = 1.2,
                    cex.axis = 2,
                    colour = brewer.pal(length(limits)/2, 'Set1'),
                    median = TRUE,
                    mc_n = 1000)

#q <- nrow(em.rob$Vqn$mean)
# Plot line-point graph with means and standard deviations
#par(mfrow = c(1,5), oma = c(0, 0, 2, 0))
#cols <- brewer.pal(q, "Set1")

#for(i in 1:q) {
#  sd.max <- ceiling(max(em.rob$scores$sd[,i]))
#  sd.min <- ceiling(min(em.rob$scores$sd[,i]))
#  plot(em.rob$scores$mean[,i],
#       1:nrow(X),
#       col = cols[i],
#       lwd = 2,
#       type = 'l',
#       ylab = 'sampel no.',
#       xlab = 'scores',
#       ylim = rev(c(1, nrow(X))),
#       xlim = c(-0.2, 1.2),
#       main = paste("EM ", i))
#  lines(em.rob$scores$mean[,i] - em.rob$scores$sd[,i], 1:nrow(X), col = cols[i], lty = 2)
#  lines(em.rob$scores$mean[,i], 1:nrow(X), col = cols[i], lwd = 2)
#  points(em.rob$scores$mean[,i], 1:nrow(X), col = cols[i])
#  lines(em.rob$scores$mean[,i] + em.rob$scores$sd[,i], 1:nrow(X), col = cols[i], lty = 2)
#}
#mtext('Mean end-member scores with uncertainty', outer = TRUE, cex = 1.3)
