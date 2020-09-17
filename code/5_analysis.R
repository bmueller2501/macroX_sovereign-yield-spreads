# analysing our main model

# looks good
plotConv(mod.BMS)
cor(pmp.bma(mod.BMS))

# inclusion and signs of coef of the best 500 models
image(mod.BMS[1:500])

plotModelsize(mod.BMS) # plot the prior and posterior model size distribution (see result)

beta.draws.bma(mod.BMS[1:5]) # produces the posterior expected coefficient values for the best 5 models (see result)

