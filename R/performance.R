library(rstan)
library(brms)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# parameters for the metabolic rate model, natural log scale
lnMetabolicRate  <-  function (lnB0 = -5, scalingExp = 0.75, lnMass = log(1:10)) {
	lnB0 + scalingExp * lnMass
}

# parameters for the Boltzmann relationship, natural log scale
lnBoltzmann  <-  function (lnBoTs = -5, Er = 0.6, invKT = 1 / 8.62e-5 * (1 / mean(274:310) - 1 / 274:310)) {
	lnBoTs + Er * invKT
}

# complete Schoolfield-Sharpe equation
lnSchoolfield  <-  function (lnBoTs = -5, Er = 0.6, Ei = 3, Topt = 303, tempKelvin = 274:310) {
	boltz  <-  lnBoltzmann(lnBoTs, Er, 1 / 8.62e-5 * (1 / mean(tempKelvin) - 1 / tempKelvin))
	boltz - log(1 + (Er / (Ei - Er)) * exp(Ei / 8.62e-5 * (1 / Topt - 1 / tempKelvin)))
}

boltzmannModelBrms  <-  function (data) {
	# lnMass slope == scalingExp, invKT slope == Er

	# FIXED EFFECTS ONLY
	set.seed(1)
	brms::brm(lnRateBoltz ~ lnMass + invKT, data = data, family = gaussian(), prior = c(brms::prior(normal(1, 2), 'b'), brms::prior(normal(3, 3), 'Intercept'), brms::prior(student_t(3, 0, 20), 'sigma')), sample_prior = TRUE, chains = 3, cores = 3, iter = 1e3)
	
	# OR ADD RANDOM EFFECTS like in lme4, where "ID" is your grouping variable
	# brms::brm(lnRateBoltz ~ lnMass + invKT + (lnMass + invKT | ID), data = data, family = gaussian(), prior = c(brms::prior(normal(1, 2), 'b'), brms::prior(normal(3, 3), 'Intercept'), brms::prior(student_t(3, 0, 20), 'sd'), brms::prior(student_t(3, 0, 20), 'sigma')), sample_prior = TRUE, chains = 3, cores = 3, iter = 1e3)
}

schoolfieldModelBrms  <-  function (data) {
	# fit model using Er on logit scale to force it to be positive and smaller than Ei
	# Er = Ei / (1 + exp(-logitEr))
	schoolfieldPriors    <-  c(prior(normal(1, 1e6), nlpar = 'lnBoTs'),
	                           prior(normal(1, 1e6), nlpar = 'scalingExp'),
	                           prior(normal(2, 2), nlpar = 'Ei', lb = 1e-10, ub = 5),
	                           prior(normal(-1.2, 1), nlpar = 'logitEr'),
	                           prior(normal(298, 5), nlpar = 'Topt', lb = 250, ub = 320))

	schoolfieldInits    <-  list(list(scalingExp = 0.8, logitEr = -0.2, lnBoTs = -6, Ei = 1, Topt = 295),
	                             list(scalingExp = 0.7, logitEr = -1,   lnBoTs = -7, Ei = 2, Topt = 300),
	                             list(scalingExp = 0.6, logitEr = -0.7, lnBoTs = -8, Ei = 3, Topt = 305))

	# FIXED EFFECTS ONLY
	schoolFieldEquation  <-  brms::bf(lnRateSchool ~ lnBoTs + scalingExp * lnMass + (Ei / (1 + exp((-1) * logitEr))) * invKT - log(1 + (Ei / (1 + exp((-1) * logitEr))) / (Ei - (Ei / (1 + exp((-1) * logitEr)))) * exp(Ei / boltzmannK * (1 / Topt - 1 / tempKelvin))), lnBoTs ~ 1, scalingExp ~ 1, logitEr ~ 1, Topt ~ 1, Ei ~ 1, nl = TRUE)

	# OR ADD RANDOM EFFECTS using "parameter  ~ 1 + (1|G|ID)", where "ID" is your grouping variable
	# schoolFieldEquation  <-  brms::bf(lnRateSchool ~ lnBoTs + scalingExp * lnMass + (Ei / (1 + exp((-1) * logitEr))) * invKT - log(1 + (Ei / (1 + exp((-1) * logitEr))) / (Ei - (Ei / (1 + exp((-1) * logitEr)))) * exp(Ei / boltzmannK * (1 / Topt - 1 / tempKelvin))), lnBoTs ~ 1 + (1|G|ID), scalingExp ~ 1 + (1|G|ID), logitEr ~ 1, Topt ~ 1 + (1|G|ID), Ei ~ 1, nl = TRUE)

	set.seed(1)
	brms::brm(schoolFieldEquation, data = data, family = gaussian(), prior = schoolfieldPriors, sample_prior = TRUE, chains = 3, cores = 3, iter = 1e3, inits = schoolfieldInits)
}

calculateErWithCI  <-  function (brmsModel) {
	posteriors    <-  brms::posterior_samples(brmsModel, params = 'b_')
	posteriorErs  <-  posteriors[, 'b_Ei_Intercept'] / (1 + exp((-1) * posteriors[, 'b_logitEr_Intercept']))
	cis           <-  quantile(posteriorErs, probs = c(0.025, 0.975))
	list('allErs' = posteriorErs, 'mean' = mean(posteriorErs), 'median' = median(posteriorErs), 'lower95CI' = cis[1], 'upper95CI' = cis[2])
}

data               <-  data.frame(lnMass = rep(1:10, each = length(274:310)), tempKelvin = rep(274:310, 10), boltzmannK = 8.62e-5, stringsAsFactors = FALSE) # boltzmannK necessary for the non-linear parametrisation of brms
data$invKT         <-  1 / data$boltzmannK * (1 / mean(data$tempKelvin) - 1 / data$tempKelvin)
data$lnRateBoltz   <-  lnMetabolicRate(lnB0 = lnBoltzmann(lnBoTs = -5, Er = 0.6, invKT = data$invKT), scalingExp = 0.75, lnMass = data$lnMass)
data$lnRateSchool  <-  lnMetabolicRate(lnB0 = lnSchoolfield(lnBoTs = -5, Er = 0.6, Ei = 3, Topt = 303, tempKelvin = data$tempKelvin), scalingExp = 0.75, lnMass = data$lnMass)

modelBoltz   <-  boltzmannModelBrms(data)
modelSchool  <-  schoolfieldModelBrms(data)
calculateErWithCI(modelSchool)
