
cat("Welcome to licorstat.R\n")
cat("Loading data...\n")
dat <- read_csv("../inventory_data/collar_to_tree_prox.csv")
BAdat <- read_csv("../inventory_data/tree_frequency.csv")

# Only looking at the undisturbed controls for this analysis
cat("Filtering data...\n")
dat <- filter(dat, Group == "Control")
#only want true controls

# We are interested in understanding the drivers of soil respiration
# (`Flux`, µmol/m2/s). Variables that we have a priori reason to 
# think might be influential include soil temperature (measured at two depths,
# 5 and 20 cm), soil moisture (measured at 0-10 cm), and salinity (measured
# by the continuous dataloggers). Here we examine all except the last.

# We suspect (and preliminary data exploration showed) that Flux will
# be nonlinear with soil temperature, and its variability will
# increase with temperature. So we transform  the dependent variable
# via log(). In addition, soil moisture may limit soil respiration
# both when it's very low (when the microbes don't have enough water to
# function) and very high (when they don't have enough oxygen). To
# handle this, we fit a quadratic moisture function.

# Finally, the salinity level (GCREW versus Canoe Shed versus North Branch)
# might exert an effect (due to differing soil types, for example).
# But this isn't a treatment, so we'll model is as a *random* effect.
# To do so we use a linear mixed-effects model.
cat("Fitting full model...\n")
library(nlme)

mod_full <- nlme::lme(log(Flux) ~ T5 + T20 + SMoisture + I(SMoisture ^ 2) + BA5, 
                      random = ~ 1 | Dest_Salinity,
                      method = "ML", # because we're doing stepAIC below
                      data = dat)
print(summary(mod_full))

# Remove nonsignificant terms in an automated (i.e. reproducible) way
# This algorithm does stepwise model selection, trying both forward
# (adding terms) and backward (removing them) steps.
cat("Removing nonsignificant terms...\n")
library(MASS)
mod <- MASS::stepAIC(mod_full, direction = "both")
print(summary(mod))

# Plot and save diagnostics for this model
pdf("../outputs/licorstat_mod.pdf")
plot(mod)
dev.off()

# SP TODO: compute Q10 values for this model for both T5 and T20, over the
# observed temperature ranges
# This is actually a little tricky as both end up as significant


cat("All done.\n")
