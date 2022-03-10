
check_for_errors_and_warnings = function(coverage,
                                         female.exposure,
                                         male.exposure,
                                         heritability,
                                         dispersal.rate,
                                         maximum.bioassay.survival.proportion,
                                         michaelis.menten.slope,
                                         applied.insecticide.dose,
                                         recommended.insecticide.dose,
                                         starting.refugia.resistance.score,
                                         starting.intervention.resistance.score,
                                         irm.deployment.strategy,
                                         intervention.coverage.1,
                                         intervention.coverage.2,
                                         intervention.coverage.1.2,
                                         probability.only.i.male,
                                         probability.only.j.male,
                                         probability.both.i.j.male,
                                         probability.only.i.female,
                                         probability.only.j.female,
                                         probability.both.i.j.female,
                                         vector.length,
                                         half.population.bioassay.survival.resistance){

##Error Messages:::
if(coverage > 1|coverage < 0){stop("coverage must be above 0 and up to 1")}
if(coverage == 0){stop("there is no coverage")}
if(female.exposure > 1| female.exposure < 0){stop("female.exposure must be between 0 and 1")}
if(male.exposure > 1| male.exposure < 0){stop("male.exposure must be between 0 and 1")}

if(heritability > 1| heritability < 0){stop("heritability must be between 0 and 1")}
if(dispersal.rate > 1| dispersal.rate < 0){stop("dispersal.rate must be between 0 and 1")}
if(maximum.bioassay.survival.proportion != 1){stop("maximum.bioassay.survival.proportion must equal 1")}
if(michaelis.menten.slope != 1){stop("michaelis.menten.slope must equal 1")}

if(applied.insecticide.dose < 0){stop("applied.insecticide.dose must be greater than or equal to 0")}
if(recommended.insecticide.dose < 0){stop("recommended.insecticide.dose must be greater than 0")}
if(starting.refugia.resistance.score < 0){stop("starting.refugia.resistance.score must be greater than or equal to 0")}
if(starting.intervention.resistance.score < 0){stop("starting.intervention.resistance.score must be greater than or equal to 0")}

if(irm.deployment.strategy == "combinations"){
  if(intervention.coverage.1 + intervention.coverage.2 + intervention.coverage.1.2 != 1){stop("toal intervention coverages must add to 1")}
  if(intervention.coverage.1 < 0 | intervention.coverage.1 > 1|
     intervention.coverage.2 < 0 | intervention.coverage.2 > 1|
     intervention.coverage.1.2 < 0 | intervention.coverage.1.2 > 1){stop("individual ntervention.coverages probabilities must be between 0 and 1")}
}

if(irm.deployment.strategy == "combinations"){
  if(probability.only.i.male + probability.only.j.male + probability.both.i.j.male != 1){stop("total male encounter probabilities must add to 1")}
  if(probability.only.i.female + probability.only.j.female + probability.both.i.j.female != 1){stop("total female encounter probabilities must add to 1")}
  if(probability.only.i.male < 0 | probability.only.i.male > 1|
     probability.only.j.male < 0 | probability.only.j.male > 1|
     probability.both.i.j.male < 0 | probability.both.i.j.male > 1|
     probability.only.i.female < 0 | probability.only.i.female > 1|
     probability.only.j.female < 0 | probability.only.j.female > 1|
     probability.both.i.j.female < 0 | probability.both.i.j.female > 1){stop("individual encounter probabilities mus be between 0 and 1")}
}

if(irm.deployment.strategy == "micromosaics"){
  if(intervention.coverage.1 + intervention.coverage.2 != 1){stop("toal intervention coverages must add to 1")}
  if(intervention.coverage.1 < 0 | intervention.coverage.1 > 1|
     intervention.coverage.2 < 0 | intervention.coverage.2 > 1){stop("individual ntervention.coverages probabilities must be between 0 and 1")}
}

##Warnings:::
if(vector.length < 1000){warning("vector.length may not be sufficiently long for numerical precision")}
if(half.population.bioassay.survival.resistance != 900){warning("Model calibration was originally performed with half.population.bioassay.survival.resistance = 900")}


}
