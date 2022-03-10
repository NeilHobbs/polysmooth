# polysmooth
Modelling insecticide resistance as a polygenetic trait: SMOOTH SELECTION

This version of modelling insecticide resistance as a polygenic trait has insecticide selection occurring through smooth selection. This is where survival to the insecticide survival of an individual is dependent on the Polygenic Resistance Score of the individual. E.g. if an individual mosquito has a polygenic resistance score that gives 10% bioassay survival, we would expect an individual with that polygenic resistance score to survive their encounter 10% of the time. A separate model assuming selection by truncation is the polytruncate model.


Polysmooth allows for the exploration of multiple insecticide resistance management strategies applicable to vector-borne diseases; with the primary focus on _Anopheles_ mosquitoes. However the methodology and results are likely to be applicable across a wider range of vectors and pests; although some re-calibration of the model is likely to be required. 

This model allows for the user input of a range of biological, chemical and operational parameters.

## Biological Parameters
female.fitness.cost
male.fitness.cost
female.insecticide.exposure
male.insecticide.exposure
heritability
dispersal.rate
standard.deviation
starting.refugia.resistance.score
starting.intervention.resistance.score
n.cycles 

## Chemical Parameters

## Operational Parameters
number.of.insecticides
intervention.coverage
irm.strategy 
withdrawal.threshold.value 
return.threshold.value 
deployment.frequency

## Other
exposure.scaling.factor 
vector.length                               
maximum.bioassay.survival.proportion
michaelis.menten.slope
regression.coefficient
regression.intercept
maximum.generations 
half.population.bioassay.survival.resistance 
maximum.resistance.value
                          
IRM Strategies:
Solo Deployments - Rotations, Sequences, Adaptive Rotations
Mixtures
Micro-Mosaics
Combinations
