#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer = function(input, output) {

    output$distPlot <- renderPlot({

        compare_micro_mosaics(number.of.insecticides = 2,
                              maximum.generations = 100,
                              starting.intervention.resistance.score =0,
                              applied.insecticide.dose = 1,
                              recommended.insecticide.dose = 1,
                              threshold.generations = 15,
                              base.efficacy.decay.rate = 0,
                              rapid.decay.rate =0,
                              deployment.interval = 20,
                              max.cycles = 5,
                              standard.deviation = 50,
                              vector.length = 1000,
                              female.insecticide.exposure = 0.4,
                              male.insecticide.exposure = 0.5,
                              heritability = 0.3,
                              regression.coefficient = 0.48,
                              regression.intercept = 0.15,
                              exposure.scaling.factor = 10,
                              male.fitness.cost = 0,
                              female.fitness.cost=0,
                              half.population.bioassay.survival.resistance = 900,
                              michaelis.menten.slope = 1,
                              maximum.bioassay.survival.proportion = 1,
                              cross.selection = 0,
                              female.natural.survival.probability = 1,
                              male.natural.survival.probability = 1)

})
    }
