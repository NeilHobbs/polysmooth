bioassay.values = seq(from = 0, to = 1, by = 0.1)

for(i in 1:length(bioassay.values)){
test_that("matches bioassay values",{

  expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.values[i],
                                                           regression.coefficient = 1,
                                                           regression.intercept = 0,
                                                           current.insecticide.efficacy = 1),
               bioassay.values[i])


})
}

expected.values = bioassay.values^0.5

for(i in 1:length(bioassay.values)){
  test_that("matches bioassay values",{

    expect_equal(convert_bioassay_survival_to_field_survival(bioassay.survival = bioassay.values[i],
                                                             regression.coefficient = 1,
                                                             regression.intercept = 0,
                                                             current.insecticide.efficacy = 0.5),
                 expected.values[i])


  })
}
