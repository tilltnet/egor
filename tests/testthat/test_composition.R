context("test_composition.R")

test_that(
  "Composition commands work.",
  {
    e <- make_egor(3, 3)
    
    expect_error(composition(e, "age", T), NA, label = "composition absolute")
    expect_error(composition(e, "age"), NA, label = "composition proportional")

    expect_error(comp_ei(e, "sex", "sex"), NA, label = "comp_ei")
    
    expect_error(comp_ply(e, "age", .f = egor:::fun_alts_diversity), 
                 NA, label = "comp_ply fun_alts_diversity")
    expect_error(comp_ply(e, "age", .f = egor:::fun_entropy, base = 7),
                 NA, label = "comp_ply fun_entropy")
    expect_error(comp_ply(e, "age", .f = egor:::fun_comp_ei, ego.attr = "age"),
                 NA, label = "comp_ply fun_comp_ei")
    # Example
    df <- make_egor(10, 32)
    expect_error(comp_ply(df, "age.years", sd, na.rm = TRUE), 
                 NA, "comp_ply Example")
    expect_error(alts_diversity_entropy(e, "age"), NA, label = "diversity_entropy")
    expect_error(alts_diversity_count(e, "age"), NA, label = "diversiy_count")
  }
)

