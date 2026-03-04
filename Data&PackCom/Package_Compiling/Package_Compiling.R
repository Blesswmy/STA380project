usethis::create_package("/Users/leosmac/Documents/STA380project")

usethis::use_testthat(3)

usethis::use_vignette("my-vignette")

devtools::build_vignettes()

devtools::document()

devtools::check()

devtools::test()




# Usage of Generative AI for fixing notes error.
# -   https://chatgpt.com/share/69a757ea-4820-8003-b69a-0eda26794c72

#    Fixing notes error "
# ❯ checking R code for possible problems ... NOTE
#  bootstrap_ci: no visible global function definition for ‘quantile’
# bootstrap_median: no visible global function definition for ‘median’
# Undefined global functions or variables:
#    median quantile
#  Consider adding
#    importFrom("stats", "median", "quantile")
#  to your NAMESPACE file.
