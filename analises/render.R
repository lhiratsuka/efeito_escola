params <- new.env()
params$serie <-  '5EF'
params$book <-  "../books/saresp5ef_pais_mode.csv"

rmarkdown::render(
  'teste.Rmd',
  output_file = "Saresp Pais 5EF.pdf",
  envir = params)

