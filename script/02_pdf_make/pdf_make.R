library(here)

# import data
md <- readr::read_csv(here("img_url_list/lovot_img_20201103.csv"))


# make pdf file for all tweets
for (i in 1:nrow(md)) {
  if (file.exists(paste(here("images/lovot/"),md$img_id[i],".jpg", sep=''))) {
    rmarkdown::render(input = here("script/pdf_make/do_pdf.Rmd"),
                      output_format = "pdf_document",
                      output_file = paste('lovot_', i, '.pdf', sep=''),
                      output_dir = here('pdf/'))  #save file in "pdf" folder
  }
}



# make pdf file for selected tweets

sample_md <- sort(sample(nrow(md), size=20, replace = F))  # randomly sampling N tweets (N=20 here)

# make pdf file
for (i in sample_md) {
  if (file.exists(paste(here("images/lovot/"),md$img_id[i],".jpg", sep=''))) {
    rmarkdown::render(input = here("script/pdf_make/do_pdf.Rmd"),
                      output_format = "pdf_document",
                      output_file = paste('lovot_', i, '.pdf', sep=''),
                      output_dir = here('pdf/'), 
                      clean = TRUE)
  }
}
