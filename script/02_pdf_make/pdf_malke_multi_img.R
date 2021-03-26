library(here)
library(dplyr)

old_list <- rtweet::read_twitter_csv(here("img_url_list/aibo_img_20201103.csv"))

keep <- readxl::read_xlsx("temp/List_for_pdfs_.xlsx", col_names = F, sheet = 2) %>% as.data.frame()

colnames(keep) <- "img_id"

keep$img_id <- stringr::str_remove_all(keep$img_id, pattern = ".jpg")

head(keep$img_id)
head(old_list$img_id)

## remove unnecessary images
remove_list <- anti_join(old_list, keep, by = "img_id")
remove_list$img_path <- paste0(here("temp/aibo/"), remove_list$img_id, ".jpg")

for (i in 1:nrow(remove_list)) {
  unlink(remove_list$img_path[i])
}


## get images file path
new_list <- dplyr::inner_join(old_list, keep, by = "img_id")

head(new_list$img_id)

# group images of the same tweet together
img_list <- new_list %>% 
  group_by(status_id) %>% 
  summarize(img_list = list(img_id))

head(img_list)

new_list <- inner_join(new_list, img_list, by = "status_id") %>% 
  distinct(status_id, .keep_all = TRUE)

str(new_list)

new_list$img_path <- new_list$img_list %>% 
  purrr::map(function(x) paste0("temp/aibo/",x,".jpg"))

head(new_list$img_path, 10)

library(imager)
pp <- kk$img_path %>% 
  purrr::map(function(x) load.image(x))

purrr::map(kk$img_path[[63]], load.image) %>% imappend("x") %>% plot(axes=F)

for (i in 1:nrow(new_list)) {
    rmarkdown::render(input = here("temp/do_pdf_multi_img.Rmd"),
                      output_format = "pdf_document",
                      output_file = paste('aibo_', i, '.pdf', sep=''),
                      output_dir = here('output/pdf/'))  #save file in "pdf" folder
}
