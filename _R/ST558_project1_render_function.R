###########################################################
# ST558 - Vignette Project 1
# Jasmine Wang
# Due 10/05/2021
# 
# Render function to output .Rmd file for github
##########################################################3


rmarkdown::render("C:/Users/peach/Documents/ST558/ST558_repos/vignette_project/_Rmd/ST558_vignette_proj.Rmd", 
                  output_format = "github_document", 
                  output_file = "C:/Users/peach/documents/ST558/ST558_repos/vignette_project/README.md", 
                  output_options = list(html_preview = FALSE, toc = TRUE, toc_depth = 3, df_print = "tibble")
)


