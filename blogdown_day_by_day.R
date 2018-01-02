#COnfigurando o git
git config --global user.name "gomesfellipe"

git config --global user.email "gomes.fellipe@hotmail.com"

#Install package:
devtools::install_github('rstudio/blogdown')


#Configurando blogdown:
library(blogdown)
install_hugo()
setwd("~/Github/site-hugo")

# automatically install Hugo, create a site, download a theme,
# add sample posts, and start a local web server
blogdown::new_site()

#Another themes:
new_site(theme = 'jpescador/hugo-future-imperfect')
# new_site(theme = 'laozhu/hugo-nuo')
# new_site(theme = 'digitalcraftsman/hugo-icarus-theme')
# # Create new site in our recently cloned blogdown repository 
# new_site(dir = 'C:/Users/Fellipe/Documents/Git/blogdown_source', theme = 'kakawait/hugo-tranquilpeak-theme', format = 'toml') 

# install_theme("digitalcraftsman/hugo-icarus-theme")


#Create a new post
blogdown:::new_post_addin()

# Render the site:
blogdown::serve_site()
