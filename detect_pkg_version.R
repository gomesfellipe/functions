# Detectar a versão a e origem de um ou mais pacotes

detect_pkg_version <- function(package){
  
  if(length(package) == 1){
    desc <- packageDescription(package)  
    
    if(is.null(desc$RemoteType)){
      glue::glue("{desc$Package} {desc$Version}")  
    }else if(desc$RemoteType == "github"){
      glue::glue("{desc$Package} {desc$Version} ({desc$GithubUsername}/{desc$GithubRepo})")
    }
    
  }else if(length(package) > 1){
    desc <- lapply(package, packageDescription)
    
    desc <- 
      unlist(
        lapply(desc, function(desc){
          if(is.null(desc$RemoteType)){
            glue::glue("{desc$Package} {desc$Version}")  
          }else if(desc$RemoteType == "github"){
            glue::glue("{desc$Package} {desc$Version} ({desc$GithubUsername}/{desc$GithubRepo})")
          }  
        })  
      )
    tibble::tibble(package = desc)
  }
}
