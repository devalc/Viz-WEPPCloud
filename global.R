# Add a custom thumbnail with a description
# 
# Need to figure out how to add action button to this to navigate through tabs
# @param image a object of class character
# @param label a object of class character
# @param content a object of class character
# 
# @return a HTML object to be included in the ui section of a shiny app
#
#

thumbnail_label1 <- function(image, label, content){
    div(class = "row",
        div(class = "col-sm-14 col-md-12",
            div(class = "thumbnail",style="height:360px;background-color:#f2f2f2;",
                img(src = image, alt = "...",
                    div(class = "caption",style="color:#000000;", h3(label), p(content)
                    )))))
}


### customize the html bakground image function to blur the image

setBackgroundImage1 <- function(src = NULL, shinydashboard = FALSE) {
    if (isTRUE(shinydashboard)) {
        el <- ".content-wrapper"
    } else {
        el <- "body"
    }
    css <- paste0(
        el, " {background: url(", src, ") no-repeat center center fixed;
           -webkit-background-size: cover;
           -moz-background-size: cover;
           -o-background-size: cover;
           background-size: cover;
        backdrop-filter: blur(50px);}"
    )
    tags$head(tags$style(HTML(css)))
}