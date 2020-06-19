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
            div(class = "thumbnail",
                img(src = image, alt = "...",
                    div(class = "caption", h3(label), p(content)
                    )))))
}