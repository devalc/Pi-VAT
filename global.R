# Add a custom thumbnail with a description
# 

# @param image a object of class character
# @param label a object of class character
# @param content a object of class character
# 
# @return a HTML object to be included in the ui section of a shiny app
#
#

# thumbnail_label1 <- function(image, label, content){
#     div(class = "row", style="height:360px",
#         div(class = "col-sm-14 col-md-12",
#             div(class = "thumbnail",style="background-color:#17141D;height:60px",
#                 img(src = image, alt = "...", width ="100%",
#                     div(class = "caption",style="color:#ffffff;max-width: 100%", h3(label), p(content)
#                     )))))
# }


thumbnail_label1 <- function(image, label, content){
    div(class = "row", style="height:370px",
        div(class = "col-sm-14 col-md-12",
            div(class = "thumbnail",style="height:50px",
                img(src = image, alt = "...", width ="100%",
                    div(class = "caption",style="max-width: 100%;text-align:centre",
                        h4(label)), div(class = "description",style="max-width:90%;text-justify:inter-word", p(content)
                    )))))
}

### customize the html bakground image function to blur the image

# setBackgroundImage1 <- function(src = NULL, shinydashboard = FALSE) {
#     if (isTRUE(shinydashboard)) {
#         el <- ".content-wrapper"
#     } else {
#         el <- "body"
#     }
#     css <- paste0(
#         el, " {background: url(", src, ") no-repeat center center fixed;
#            -webkit-background-size: cover;
#            -moz-background-size: cover;
#            -o-background-size: cover;
#            background-size: cover;
#         backdrop-filter: blur(50px);}"
#     )
#     tags$head(tags$style(HTML(css)))
# }

disconnected <- tagList(
    h1("Whoah there..."),
    p("Session timed out or something went terribly wrong! Hit Refresh..."),
    reload_button("REFRESH", class = "warning")
)