# Add a custom thumbnail with a description
# 

# @param image a object of class character
# @param label a object of class character
# @param content a object of class character
# 
# @return a HTML object to be included in the ui section of a shiny app
#
#


thumbnail_label1 <- function(image, label, content){
    div(class = "row", style="height:400px",
        div(class = "col-sm-14 col-md-12",
            div(class = "thumbnail",style="height:430px;", #background-color:#6d6875;
                img(src = image, alt = "...", width ="100%",
                    div(class = "caption",style="max-width: 100%;text-align:centre",
                        h4(label)), div(class = "description",style="max-width:90%;text-justify:inter-word", p(content)
                    )))))
}


thumbnail_label2 <- function(image, label, content, helpinfo){
    div(class = "row", style="height:400px",
        div(class = "col-sm-14 col-md-12",
            div(class = "thumbnail",style="height:430px;", #background-color:#6d6875;
                img(src = image, alt = "...", width ="100%",
                    div(class = "caption",style="max-width: 100%;text-align:centre",
                        h4(label)%>%
                            helper(
                                icon = "info-circle",
                                colour = "#fb8500",
                                content = helpinfo,
                                type = "markdown",
                                size = "l",
                                buttonLabel = "Okay",
                                easyClose = TRUE,
                                fade = TRUE
                            )), div(class = "description",style="max-width:90%;text-justify:inter-word", p(content)
                        )))))
}


disconnected <- tagList(
    h1("Whoah there..."),
    p("Session timed out or something went terribly wrong! Hit Refresh..."),
    reload_button("REFRESH", class = "danger")
)