# popup function (from J. Cheng, Shiny-discuss, 2/7/13)
# C Boivin:  add a parameter "glue" to label the button
# or to place an object on (button, selectInput, slider)
helpPopup <- function(title, content,
              placement=c('right', 'top', 'left', 'bottom'),
              trigger=c('click', 'hover', 'focus', 'manual'),
              glue = NULL, 
              info=NULL) {
  
  tagList(
    singleton(
      tags$head(
           tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")
      )
    ),
    tags$a(
      href = "#",
      class = "tip",
    ## 
    # the Information mark is the receiver of the content
    # the "glue" parameter allows the introduction of a variable text string or another object
   tags$i(class = "glyphicon glyphicon-info-sign"), glue,
   ###
      `data-toggle` = "popover",
      title = title, 
     `data-content` = content,
      # added this parameter
      `data-html` = TRUE, 
      # test
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok=TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok=TRUE)[1],  
 ##     glue,
  #     mask the icon if using glue with hover
   #  The icon style is passed with the parameter "info" in the app
    info 
    ),
    # CB added for popup width control
    tags$style(type='text/css', ".popover { width: 400px; relative; left: 320px !important; }")
    # end add
  )
}