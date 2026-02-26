module_opt06_donar_UI <- function(id) {
  ns <- NS(id)
  
  
  div(
    tagList(
      tags$head(
        tags$script(type="text/javascript", src = "busy.js"),
        tags$link(rel="shortcut icon", href="./rmediclogo.jpg"),
        tags$script(type="text/javascript", "var switchTo5x=true"),
        tags$script(type="text/javascript",'stLight.options({publisher: "675b3562-a081-470a-9fc4-3dd6a712209d", doNotHash: true, doNotCopy: true, hashAddressBar: false})')
      )
    ),
    div(id = ns("home"),
        br(),
        fluidRow(
          column(3, img(src = "rmediclogo.jpg", width = 300, height = 300)),
          column(9, div(style = "text-align: center;",
                        actionButton("donateButton", "Donar con PayPal", icon = icon("paypal")),
                        HTML('
      <form action="https://www.paypal.com/donate" method="post" target="_top">
<input type="hidden" name="hosted_button_id" value="TUVRWUQYUL7B8" />
<input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif" border="0" name="submit" title="PayPal - The safer, easier way to pay online!" alt="Donate with PayPal button" />
<img alt="" border="0" src="https://www.paypal.com/en_AR/i/scr/pixel.gif" width="1" height="1" />
</form>

    ')
          )
          )
        )
        
        
    )
  )
  
}



module_opt06_donar_SERVER <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$"super01" <- renderUI({
      
    })
    
  })
}

