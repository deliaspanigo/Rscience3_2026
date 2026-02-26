
params <- list(n=100, min=0, max=1)
params <- list(min=0, n=100, max=1)

do.call(runif,params) # doesn't work

# params <- list(min=0, max=1)
# runif(n=100,min=0,max=1) # works
# do.call(runif,list(n=100,min=0,max=1)) # works
# do.call(runif,list(n=100,params)) # doesn't work

function(input, output, session) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)

  
  
  callModule(module = Server02_t_Server, 
             id =  "aver2",
             la_distribucion = "001_Normal")
  

  

}