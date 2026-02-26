function(input, output, session) {
  
  # builds a reactive expression that only invalidates 
  # when the value of input$goButton becomes out of date 
  # (i.e., when the button is pressed)

  
  
  callModule(module = Server01_Normal_Server, 
             id =  "aver2",
             la_distribucion = "001_Normal")
  

  

}