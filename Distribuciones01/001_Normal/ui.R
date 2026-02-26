
source("../../lib.R", encoding = "UTF-8")
source("lib.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")

pageWithSidebar(
  headerPanel("Distribución Normal"),
  sidebarPanel(
    SideBar01_Normal("aver2")
  ),
  mainPanel(
    MainPanel01_Normal("aver2")
  )
)