
source("lib.R", encoding = "UTF-8")
source("../../lib.R", encoding = "UTF-8")
source("modules.R", encoding = "UTF-8")

pageWithSidebar(
  headerPanel("Distribución t"),
  sidebarPanel(
    SideBar02_t("aver2")
  ),
  mainPanel(
    MainPanel02_t("aver2")
  )
)