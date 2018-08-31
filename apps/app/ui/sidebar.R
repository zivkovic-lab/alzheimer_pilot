sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Glycosylation", 
            menuSubItem("Boxplot", tabName = "glc_boxplot"),
            menuSubItem("Histograms", tabName = "glc_hist"),
            menuSubItem("vs HDL Function", tabName = "glc_fct")
        ),
        menuItem(
            icon = icon("caret-right"), "HDL Function",
            menuSubItem("Boxplot", tabName = "fct_boxplot")
        )
    )
)