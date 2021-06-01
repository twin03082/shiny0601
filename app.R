##Load packages
library(shiny)
library(readr)

#Load data 
medical<-read_csv("data/medical07.csv")
medical08<-medical07%>%filter(연도==2013)
medical09<-medical07%>%filter(연도 == 2019)
vars1 <- setdiff(names(medical08),c("국가별","연도"))
vars2<-setdiff(names(medical09),c("국가별","연도"))

#User interface
ui<-fluidPage(
  titlePanel("OECD 보건지출비 K-means clustering"),
  
  fluidRow(
    
    column(6,
           sidebarLayout(
             sidebarPanel(
               h4("2013년 OECD 보건지출비"),
               helpText("OECD 보건지출비"),
               selectInput("xcol1","X variable",vars1),
               selectInput("ycol1","Y variable",vars1,selected=vars1[2]),
               numericInput('clusters1', 'Cluster count', 2, min =1, max=9),
               img(src="oecd1.png",height=100,width=200)
             ),
             mainPanel(plotOutput('plot1'))
           )
    ),
    
    column(6,
           sidebarLayout(
             sidebarPanel(
               h4("2019년 OECD 보건지출비"),
               helpText("OECD 보건지출비"),
               selectInput("xcol2","X variable",vars2),
               selectInput("ycol2","Y variable",vars2,selected=vars2[2]),
               numericInput('clusters2', 'Cluster count', 2, min =1, max=9),
               img(src="oecd1.png",height=100,width=200)
             ),
             mainPanel(plotOutput('plot2'))
           )
    )
  )
)
#Server logic
server <- function(input,output){
  
  
  selectedData1<-reactive({
    medical08[,c(input$xcol1,input$ycol1)]
  })
  
  selectedData2<-reactive({
    medical09[,c(input$xcol2,input$ycol2)]
  })
  
  clusters1<- reactive({
    kmeans(selectedData1(),input$clusters1)
  })
  
  clusters2<- reactive({
    kmeans(selectedData2(),input$clusters2)
  })
  
  output$plot1<-renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar=c(5.1, 4.1, 0, 1))
    plot(selectedData1(),
         col = clusters1()$cluster,
         pch=20, cex =3 )
    points(clusters1()$centers, pch=4, cex=4, lwd=4)
  })
  
  output$plot2<-renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    par(mar=c(5.1, 4.1, 0, 1))
    plot(selectedData2(),
         col = clusters2()$cluster,
         pch=20, cex =3 )
    points(clusters2()$centers, pch=4, cex=4, lwd=4)
  })

}
# Run app
shinyApp(ui,server)