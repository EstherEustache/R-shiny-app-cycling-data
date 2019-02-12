setwd("C:/Users/mehdi/Desktop/Cycliste/gitSave/R-shiny-app-cycling-data")
library(ggthemes)
library(shiny)
library(ggplot2)
library(dplyr)
source("Annexe1.R")

## Importation des donnees ##
montee <- rep('Montée')
plat <- rep('Plat')
ht <- rep('HT')

## CB
df1 =  read.csv("CB. H-T.csv")
df2 =  read.csv("CB. Plat.csv")
df3 =  read.csv("CB. Montée.csv")
df1 = data.frame(nom = rep('CB', length(df1[,1])), type = ht, df1)
df2 = data.frame(nom = rep('CB', length(df2[,1])), type = plat, df2)
df3 = data.frame(nom = rep('CB', length(df3[,1])), type = montee, df3)

## LL
df11 =  read.csv("LL. H-T.csv")
df12 =  read.csv("LL. Plat.csv")
df13 =  read.csv("LL. Montée.csv")
df11 = data.frame(nom = rep('LL', length(df11[,1])), type = ht, df11)
df12 = data.frame(nom = rep('LL', length(df12[,1])), type = plat, df12)
df13 = data.frame(nom = rep('LL', length(df13[,1])), type = montee, df13)

## PM
df21 =  read.csv("PM. H-T.csv")
df22 =  read.csv("PM. Plat.csv")
df23 =  read.csv("PM. Montée.csv")
df21 = data.frame(nom = rep('PM', length(df21[,1])), type = ht, df21)
df22 = data.frame(nom = rep('PM', length(df22[,1])), type = plat, df22)
df23 = data.frame(nom = rep('PM', length(df23[,1])), type = montee, df23)

## HT
df31 =  read.csv("SG. H-T.csv")
df32 =  read.csv("SG. Plat.csv")
df33 =  read.csv("SG. Montée.csv")
df31 = data.frame(nom = rep('SG', length(df31[,1])), type = ht, df31)
df32 = data.frame(nom = rep('SG', length(df32[,1])), type = plat, df32)
df33 = data.frame(nom = rep('SG', length(df33[,1])), type = montee, df33)


## ML
df41 =  read.csv("ML. H-T.csv")
df42 =  read.csv("ML. Plat.csv")
df43 =  read.csv("ML. Montée.csv")
df41 = data.frame(nom = rep('ML', length(df41[,1])), type = ht, df41)
df42 = data.frame(nom = rep('ML', length(df42[,1])), type = plat, df42)
df43 = data.frame(nom = rep('ML', length(df43[,1])), type = montee, df43)


df <- rbind(df1, df2, df3, df11, df12, df13, df21, df22, df23, df31, df32, df33,
            df41, df42, df43)


ui <- fluidPage(
  titlePanel("Analyse"),

  sidebarLayout(

    sidebarPanel(

      checkboxGroupInput(inputId = "y",
                         label = "Variable a analyser",
                         choices = c("Watts"  = "watts", "Heart Rate" = "hr",
                                     "Km/h" = "kph","Cadence" = "cad",
                                     "Km" =  "km","Altitude" = "alt"),
                         selected="watts"),

      selectInput(inputId = "coureur",
                  label = "Coureur",
                  choices = c("CB", "LL", "PM", "SG", "ML"), selected="CB"),

      selectInput(inputId = "entrainement",
                  label = "Type d'entrainement",
                  choices = c("HT", "Plat", "Montee")),

      selectInput(inputId = "type",
                  label = "Méthode de lissage",
                  choices = c("Moyennes mobiles"="ma", "Splines"="splines"), selected="ma"),

      sliderInput(inputId = "param",
                  label = "Parametre de lissage", min = 1, max = 100,
                  value = 3)

    ),

    # Outputs
    mainPanel(plotOutput(outputId = "scatterplot",
                         dblclick = "scatter_click",
                         brush = brushOpts(id = "scatter_brush", resetOnNew = TRUE)),
              br(),
              br(),
              br(),
              plotOutput(outputId = "barplot"))
  )
)

server <- function(input, output) {

  # Declaration du zoom interactif
  ranges <- reactiveValues(x = NULL, y = NULL)

  # Graphique principal
  output$scatterplot <- renderPlot({

    # Selection du dataframe a utiliser
    dfPlot <- df[which((df$nom == input$coureur)&(df$type == input$entrainement)),]
    dfPlot <- dfPlot[ , -which(names(dfPlot) %in% c('type','nom'))]

    # Lissage des courbes
    if (input$type == "ma"){
      dfPlot <- smoothing(dfPlot, input$param)
    } else if (input$type == "splines") {
      dfPlot <- smoothing_spline(dfPlot, df = input$param)
    }
    b1 <- mean(dfPlot$watts)*0.9
    b2 <- mean(dfPlot$watts)*1.1
    nb_watts = dfPlot$watts


    # Creation du graphique

    theme_set(theme_bw(base_size = 20))


    cols <- c("black", 'tan1',  "springgreen4", "royalblue", "rosybrown", "chartreuse3")
    if (length(input$y) == 0) {
      g <- ggplot(data.frame()) + geom_point() + xlim(0, 10) + ylim(0, 100)
      g <- g + ggtitle("Aucune variable n'est selectionnee")
    }
    if (length(input$y) == 1){
      g <- ggplot(data = dfPlot, aes_string(x = dfPlot$secs, y = input$y[1]))
      g <- g + geom_line(colour = cols[1]) + xlab("Secondes") + ylab(input$y[1])
      g <- g + ggtitle(paste("Coureur : ", input$coureur, "   | Seance : ", input$entrainement))
      g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y)

      if ("watts" %in% input$y) {
        b1 <- mean(dfPlot$watts)*0.9
        b2 <- mean(dfPlot$watts)*1.1
        g <- g + geom_hline(yintercept=b1)
        g <- g + geom_hline(yintercept=b2)
      }
    }
    if (length(input$y) > 1){
      g <- ggplot(data = dfPlot, aes_string(x = dfPlot$secs, y = input$y[1]))
      g <- g + geom_line() +  ggtitle(paste("Coureur : ", input$coureur, "   | Seance : ", input$entrainement))
      g <- g + xlab("Secondes") + ylab("")
      g <- g + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)

      if ("watts" %in% input$y) {
        b1 <- mean(dfPlot$watts)*0.9
        b2 <- mean(dfPlot$watts)*1.1
        g <- g + geom_hline(yintercept=b1)
        g <- g + geom_hline(yintercept=b2)
      }

      for (i in 1:length(input$y)){
        g = g + geom_line(data = dfPlot, aes_string(x = dfPlot$secs, y = input$y[i]),
                          colour = cols[i])

      }
    }
    g
  })


  # Plot interactif

  observeEvent(input$scatter_click, {
    brush <- input$scatter_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })

  # Second graphique EVA

  output$barplot <- renderPlot({
    dfPlot <- df[which((df$nom == input$coureur)&(df$type == input$entrainement)),]
    dfPlot <- dfPlot[ , -which(names(dfPlot) %in% c('type','nom'))]

    if (input$type == "ma"){
      dfPlot <- smoothing(dfPlot, input$param)
    } else if (input$type == "splines") {
      dfPlot <- smoothing_spline(dfPlot, df = input$param)
    }

    b1 <- mean(dfPlot$watts)*0.9
    b2 <- mean(dfPlot$watts)*1.1
    nb_watts = dfPlot$watts

    # Calcul de l'EVA
    percentages = eva(nb_watts, b1, b2)

    rt = data.frame(c(percentages,0),
                    c("<90%","<90%", ">110%", ">110%", "90-110%", "90-110%"),
                    c("Court (<10s)", "Long (>10s)", "Court (<10s)", "Long (>10s)", "Zone optimale", "Zone optimale"))

    colnames(rt) = c("Pourcentage", "Zone", "Duree")
    theme_set(theme_bw(base_size = 20))

    p <- ggplot(data=rt,
                aes(x=Zone, y=Pourcentage, fill=Duree, color=Duree)) + geom_bar(stat="identity")
    labes = c()
    for (i in 1:6){
      if (rt$Pourcentage[i] <= 1) {
        labes[i] = ""
      } else {
        labes[i] = paste(round(rt$Pourcentage[i]), " %")
      }
    }

    p <- p +  geom_text(aes(label=labes), vjust=0, color="black", size=5, position = position_stack(vjust = 0.5))
    p <- p + scale_fill_manual("Duree", values=c("Court (<10s)"="dodgerblue1","Zone optimale" = "green2","Long (>10s)" = "red2"))
    p <- p + scale_color_manual("Duree", values=c("Court (<10s)"="dodgerblue1","Zone optimale" = "green2","Long (>10s)" = "red2"))
    p <- p + ggtitle("Analyse detaillee EVA")
    p
  })


}


shinyApp(ui = ui, server = server)
