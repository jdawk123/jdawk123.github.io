library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(rsconnect)

rsconnect::setAccountInfo(name='jeremydawkins',
                          token='BE9A840737CC7621B3BFDA6BF104A9E1',
                          secret='gptJVSFnUWAXgbJCldTACBisht0KbdtorwLR09pv')

Beerdataframe = read.csv("Beers.csv",header = TRUE, sep = ",",stringsAsFactors = TRUE)
Brewerydataframe = read.csv("Breweries.csv",header = TRUE,sep = ",",stringsAsFactors = FALSE)
names(Beerdataframe) = c("Beer_Name","Beer_ID","Alcohol_by_Volume","Bitterness_Units","Brewery_ID","Beer_Style","Ounces")
names(Brewerydataframe) = c("Brewery_ID","Brewery_Name","City","State")
AllBeerDataFrame = merge.data.frame(Beerdataframe,Brewerydataframe, by = "Brewery_ID")


vars = AllBeerDataFrame %>% select(Alcohol_by_Volume,Bitterness_Units,State,Ounces)               

ui = fluidPage(
    setBackgroundColor(
        color = c("#F7FBFF", "#2171B5"),
        gradient = "linear",
        direction = "bottom"
    ),  #App Title 
    titlePanel("Beer Study!"),
    
    sidebarLayout( 
        sidebarPanel(
            selectInput("graph", label = "Choose a Graph", choices = c("Histogram","Box-Plot"), selected = "Histogram", multiple = FALSE),
            selectInput("xcol", "ABV or IBU", choices = c("Alcohol by Volume","Bitterness Units")),
            selectInput("state", label = "Filter by State", choices = vars$State),
            radioButtons("line", label = "Linear Regression Line", choices = c("Yes" = "yes","No" = "no"))
        ),
        mainPanel(plotOutput("plot1"), plotOutput("plot2"), plotOutput("plot3"), plotOutput("plot4")),
    ),
    
)

server = function(input,output){
    x = reactive({input$graph})
    t = reactive({input$xcol})
    y = reactive({if(x() == "Histogram" && t() == "Alcohol by Volume"){
        hist(vars$Alcohol_by_Volume, xlab = "Alcohol by Volume (ABV)", main = "Histogram of ABV", col = "#B2182B")
        
    }
        else if(x() == "Box-Plot" && t() == "Alcohol by Volume"){
            boxplot(vars$Alcohol_by_Volume, xlab = "Alcohol by Volume (ABV)", main = "Box-Plot of ABV", col = "#2166AC")}
    })
    output$plot1 = renderPlot({ y()})
    
    y1 = reactive({if(x() == "Histogram" && t() == "Bitterness Units"){
        hist(vars$Bitterness_Units, xlab = "Bitterness Units", main = "Histogram of IBU", col = "#D6604D")}
        else if(x() == "Box-Plot" && t() == "Bitterness Units"){
            boxplot(vars$Bitterness_Units, xlab = "Bitterness Units (IBU)", main = "Box-Plot of IBU", col = "#FDDBC7")}
    })
    output$plot2 = renderPlot({ y1()})
    
    output$plot3 = renderPlot({
        line2 = switch(input$line, yes = vars %>% ggplot(aes(x = Alcohol_by_Volume, y = Bitterness_Units),na.rm = TRUE) + geom_point(na.rm = TRUE, aes(colour = factor(vars$State))) + geom_smooth(method = "lm") + labs(title = "ABV vs IBU Scatterplot" , x = "Alcohol by Volume (ABV)", y = "Bitterness Units (IBU)"),
                       no = vars %>% ggplot(aes(x = Alcohol_by_Volume, y = Bitterness_Units),na.rm = TRUE) + geom_point(na.rm = TRUE, aes(colour = factor(vars$State))) + labs(title = "ABV vs IBU Scatterplot" , x = "Alcohol by Volume (ABV)", y = "Bitterness Units (IBU)"))
        line2
    })
    output$plot4 = renderPlot({vars %>% ggplot(aes(x = Alcohol_by_Volume, y = Ounces, color = State)) + geom_point(size = 5) + theme(legend.position = "none") + labs(title = "ABV vs Ounces", subtitle = "Ounces of 20 or More", x = "ABV", y = "Ounces") 
    })
}

shinyApp(ui = ui, server = server)
