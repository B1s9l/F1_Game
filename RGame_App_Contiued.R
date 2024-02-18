##********************************************************************************
##**********RGame**********
##********************************************************************************

##********************************************************************************
##Include libraries
##********************************************************************************

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(ggthemes)
library(maps)





##********************************************************************************
##User Interface
##********************************************************************************

ui <- fluidPage(
  tags$head(HTML("<title>F1 Game</title>")),
 
##********************************************************************************
##Background style (comment to disable/ uncomment to enable)
    ##Solid Black
  setBackgroundColor(color ="black"),
    ##Solid White (cn't read white titles)
  #setBackgroundColor(color ="ghostwhite"),
    
  #setBackgroundImage(src = "https://c4.wallpaperflare.com/wallpaper/410/494/431/racing-f1-car-formula-1-race-car-hd-wallpaper-preview.jpg"),
    ##background option (hard to read)
  #setBackgroundImage(src = "https://www.wallpaperup.com/uploads/wallpapers/2013/06/03/97350/026fda7aee719b82af0a30442ba115d3-1400.jpg"),
    ##background option (hard to read)
  #setBackgroundImage(src = "https://www.wallpaperup.com/uploads/wallpapers/2013/07/18/120380/b729df39920dc2e9ed55f9f4cb9fc4c8-1400.jpg"),
    
##********************************************************************************
##Title
  titlePanel(
    div(strong(textOutput("headTitle")), style = "color:red; font-size: 50px")
             ),
  
##********************************************************************************
##Setup Sidebarpanel Layout
  sidebarLayout(
    
##********************************************************************************
##Setup sidebarPanel 
    sidebarPanel(
      
##********************************************************************************
##Info output & startBut
      
      div(h3(textOutput("info")), style = "color: black"),
      tags$head(tags$style(HTML("a {color: blue}"))),
      div(h4("A game by ", a(href = "https://reddit.com/r/goats/", "Nina"), "and", a( href = "https://instagram.com/basilfurrer", "Basil")),style = "color:blue"),
      uiOutput("startBut"),
      uiOutput("cheatSwitch"),
      uiOutput("cheat"),
      
##********************************************************************************
##Balance Outputs

      strong(h3(textOutput("balanceTitle"))),
      div(tableOutput("balanceTable"),style = "color:red"),

##********************************************************************************
##How to play

      strong(h3(textOutput("howtoTitle"))),
      textOutput("howtoInfo"),

##********************************************************************************
##BWiki Formula One
      strong(h3(textOutput("wikiTitle"))),
      textOutput("wikipedia_f1"),

      tags$a(href = "https://en.wikipedia.org/wiki/Formula_One",
       "Source: Wikipedia - Formula One")
    ),
  
##********************************************************************************
##Setup mainPanel    
    mainPanel(
      
##********************************************************************************
##1 First Row
      fluidRow(
        
##********************************************************************************
##1.1 First Column (Inputs; Bets & Guess)
        column(
          div(h3(textOutput("inputTitle")),style = "color: ghostwhite"),
          uiOutput("inputGuessBet"),width=4),
        
##********************************************************************************
##1.2  Second Column (Result Output)
        column(
          div(h3(textOutput("resultsTitle")), style = "color: ghostwhite"),
          div(h2(textOutput("res")), style = "color:blue"),
          div(textOutput("resCor"), style = "color:red"),
          uiOutput("resultsUI")
          ,width=4),

        #column(div(style = "height:290px; border: 1px solid white"), width = 1) ##no logo option as placeholder
        #column(div(img(src = "https://steamuserimages-a.akamaihd.net/ugc/1297549469329716123/31D41C3CD90B4732D55509ABD4D345DDA4373EE9/?imw=5000&imh=5000&ima=fit&impolicy=Letterbox&imcolor=%23000000&letterbox=false", height = 300, width = 300)), width = 2) ##dark logo option

        column(div(img(src = "https://logodownload.org/wp-content/uploads/2016/11/formula-1-logo-0.png", height = 300, width = 300)), width = 2) ##light logo option
      ),
      
##********************************************************************************
##2 Second Row
      
      fluidRow(class = "limRow",
        
##********************************************************************************
##2.1 First Column (Race Info & Map)
        column(
          div(h3(textOutput("raceTitle")),style = "color: ghostwhite"),
          div(strong(tableOutput("raceTable")), style = "background-color:rgba(255,0,0, 0)"),
          plotOutput("plot"),
          width=7),
        
##********************************************************************************
##2.2 Second Column (Drivers Info)
        column(
          div(h3(textOutput("driversTitle")),style = "color: ghostwhite"),
          div(strong(tableOutput("driverTable")), style = "background-color:rgba(255,0,0, 0); overflow-y: scroll; height: 400px"),
          width=4)
        , style = "border: 4px solid black; background-color:rgba(255,0,0, 0.8)"
        
        
      ), ##second fluidRow closing bracket
tags$head(tags$style(".limRow{height:490px; width: 1000px}")), ##second fluid row div closing bracket

    ) ##mainPanel closing bracket
    
)##sidebarLayout closing bracket
) ##UI closing bracket 

##********************************************************************************
##Server
##********************************************************************************

server <- function(input, output, session){
  
##********************************************************************************
##Simple Text Outputs


  
  
##Title
output$headTitle <- renderText("Who won the Grand Prix?")


##Sidebar
  ##Info
output$howtoTitle <- renderText("How to play:")
output$howtoInfo <- renderText("After clicking *Start* a random GP from the past 10 years will be generated.
                              You have to guess which driver won and place a bet on it. If your guess is correct you
                               win. The game continues until you run out of money or hit *End Game*. Good luck!")

output$info <- renderText("How well do you know your favorite motorsport? Based on real Formula One data!")
  ##Balance
output$balanceTitle <- renderText("Balance")
  ##Wikipedia Cut
    ##Title
output$wikiTitle <- renderText("Formula One")
    ##Content
output$wikipedia_f1 <- renderText("Formula One (also known as Formula 1 or F1) is the highest class 
                                  of international racing for open-wheel single-seater formula racing 
                                  cars sanctioned by the Fédération Internationale de l'Automobile 
                                  (FIA). The World Drivers' Championship, which became the FIA Formula 
                                  One World Championship in 1981, has been one of the premier forms of 
                                  racing around the world since its inaugural season in 1950. The word 
                                  formula in the name refers to the set of rules to which all 
                                  participants' cars must conform. A Formula One season consists of 
                                  a series of races, known as Grands Prix, which take place worldwide 
                                  on both purpose-built circuits and closed public roads.")

##First Row
  ##First Column
output$inputTitle <- renderText("Bets & Guesses")
##Second Column
output$resultsTitle <- renderText("Your Results")

##Second Row
  ##First Column
output$raceTitle <- renderText("The Grand Prix")
  ##Second Column
output$driversTitle <- renderText("The Drivers")


##********************************************************************************
##Render reactive Values
##********************************************************************************

values <- reactiveValues()

##********************************************************************************
##Render Bet & Guess Inputs
##****************************************

##Render as UI
observeEvent(input$startBut,{
output$inputGuessBet <- renderUI( ##guess & bet
  
##As input Panel
  inputPanel( ##guess & bet
    
    
##****************************************
##(in round) Guess Input
##****************************************
    
    selectInput("guess", "Who will win?", values$choice), #Input guess
    
    
##****************************************
##(in round) Bet Input
##****************************************

    sliderInput("bet", "How much do you want to bet?", 0, min = 0, max = values$balance),#Input bet
    
    
##****************************************
##(in round) Accept Button
##****************************************    
    
    actionButton("accept", "Submit") #Submit button

  ) ## input panel closing bracket
) ## render UI closing bracket
})


output$waitRes <- renderText("Your result --->")
observeEvent(input$accept,{
  output$inputGuessBet <- renderUI(div(h3(textOutput("waitRes")), style = "color:red"))
 
})

##****************************************
##Cheats
##****************************************
output$cheatSwitch <- renderUI(
  prettyToggle(inputId = "cheatToggle",  label_on= "cheats on", label_off = "cheats off")
)


observeEvent(input$startBut,{
  output$cheatSwitch <- renderUI(textOutput(""))
observeEvent(input$cheatToggle, {
  if(input$cheatToggle == TRUE){
    output$cheat <- renderUI(
      div(textOutput("winnerCheat"), style = "color:red")
    )
  }else{
    output$cheat <- renderUI(
      div(textOutput(""), style = "color:red")
    )
  }
})
})


##****************************************
##(at start) Start Button
##****************************************

output$startBut <- renderUI(
  actionButton("startBut", values$butText),
  )

values$butText <- "Start"

observeEvent(input$startBut, {
  output$startBut <- renderUI(actionButton("endBut", "End Game"))
})

observeEvent(input$endBut, {
  output$end <- renderText("The game is over!")
  values$game <- 0
})



##********************************************************************************
##(per round) Generate random race
##****************************************

observeEvent(input$startBut, { 
  
##**********
##Change data.frame to only use data from 2011 to 2021 (since not all races from before have all the results)
 values$comb2011 <- read.csv("combined_data.csv")
 
  
##**********
##Find random race and save its race id by only looking at the first row  
  values$raceInfo <- values$comb2011 %>% filter(raceId==sample(raceId, 1))
  
##**********
##Save the codes (short names) from all the drivers from the race to display them later as choices
  values$choice <- values$raceInfo[,"code"]
  
##****************************************
##(per round) Find winner
##****************************************
values$raceInfo$year <- format(as.numeric(values$raceInfo$year), digits = 0)
  values$whoWin <- values$raceInfo %>% filter(position==1)
  values$winner <- c(values$whoWin$code,values$whoWin$surname, values$whoWin$forename)
  output$winnerCheat <- renderText(c("Cheat: The winner was:", values$winner))##comment to turn cheats off
})


##****************************************
##(per round) Generate Race Info
##****************************************

output$raceTable <- renderTable({
  info <- values$raceInfo[ 1,c("name.x","name","year","location", "country")] 
},colnames = FALSE, spacing = "xs")

##****************************************
##(per round) Generate Drivers Info
##****************************************

output$driverTable <- renderTable({
  raceInfo <- values$raceInfo[,c("forename", "surname","code")]
}, spacing = "xs")

##****************************************
##(per round) Generate Map Plot
##****************************************
world <- map_data("world")
output$plot <- renderPlot({

  ggplot(data = map_data("world"),aes(long,lat)) + geom_map(map = world,aes(map_id = region),
                      color = "black", fill = "lightblue", size = 0.2, show.legend = FALSE) + theme_map() 
}, width = 480, height = 320) 

##****************************************
##Display results
##****************************************
output$inGame <- renderText("Place your bet!")

observeEvent(input$accept,{
output$resultsUI <- renderUI(
  actionButton("startBut", "Next Round")
)
})

observeEvent(input$startBut,{
  output$resultsUI <- renderUI(
    div(h3(textOutput("inGame")), style = "color:red")
    )
})

##**********
##Display location when round start

dataMap <- read.csv("combined_data.csv")
observeEvent(input$startBut,{
output$plot <- renderPlot({
  
  coLat <- mean(values$raceInfo$lat)
  coLng <- mean(values$raceInfo$lng)
  
  ggplot(data = map_data("world"),aes(long,lat)) + geom_map(map = world,aes(map_id = region),
                      color = "black", fill = "lightblue", size = 0.2,show.legend = FALSE) +
    geom_point(data = dataMap, aes(coLng, coLat, color = "red"), size = 2, show.legend = FALSE) + theme_map() 
  
}, width = 480, height = 320)
})
##********************************************************************************
##see if guess is right
##****************************************




observeEvent(input$accept,{
  output$highscore <- renderText(values$highscore)
  output$highscoreText <- renderText("Your highscore was:")
  output$rounds <- renderText(values$round)
  output$roundText <- renderText("Rounds you played:")
  ##save inputs as reactive variables
  values$bet <- input$bet
  values$guess <- input$guess
  
  output$resCor <- renderText(c("The winner was:", values$winner))
  
  ##renderoutputs for variables
  output$bet <- renderText(values$bet)
  output$guess <- renderText(values$guess)
  
  ##see if guess is correct
  if(values$guess %in% values$winner){
    output$res <- renderText("You win!")
    values$balance <-  values$balance+values$bet
    
    ##Calculate Highscore
    if(values$highscore < values$balance){
      values$highscore <- values$balance
    }
  }else{
    output$res <- renderText("You lose!")
    output$gameoverText <- renderText("Game Over!")
    values$balance <-  values$balance-values$bet
    if(values$balance <= 0){
      showModal(
      modalDialog(div(h2(textOutput("gameoverText")), style = "color:red"), textOutput("highscoreText"), textOutput("highscore"),
                  textOutput("roundText"), textOutput("rounds"),
      footer = list(actionButton("gameover", "Quit"), actionButton("restart", "Restart")))
      )
      observeEvent(input$restart,{
        session$reload()
      })
    observeEvent(input$gameover,{
      stopApp("Game Over!")
    })
      

      
    }
  }
  

})
observeEvent(input$startBut,{
  output$res <- renderText("")
  output$resCor <- renderText("")
  values$round <- values$round+1

})


##********************************************************************************
##(fix) Generate Balance & highscore Table
##****************************************

##Define starting balance
balance <- 100


values$balance <- balance
values$highscore <- balance

values$round <- 0




tableBalance <- reactive({
  data.frame(Balance = values$balance,
             Highscore = values$highscore,
             Rounds = values$round)
})

output$balanceTable <- renderTable({tableBalance()})






##********************************************************************************
##(end) show highscore and end game when endBut clicked
##****************************************

output$quitText <- renderText("You quit the game!")
observeEvent(input$endBut, {
showModal(
  modalDialog(div(h2(textOutput("quitText")), style = "color:red"),textOutput("highscoreText"), textOutput("highscore"),
              textOutput("roundText"), textOutput("rounds"),
              footer = list(actionButton("playerQuit", "Quit"), actionButton("restart", "Restart"))
              )
)
  observeEvent(input$restart,{
    session$reload()
  })
observeEvent(input$playerQuit,{
  stopApp("Game Quit!")
})
})

} ##Server closing bracket



##********************************************************************************
##Start App
##********************************************************************************

shinyApp(ui, server)


