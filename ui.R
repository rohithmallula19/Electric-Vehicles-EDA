ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        //background-image: linear-gradient(to left, #d66d75, #d6ae7b);
        background: url('16532.jpeg') no-repeat;
        
        color: black;
        font-family: 'Yusei Magic', sans-serif;
      }
      h2 {
        //font-family: 'Yusei Magic', sans-serif;
      }
      .shiny-input-container {
        color: #474747;
      }
      a{
       color: white;
       font-weight: bold;
       //font-family: 'Yusei Magic', sans-serif;
      }
      .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover{
       color: black;
      }
      .well{
        border: 1px solid #d66d75;
        box-shadow: 10px 10px 10px burlywood;
        background-image: linear-gradient(to left, #d66d75, #d6ae7b);
      }
      .irs--shiny .irs-line {
      background: linear-gradient(to bottom, #dedede -50%,#d6ae7b 150%);
      background-color: #d6ae7b;
      border: 1px solid #d6ae7b;
      }
      .selectize-input.full {
       background-color: #dd66;
       border: 2px solid #d6ae7b;
      }
     .selectize-input, .selectize-control.single .selectize-input.input-active {
       background-color: #d66d75;
       border: 2px solid #d66d75;
     }
    .selectize-dropdown, .selectize-dropdown.form-control {
       background-color: #d66d75;
       border: 2px solid #d66d75;
    }
    .irs--shiny .irs-handle {
    border: 1px solid white;
    background-color: white;
    }
   .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
     background-color: #d66d75;
   }
    .irs--shiny .irs-bar {
    border-top: 1px solid #d66d75;
    border-bottom: 1px solid #d66d75;
    background: #d66d75;
    }
    .row{
    margin-top: 20px;
    }
                    "))
  ),
  fluidPage(
    column(4,offset = 4, titlePanel("Electric Vehicles New York")) 
  ),    
  sidebarLayout(position = "left",
                sidebarPanel(
                  conditionalPanel(condition = "input.tabs1==1",
                                   selectInput("Brand","Brand",choices=sort(unique(EV$Make)),selected = "Chevrolet"),
                                   sliderInput("Year","Year",min = 2017,max = 2021,value= 2017,sep="")
                                  ),
                  conditionalPanel(condition = "input.tabs1==2",
                                   radioButtons("E_R","Emissions_Rebate",choices=c("CO2","Petrol","Rebate_Amount"))
                                   ),
                  conditionalPanel(condition = "input.tabs1==3",
                                   sliderInput("E_Year","Year",min = 2017,max = 2021,value= 2017,sep=""),
                                   radioButtons("Emissions","Emissions",choices=c("CO2","Petrol"))
                                   ),
                  conditionalPanel(condition = "input.tabs1==4",
                                   selectInput("map_Brand","Brand",choices=sort(unique(EV$Make)),selected = "Chevrolet"),
                                   sliderInput("map_Year","Year",min = 2017,max = 2021,value= 2017,sep=""),
                                   radioButtons("map","Choose",choices=c("Car Density","CO2","Petrol","Rebate_Amount"))
                                   ),
                  conditionalPanel(condition = "input.tabs1==5",
                                   radioButtons("car_county","Choose",choices=c("Car","County")),
                                   selectInput("Top5_Year","Year",choices=sort(unique(EV$Year))),
                                   radioButtons("Top5_EVtype","EV Type",choices = c("PHEV","BEV","PHEV & BEV"))
                                  )

                  
                ),
                mainPanel(
                  tabsetPanel(id="tabs1",
                              tabPanel(icon = icon("car"),"Models and OwnerShip",value=1,
                                       fluidRow(
                                         column(6,uiOutput("UIout1")),
                                         column(6,uiOutput("UIout2")))),
                              tabPanel(icon=icon("money"),"Rebate",value=1,
                                       fluidRow(
                                         column(6,uiOutput("UIout5")),
                                         column(6,uiOutput("UIout4")))),
                              tabPanel(icon=icon("chart-line"),"Emissions and Rebate",value = 2,
                                       fluidRow(
                                column(12,uiOutput("UIout7")),
                                column(6,uiOutput("UIout10")),
                                column(6,uiOutput("UIout11")))),
                              tabPanel(icon=icon("gas-pump"),"Emissions Brand",value = 3,uiOutput("UIout8")),
                              tabPanel(icon=icon("globe"),"Map",value=4,uiOutput("UIout3")),
                              tabPanel(icon=icon("trophy"),"Top5",value = 5 ,uiOutput("UIout9"))
                              
                  )
                ))
  )


