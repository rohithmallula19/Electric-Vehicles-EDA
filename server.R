server <- function(input, output){

  # Brands and their Models data frame for Bar plot
  brands_df <- reactive({ EV %>%
      filter(
        Make == input$Brand,
        Year == input$Year)%>%
      group_by(Model)})
  
  # Transaction type for the brands Data frame for pie chart
  counts_df <- reactive({brands_df()%>%
      group_by(Make,Transaction.Type)%>%
      summarise(count =n())%>%
      mutate(prop = (count/sum(count))*100)})
  
  # Total count of cars with brand and year as input for maps
  Map_df <- reactive({EV %>%
      group_by(Make,Year,County)%>%
      filter(Make == input$map_Brand,Year == input$map_Year)%>%
      summarise(count = n())})
  # Joining with county map
  Map_County <- reactive({inner_join(ny_county, Map_df(), by = "County")})
  
  # Total Rebate amount with brand and year as input for maps
  County_Rebate_df <- reactive({EV%>%
      group_by(Make,Year,County)%>%
      filter(Year == input$map_Year,Make == input$map_Brand)%>%
      summarise(Value = sum(Rebate_Amount))})
  
  # Joining with county map
  Map_County_Rebate <- reactive({inner_join(ny_county, County_Rebate_df(), by = "County")})
  
  # Total MTCO2E with brand and year as input for maps
  County_CO2_df <- reactive({EV%>%
      group_by(Make,Year,County)%>%
      filter(Year == input$map_Year,Make == input$map_Brand)%>%
      summarise(Value = sum(CO2))})
  
  # Joining with county map
  Map_County_CO2 <- reactive({inner_join(ny_county, County_CO2_df(), by = "County")})
  
  # Total Petrol reduced with brand and year as input for maps
  County_Petrol_df <- reactive({EV%>%
      group_by(Make,Year,County)%>%
      filter(Year == input$map_Year,Make == input$map_Brand)%>%
      summarise(Value = sum(Petrol))})
  
  # Joining with county map
  Map_County_Petrol <- reactive({inner_join(ny_county, County_Petrol_df(), by = "County")})
  
  # Rebate amount for each brand with input as brand and year
  Brand_Rebate_df <- reactive({ EV%>%
      filter(Year == input$Year,Make== input$Brand)%>%
      group_by(EV.Type)%>%
      summarise(Rebate = sum(Rebate_Amount))})
  
  # Rebate amount for every model in the brand with input as brand and year
  Model_Rebate_df <- reactive({EV%>%
      filter(Year == input$Year, Make== input$Brand)%>%
      group_by(Make,Model,Year,Rebate_Amount)%>%
      select(Make,Model,Year,Rebate_Amount,EV.Type)%>%
      unique()
    
  })
  
  # MTCO2E reduced every month for all the years
  Line_CO2_df <- reactive({EV%>%
      group_by(Month_Yr,EV.Type)%>%
      summarise(CO2_reduced_MetricTons = sum(CO2))%>%
      mutate(Year = as.Date(paste(Month_Yr,"-28",sep="")))
  })
  
  # Petrol reduced every month for all the years
  Line_Petrol_df <- reactive({df <- EV%>%
    group_by(Month_Yr,EV.Type)%>%
    summarise(Petrol_reduced_Gallons = sum(Petrol))%>%
    mutate(Year = as.Date(paste(Month_Yr,"-28",sep="")))
  })
  
  #Rebate amount for every month for all the years 
  Line_Rebate_df<- reactive({df <- EV%>%
    group_by(Month_Yr,EV.Type)%>%
    summarise(TotalUSD = sum(Rebate_Amount))%>%
    mutate(Year = as.Date(paste(Month_Yr,"-28",sep="")))
  })
  
  # MTCO2E for each brand and year
  CO2_df <- reactive({ EV %>%
      group_by(Make,Model,EV.Type,Year)%>%
      summarise(Emissions = sum(CO2))%>%
      arrange(desc(Emissions))%>%
      filter(Year == input$E_Year)})
  
  # Petrol reduced in gallons for brand and year
  Petrol_df <- reactive({ EV %>%
      group_by(Make,Model,EV.Type,Year)%>%
      summarise(Emissions = sum(Petrol))%>%
      arrange(desc(Emissions))%>%
      filter(Year == input$E_Year)})
  
  # MTCO2E each year
  CO2_Year_df <- reactive({EV %>%
      group_by(EV.Type,Year)%>%
      summarise(Total = sum(CO2))
  })
  
  # Petrol reduced each year 
  Petrol_Year_df <- reactive({EV %>%
      group_by(EV.Type,Year)%>%
      summarise(Total = sum(Petrol))
  })
  
  # Rebate amount each year
  Rebate_Year_df <- reactive({EV %>%
      group_by(EV.Type,Year)%>%
      summarise(Total = sum(Rebate_Amount))
  })
  
  # Over all top5 sold cars each year and input as EVtype and year
  Top5_df <- reactive({EV%>%
      filter(Year == input$Top5_Year)%>%
      group_by(Name,EV.Type)%>%
      summarise(Total = n())%>%
      arrange(desc(Total))%>%
      filter(EV.Type == input$Top5_EVtype)%>%
      ungroup()%>%
      top_n(5)
  })
  
  # PHEV and BEV together top sold cars
  Top5_PBEV_df <- reactive({EV%>%
      filter(Year == input$Top5_Year)%>%
      group_by(Name,EV.Type)%>%
      summarise(Total = n())%>%
      arrange(desc(Total))%>%
      ungroup()%>%
      top_n(5)
  })
  
  Top5_county <- reactive({EV%>%
      filter(Year == input$Top5_Year)%>%
      group_by(EV.Type,County)%>%
      summarise(Total = n())%>%
      arrange(desc(Total))%>%
      filter(EV.Type == input$Top5_EVtype)%>%
      ungroup()%>%
      top_n(5)
  })
  
  Top5_PBEV_county <- reactive({EV%>%
      filter(Year == input$Top5_Year)%>%
      group_by(County,EV.Type)%>%
      summarise(Total = n())%>%
      arrange(desc(Total))%>%
      ungroup()%>%
      top_n(5)
  })
  
  
  # Bar plot for each brand with model counts
  output$barplot <- renderPlot({ggplot(data = brands_df() ) + geom_bar(mapping = aes(x = Model, fill = Transaction.Type), position = "dodge",width = 0.5)+
      facet_wrap(~EV.Type,scales="free_x")+
      labs(x= paste(input$Brand,input$Year),y="Total Count",fill="OwnerShip")+
      ggtitle(paste(input$Brand,"Models of",input$Year))+
      theme(axis.text.x =element_text(angle =45,hjust = 1))+
      theme_bw()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.text.x = element_text(size=12,color="white", face = "bold"),
        axis.text.y = element_text(size=12,color="white", face = "bold"),
        plot.title = element_text(size=16,face = "bold", color = "white"),
        axis.title.x = element_text(size=12,color = "white", face="bold"),
        axis.title.y = element_text(size=12,color = "white", face="bold"),
        legend.title = element_text(size=12,color="black", face="bold")
      )+
      scale_fill_manual(values=c("#73d66d","#6dd6d0"))
  },bg="transparent")
  
  
  # Pie chart with owner ship style
  output$piechart <- renderPlot({ggplot(counts_df(), aes(x="", y=count, fill=Transaction.Type)) + geom_bar(stat="identity", width=1)+ coord_polar(theta = "y")+ 
      geom_text(aes(label = paste0(round(prop), "%")), position = position_stack(vjust = 0.5))+
      ggtitle(paste(input$Brand,"Ownership style of",input$Year))+
      labs(fill="OwnerShip")+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=16,face = "bold", color = "white"),
        legend.title = element_text(size=12,color="black", face="bold")
      )+
      scale_fill_manual(values=c("#73d66d","#6dd6d0"))
      },bg="transparent")
  
  # Map for the car density, CO2, Petrol, Rebate amount
  output$County_Map <- renderPlotly({map <- if(input$map == "Car Density"){
    NY_County_map + 
    geom_polygon(data = Map_County() , aes(fill = count,label = County), color = "black") +
    geom_polygon(color = "black", fill = NA)+
    scale_fill_continuous(
      low = "#56B1F7", high = "#132B43",na.value = "red",
      guide=guide_colorbar(barwidth = 2,barheight = 10))+
    ggtitle(paste("NY County Wise",input$map_Brand,"Cars Count For The Year",input$map_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(fill = "Density")+
    theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
    theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="black", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("blue", "red"))
    }
  else if(input$map == "CO2"){
    NY_County_map + 
      geom_polygon(data = Map_County_CO2() , aes(fill = Value,label = County), color = "black") +
      geom_polygon(color = "black", fill = NA)+
      scale_fill_continuous(
        low = "#56B1F7", high = "#132B43",na.value = "red",
        guide=guide_colorbar(barwidth = 2,barheight = 10))+
      ggtitle(paste("NY County Wise",input$map_Brand,"Cars Total CO2 Reduced For The Year",input$map_Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "MTCO2E")+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="black", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("blue", "red"))
  }
  else if(input$map == "Petrol"){
    NY_County_map + 
      geom_polygon(data = Map_County_Petrol() , aes(fill = Value,label = County), color = "black") +
      geom_polygon(color = "black", fill = NA)+
      scale_fill_continuous(
        low = "#56B1F7", high = "#132B43",na.value = "red",
        guide=guide_colorbar(barwidth = 2,barheight = 10))+
      ggtitle(paste("NY County Wise",input$map_Brand,"Cars Total Petrol Reduced For The Year",input$map_Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "Gallons")+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="black", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }
  else {NY_County_map + 
      geom_polygon(data = Map_County_Rebate() , aes(fill = Value,label = County), color = "black") +
      geom_polygon(color = "black", fill = NA)+
      scale_fill_continuous(
        low = "#56B1F7", high = "#132B43",na.value = "red",
        guide=guide_colorbar(barwidth = 2,barheight = 10))+
      ggtitle(paste("NY County Wise",input$map_Brand,"Cars Total Rebate Amount For The Year",input$map_Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill = "USD")+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size=18,face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="black", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
    
  }
  
    })
  
  # Pie chart for the whole rebate amount for the brand
  output$Piechart_Rebate<- renderPlot({ggplot(Brand_Rebate_df(), aes(x="", y=Rebate, fill= EV.Type)) + geom_bar(stat="identity", width=1)+ coord_polar(theta = "y")+ 
      geom_text(aes(label = paste0("$",Rebate)), position = position_stack(vjust = 0.5))+
      ggtitle(paste("Total Rebate Amount for",input$Brand,"Cars of",input$Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(fill="EV_Type")+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=12,face = "bold", color = "white"),
        legend.title = element_text(size=10,color="black", face="bold")
      )+
      scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
      },bg="transparent")
  
  # Bar chart for the Rebate amount for each model
  output$Barchart_Rebate_Model <- renderPlotly({Rebate_bar <-  ggplot(data = Model_Rebate_df() ) + geom_bar(mapping = aes(x = Model, y=Rebate_Amount, fill = EV.Type), stat = "identity",position="dodge",width=0.5)+
    labs(x= paste(input$Brand,input$Year),y="Total Amount",fill="EVType")+
    ggtitle(paste("Rebate for",input$Brand,"Models of",input$Year))+
    theme_classic()+
    theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
    theme(axis.text.x = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
          axis.text.y = element_text(size=10,color="white", face = "bold",family='Yusei Magic'),
          plot.title = element_text(size=12,face = "bold", color = "white", family='Yusei Magic'),
          axis.title.x = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
          axis.title.y = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
          legend.title = element_text(size=10,color="white", face="bold", family='Yusei Magic')
    )+
    theme(axis.text.x =element_text(angle =45,hjust = 1))+
    scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
  Rebate_bar<- ggplotly(Rebate_bar)
  Rebate_bar
  })
  
  #Line chart for the CO2 Petrol and Rebate
  output$Linechart <- renderPlotly({Linechart <- if(input$E_R == "CO2"){
    linechart <- ggplot(data=Line_CO2_df(), aes(x=Year, y=CO2_reduced_MetricTons)) +
      geom_line(aes(color = EV.Type))+
      scale_x_date(date_labels = '%Y')+
      ggtitle("Amount of CO2 Reduced by EVs in New York State")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  } else if(input$E_R == "Petrol"){
    linechart <- ggplot(data=Line_Petrol_df(), aes(x=Year, y=Petrol_reduced_Gallons)) +
      geom_line(aes(color = EV.Type))+
      scale_x_date(date_labels = '%Y')+ 
      ggtitle("Amount of Petrol Usage Reduced by EVs in New York State")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }else {
    linechart <- ggplot(data=Line_Rebate_df(), aes(x=Year, y= TotalUSD)) +
      geom_line(aes(color = EV.Type))+
      scale_x_date(date_labels = '%Y')+
      ggtitle("Rebate Amount Provided by Government for EVs in New York State")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 18, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }
  linechart <- ggplotly(linechart)
  linechart
  })
  
  # Circular Barplot for CO2 Petrol and Rebate amount
  output$Circularbar <- renderPlot({if(input$E_R == "CO2"){
    CBarplot<- ggplot(CO2_Year_df(), aes(x=Year, y=Total)) +
      geom_bar(stat="identity", fill = alpha("yellow", 0.6) ) + theme_minimal()+
      ggtitle("Total MTCO2E Reduced")+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x = "Year", y = "MTCO2E") +
      coord_polar(start = 0) +
      theme_minimal()+
    theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold"),
            axis.text.y = element_text(size=12,color="white", face = "bold"),
            plot.title = element_text(size=16,face = "bold", color = "white"),
            axis.title.x = element_text(size=12,color = "white", face="bold"),
            axis.title.y = element_text(size=12,color = "white", face="bold"),
            legend.title = element_text(size=12,color="white", face="bold")
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  } else if(input$E_R == "Petrol"){
    CBarplot<- ggplot(Petrol_Year_df(), aes(x=Year, y=Total)) +
      geom_bar(stat="identity", fill = alpha("blue", 0.6) ) + theme_minimal()+
      ggtitle("Total Petrol usage Reduced")+
      labs(x = "Year", y = "Petrol reduced gallons") + 
      theme_minimal() +
      coord_polar(start = 0) +
      theme_minimal()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold"),
            axis.text.y = element_text(size=12,color="white", face = "bold"),
            plot.title = element_text(size=16,face = "bold", color = "white"),
            axis.title.x = element_text(size=12,color = "white", face="bold"),
            axis.title.y = element_text(size=12,color = "white", face="bold"),
            legend.title = element_text(size=12,color="white", face="bold")
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }else {
    CBarplot<- ggplot(Rebate_Year_df(), aes(x=Year, y=Total)) +
      geom_bar(stat="identity", fill = alpha("green", 0.6) ) + theme_minimal()+
      ggtitle("Total Rebate Amount")+
      labs(x = "Year", y = "Rebate Amount") + 
      coord_polar(start = 0) +
      theme_minimal()+theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold"),
            axis.text.y = element_text(size=12,color="white", face = "bold"),
            plot.title = element_text(size=16,face = "bold", color = "white"),
            axis.title.x = element_text(size=12,color = "white", face="bold"),
            axis.title.y = element_text(size=12,color = "white", face="bold"),
            legend.title = element_text(size=12,color="white", face="bold")
      )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }
    CBarplot
  },bg="transparent")

  #Density plot for the Emissions and Rebate
  output$Density <- renderPlotly({if(input$E_R == "CO2"){
    Densityplot<- ggplot(EV, aes(x=CO2, color=EV.Type)) +
      geom_density()+
      ggtitle("CO2 Density Curve")+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.text.x = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        axis.text.y = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        plot.title = element_text(size=12,face = "bold", color = "white", family='Yusei Magic'),
        axis.title.x = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        axis.title.y = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        legend.title = element_text(size=10,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999","#E69F00", "#56B4E9"))
    
  } else if(input$E_R == "Petrol"){
    Densityplot<- ggplot(EV, aes(x=Petrol, color=EV.Type)) +
      geom_density()+
      ggtitle("Petrol Density Curve")+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.text.x = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        axis.text.y = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        plot.title = element_text(size=12,face = "bold", color = "white", family='Yusei Magic'),
        axis.title.x = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        axis.title.y = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        legend.title = element_text(size=10,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999","#E69F00", "#56B4E9"))
    
  }else 
    {
    Densityplot<- ggplot(EV, aes(x=Rebate_Amount, color=EV.Type)) +
      geom_density()+
      ggtitle(paste("Rebate Density Curve"))+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(
        axis.text.x = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        axis.text.y = element_text(size=10,color="white", face = "bold", family='Yusei Magic'),
        plot.title = element_text(size=12,face = "bold", color = "white", family='Yusei Magic'),
        axis.title.x = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        axis.title.y = element_text(size=10,color = "white", face="bold", family='Yusei Magic'),
        legend.title = element_text(size=10,color="white", face="bold", family='Yusei Magic')
      )+
      scale_color_manual(values = c("#999999","#E69F00", "#56B4E9"))
    
  }
    ggplotly(Densityplot)
  })
  
  # scatter plot for each brand MTCO2E and Petrol
  output$scatterplot_Emissions <- renderPlotly({if(input$Emissions == "CO2"){
    scatterplot<-ggplot(data = CO2_df(), mapping = aes(label = Model)) + 
      geom_point(mapping = aes(x = Make, y = Emissions , color = EV.Type))+
      ggtitle(paste("Reduction of Metric tons of CO2 For The Year",input$E_Year))+
      theme(plot.title = element_text(hjust = 0.5))+
      labs(x=paste("Brands",input$E_Year),y=paste(input$Emissions,"MTCO2E"))+
      theme_bw()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,angle =45,hjust = 1, color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size = 16, face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
            )+
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
     
  } else {scatterplot<-ggplot(data = Petrol_df(), mapping = aes(label = Model)) + 
    geom_point(mapping = aes(x = Make, y = Emissions , color = EV.Type))+
    ggtitle(paste("Usage Reduction of Petrol in Gallons For The Year",input$E_Year))+
    theme(plot.title = element_text(hjust = 0.5))+
    labs(x=paste("Brands",input$E_Year),y=paste(input$Emissions,"(Gallons)"))+
    theme_bw()+
    theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
    theme(axis.text.x = element_text(size=12,angle =45,hjust = 1, color="white", face = "bold", family='Yusei Magic'),
          axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
          plot.title = element_text(size = 16, face = "bold", color = "white", family='Yusei Magic'),
          axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
          axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
          legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
    )+
    scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9"))
  }
    scatterplot <- ggplotly(scatterplot)
    scatterplot
    })
  

  # Top5 sold cars barplot
  output$Top5_barchart <- renderPlotly({ if(input$car_county=="Car"){
    if(input$Top5_EVtype == "PHEV & BEV"){
    H_bar <- ggplot(Top5_PBEV_df(), aes(x = reorder(Name,Total), y=Total,fill=EV.Type)) +
      geom_bar(stat='identity') +
      coord_flip()+
      labs(x="Cars",y="Count")+
      ggtitle(paste("Most sold EVs in Newyork for the Year",input$Top5_Year))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme_light()+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
      )+
      scale_fill_manual(values=c("#999999", "#E69F00"))
  } else if (input$Top5_EVtype == "PHEV") {
    H_bar <- ggplot(Top5_df(), aes(x = reorder(Name,Total), y=Total)) +
      geom_bar(stat='identity',fill="#E69F00") +
      coord_flip()+
      labs(x="Cars",y="Count")+
      ggtitle(paste("Most sold PHEVs in Newyork for the Year",input$Top5_Year))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black"))+
      theme_light()+
      theme_classic()+
      theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
      theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
            axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
            plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
            axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
            legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
      )
  }
  
    else {
      H_bar <- ggplot(Top5_df(), aes(x = reorder(Name,Total), y=Total)) +
        geom_bar(stat='identity',fill="#999999") +
        coord_flip()+
        labs(x="Cars",y="Count")+
        ggtitle(paste("Most sold BEVs in Newyork for the Year",input$Top5_Year))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))+
        theme_light()+
        theme_classic()+
        theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
        theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
              axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
              plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
              axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
              axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
              legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
        )
    }
  }
    
    else {
      if(input$Top5_EVtype == "PHEV & BEV"){
        H_bar <- ggplot(Top5_PBEV_county(), aes(x = reorder(County,Total), y=Total,fill=EV.Type)) +
          geom_bar(stat='identity',position=position_dodge()) +
          coord_flip()+
          labs(x="County",y="Count")+
          ggtitle(paste("Counties with highest count of EVs in Newyork for the Year",input$Top5_Year))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          theme_light()+
          theme_classic()+
          theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
          theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
                axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
                plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
                axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
          )+
          scale_fill_manual(values=c("#999999","#E69F00"))
      } else if (input$Top5_EVtype == "PHEV") {
        H_bar <- ggplot(Top5_county(), aes(x = reorder(County,Total), y=Total)) +
          geom_bar(stat='identity',fill="#E69F00") +
          coord_flip()+
          labs(x="County",y="Count")+
          ggtitle(paste("Counties with highest count of EVs in Newyork for the Year",input$Top5_Year))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          theme_light()+
          theme_classic()+
          theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
          theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
                axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
                plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
                axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
          )
      }
      else {
        H_bar <- ggplot(Top5_county(), aes(x = reorder(County,Total), y=Total)) +
          geom_bar(stat='identity',fill="#999999") +
          coord_flip()+
          labs(x="County",y="Count")+
          ggtitle(paste("Counties with highest count of EVs in Newyork for the Year",input$Top5_Year))+
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))+
          theme_light()+
          theme_classic()+
          theme(plot.background = element_rect(fill = alpha('#d66d75', 0.7)))+
          theme(axis.text.x = element_text(size=12,color="white", face = "bold", family='Yusei Magic'),
                axis.text.y = element_text(size=12,color="white", face = "bold",family='Yusei Magic'),
                plot.title = element_text(size=16,face = "bold", color = "white", family='Yusei Magic'),
                axis.title.x = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                axis.title.y = element_text(size=12,color = "white", face="bold", family='Yusei Magic'),
                legend.title = element_text(size=12,color="white", face="bold", family='Yusei Magic')
          )
      }
      
    }
    
    H_bar <- ggplotly(H_bar)
    H_bar
    
  })
  
  # Rendering UI Outputs bar plot for models and ownership
  output$UIout1 <- renderUI({
    if(nrow(brands_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotOutput("barplot")
    }
  })
  
  # Rendering UI Outputs pie chart for ownership
  output$UIout2 <- renderUI({
    if(nrow(counts_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotOutput("piechart")
    }
  })
  
  # Rendering UI outputs New York map
  output$UIout3 <- renderUI({
    if(nrow(Map_df()) == 0){
      p(strong(paste("No"),input$map_Brand, paste("Cars for the year"),input$Year))
    } else {
      plotlyOutput("County_Map")
    }
    
  })
  
  # Rendering UI outputs pie chart rebate
  output$UIout4 <- renderUI({
    if(nrow(Brand_Rebate_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotOutput("Piechart_Rebate")
    }
    
  })
  
  # Rendering UI outputs Bar chart model
  output$UIout5 <- renderUI({
    if(nrow(Model_Rebate_df()) == 0){
      p(strong(paste("No"),input$Brand, paste("Cars for the year"),input$Year))
    } else {
      plotlyOutput("Barchart_Rebate_Model")
    }
    
  })
  
  # Rendering UI outputs line chart model
  output$UIout7 <- renderUI({
    plotlyOutput("Linechart")
  })
  
  # Rendering UI outputs Scatter plot
  output$UIout8 <- renderUI({
    plotlyOutput("scatterplot_Emissions")
  })
  
  # Rendering UI outputs Top5 bar charts
  output$UIout9 <- renderUI({
    plotlyOutput("Top5_barchart")
  })
  
  # Rendering UI outputs Circular bar plot
  output$UIout10 <- renderUI({
    plotOutput("Circularbar")
  })
  
  # Rendering UI outputs Density plot 
  output$UIout11 <- renderUI({
    plotlyOutput("Density")
  })

}
