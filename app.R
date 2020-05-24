library(shiny)

# User Interface 
ui <- fluidPage(
  titlePanel("Differential Prediction by Protected Class"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="outcome", 
                  label=h3("Criterion"),
                  choices=list("Performance"="PerfEval",
                               "Sales"="Sales"), 
                  selected = 1),
      selectInput(inputId="predictor", 
                  label=h3("Selection Tool"),
                  choices=list("Interview"="Interview",
                               "Work Sample"="WorkSample"), 
                  selected=1),
      selectInput(inputId="moderator", 
                  label=h3("Protected Class"),
                  choices=list("Age"="Age",
                               "Gender"="Gender",
                               "Race"="Race"), 
                  selected=1)      
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Distributions", # Plots of distributions
                           fluidRow(
                             column(4, plotOutput("distribution1")),
                             column(4, plotOutput("distribution2")),
                             column(4, plotOutput("distribution3")),
                             column(6, plotOutput("distribution4")),
                             column(6, plotOutput("distribution5")))),
                  tabPanel("Scatterplot", # Plots of distributions
                           fluidRow(plotOutput("scatter1"))),
                  tabPanel("Model Summary", verbatimTextOutput("summary")),
                  tabPanel("Interaction Plot", 
                          fluidRow(plotOutput("interact"))),
                  tabPanel("Simple Slopes", verbatimTextOutput("slopes"))
      ))))

# Server
server <- function(input, output) {
  
  # Set up data
  library(readr)
  library(ggplot2)
  library(tidyr)
  library(interactions)
  library(dplyr)
  df <- read_csv("MMR.csv")
  df$Interview <- df$Interview - mean(df$Interview, na.rm=TRUE)
  df$WorkSample <- df$WorkSample - mean(df$WorkSample, na.rm=TRUE)
  df$Age <- ifelse(df$Age < 40, "under_40", "over_40")
  drop_na(df)
  df <- as.data.frame(df)
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    ggplot() +
      aes(df[,input$predictor]) +
      geom_histogram(fill="darkgrey") +
      labs(x="Selection Tool", y="Frequency") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=300)
  
  # Bar chart output var 2
  output$distribution2 <- renderPlot({
    ggplot() +
      aes(df[,input$moderator]) +
      geom_bar(fill=c("#F8766D", "#00BFC4")) +
      labs(x="Protected Class", y="Frequency") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
    }, height=300, width=300)
  
  # Histogram output var 3
  output$distribution3 <- renderPlot({
    ggplot() +
      aes(df[,input$outcome]) +
      geom_histogram(fill="black") +
      labs(x="Criterion", y="Frequency") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=300)
  
  # Histogram output var 4
  output$distribution4 <- renderPlot({
    ggplot() +
      aes(x=df[,input$predictor], fill=df[,input$moderator]) +
      geom_histogram(position="identity", alpha=.7) +
      labs(x="Selection Tool", y="Frequency", fill="Protected Class") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=500)
  
  # Histogram output var 5
  output$distribution5 <- renderPlot({
    ggplot() +
      aes(x=df[,input$outcome], fill=df[,input$moderator]) +
      geom_histogram(position="identity", alpha=.7) +
      labs(x="Criterion", y="Frequency", fill="Protected Class") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=300, width=500)
  
  # Scatterplot 1 output
  output$scatter1 <- renderPlot({
    ggplot() +
    aes(x=df[,input$predictor], y=df[,input$outcome], color=df[,input$moderator]) +
      geom_point() +
      labs(x="Selection Tool", y="Criterion", color="Protected Class") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            panel.border=element_blank(),
            panel.background=element_blank())
  }, height=600, width=800)
  
  # Regression output
  output$summary <- renderPrint({
    mod1 <- lm(df[,input$outcome] ~ df[,input$predictor]*df[,input$moderator])
    names(mod1$coefficients) <- c("Intercept", input$predictor, input$moderator, "Interaction")
    summary(mod1)
  })
  
  # Interaction plot
  output$interact <- renderPlot({
    ggplot() +
      aes(x=df[,input$predictor], y=df[,input$outcome], color=df[,input$moderator]) +
      geom_point() +
      geom_smooth(method="lm") + 
      labs(x="Selection Tool", y="Criterion", color="Protected Class") +
      theme(axis.title=element_text(size=20),
            axis.text=element_text(size=16),
            legend.title=element_text(size=20),
            legend.text=element_text(size=16),
            axis.line = element_line(colour="black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  }, height=600, width=800)
  
  # Simple Slopes
  output$slopes <- renderText({

    mod1 <- lm(df[,input$outcome] ~ df[,input$predictor]*df[,input$moderator])
    
    b0 = summary(mod1)$coefficients[1,1]
    b1 = summary(mod1)$coefficients[2,1]
    b2 = summary(mod1)$coefficients[3,1]
    b3 = summary(mod1)$coefficients[4,1]
    zlo = 0  
    zhi = 1
    
    covmat = vcov(mod1)
    var_b1 <- covmat[2,2]
    var_b3 <- covmat[4,4]
    var_b13 <- covmat[2,4]
    
    ss_hi = b1 + b3*zhi
    se_hi = sqrt(var_b1 + (zhi^2)*var_b3 + 2*zhi*var_b13)
    t_hi = ss_hi/se_hi
    df = nrow(model.frame(mod1)) - 3 - 1
    p_hi = 2*pt(-abs(t_hi), df=df)
    
    ss_lo = b1 + b3*zlo
    se_lo = sqrt(var_b1 + (zlo^2)*var_b3 + 2*zlo*var_b13)
    t_lo = ss_lo/se_lo
    p_lo = 2*pt(-abs(t_lo), df=df)
    
    if(input$moderator=="Age"){
      line1 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is over 40 ", 
                      "= ", round(ss_lo, 3),
                      " (SE=", round(se_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      line2 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is under 40 ",  
                    "= ", round(ss_hi, 3),
          " (SE=", round(se_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
    full <- paste0(line1, "\n", line2)
    print(full)}
    
    else if(input$moderator=="Gender"){
      line1 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is woman ", 
                      "= ", round(ss_hi, 3),
                      " (SE=", round(se_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
      line2 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is man ", 
                      "= ", round(ss_lo, 3),
                      " (SE=", round(se_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      full <- paste0(line1, "\n", line2)
      print(full)}
    
    else if(input$moderator=="Race"){
      line1 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is Asian ", 
                      "= ", round(ss_hi, 3),
                      " (SE=", round(se_hi, 3), ", t=", round(t_hi, 3), ", p=", round(p_hi, 3), ")")
      line2 <- paste0("SS of ", input$predictor, " when ", input$moderator, " is Black ", 
                      "= ", round(ss_lo, 3),
                      " (SE=", round(se_lo, 3), ", t=", round(t_lo, 3), ", p=", round(p_lo, 3), ")")
      full <- paste0(line1, "\n", line2)
      print(full)}
  })
  
}

# shiny App
shinyApp(ui=ui, server=server)
