##-------------------------
## Shiny UI
## Author: Peng Yang
##-------------------------

## Load package
library(shiny)
library(base64enc)
# library(shinyjs)
# library(shinyBS)
# library(DT)
CP <- base64enc::dataURI(file="CP.png", mime="image/png")

ui <- fluidPage(
  # numericInput(inputId = "Num",
  #              "Sample size", value = 15),
  # plotOutput(outputId = "LinePlot")
  
  
  ##--------------------------- tags header --------------------------------##
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                    color: red;
                    }
                    ")),
    
    tags$style(HTML('#simu_btn{  width:100%; }')),
    tags$style(HTML('#compBtnClick{  width:100%; }'))
  ),
  
  ##--------------------------- fixed Row --------------------------------##
  
  fixedRow(
    
    column(12,
           fixedRow(
             div(style="color:black",
                 align = "center",
                 headerPanel("Consortium for the Study of Chronic Pancreatitis, Diabetes and Pancreatic Cancer"
                 )
             ),
             column(12,br()),
             column(12,tags$h4("Dr. Temel Tirkes (chair of the imaging working group), Dr. Naoki Takahashi, Dr. Joseph Grajo, Dr. Benjamin Spilseth, Dr. Anil Dasyam, Dr. Sudhakar Venkatesh, Dr. Darwin Conwell, Dr. Christopher Forsmark, Dr. Andrew Trout, Dr. Maisam Abu-El-Haija, Dr. Zarine Shah",
                               align = "Center", style = "color:black")),
             column(12, tags$h4(tags$em("The Consortium for the Study of Chronic Pancreatitis, Diabetes and Pancreatic Cancer (CPDPC) was formed to undertake a comprehensive clinical, epidemiological, and biological characterization of patients with Chronic Pancreatitis including those with Acute Recurrent Pancreatitis and ARP."),
                                align = "Center", style = "color:grey")
             )
           )
    )
  ),
  
  ##--------------------------- Start tabset --------------------------------##
  tabsetPanel(type="tabs", 
              id = "tabsetPnl",
              
              ##--------------------------- Introduction --------------------------------##
              
              tabPanel(
                value = 'Introduction',
                tags$strong(tags$p(tags$h4("Introduction", style="color:black"))),
                br(),
                column(5, tags$img(src = CP, height = '200px', width = '400px')
                       ###   Input of endpoints  ###
                       # wellPanel(style = "background-color: lightgrey;",
                       #           div(
                       #             fixedRow(
                       #               ## Select the different endpoints
                       #               column(10,tags$h4("Endpoints:")),
                       #               ## Choice of binary and continuous
                       #               column(10, radioButtons("endpoints", label = NULL, choices = c('Binary', 'Continuous'), inline = T))
                       #             )
                       #           )
                       # )
                ),
                column(5, tabsetPanel( id = "OCtabs",
                                       tabPanel(tags$h4( tags$strong("CP-MRI Score for CP"),
                                                         align = "center", style = "color:coral"),
                                                value = 1,
                                                br(), 
                                                column(12, htmlOutput("opt_intro"))
                                                # br(),
                                                # column(12, plotOutput("CP_plot", width="60%", height="700px"))
                                                
                                                # dataTableOutput("OC_table"),
                                                # column(12,br()),
                                                # uiOutput("OC_plot_download")
                                                # uiOutput("OC_plot_caption")
                                                # column(12, htmlOutput("OC_plot_caption")),
                                                # column(12,downloadButton("download_OC_plot",HTML("Download OC plot"))),
                                                # column(12,plotOutput("OC_plot", width="60%", height="700px"))
                                                
                                       )))
              ), ## End of introduction
              
              
              
              ## ----------------------- User Input -----------------------  ##
              tabPanel(
                value = 'Score',
                tags$strong(tags$p(tags$h4("Score", style="color:black"))),
                br(),
                column(5,
                       ###   Input of T1 DIXON series :  ###
                       wellPanel(style = "background-color: lightgrey;",
                                 div(
                                   fixedRow(
                                     ## Input 1
                                     column(12,tags$h4("T1 DIXON series:")),
                                     column(6, numericInput("fat",
                                                            label=HTML("<span class='tex2jax_ignore'>Fat only signal intensity (S1)\n</span>"),
                                                            value = 1,  min = 0, max = 1000, step=1)),
                                     column(6, numericInput("water",
                                                            label=HTML("<span class='tex2jax_ignore'>Water only signal intensity (S1)\n</span>"),
                                                            value = 1,  min = 0, max = 1000, step=1)), 
                                     ## Input 2
                                     column(12,tags$h4("T1W gradient echo with fat suppression:")),
                                     column(4, numericInput("Unenhanced",
                                                            label=HTML("<span class='tex2jax_ignore'>Unenhanced phase\n</span>"),
                                                            value = 1,  min = 0, max = 1000, step=1)),
                                     column(4, numericInput("Arterial",
                                                            label=HTML("<span class='tex2jax_ignore'>Arterial phase\n</span>"),
                                                            value = 2,  min = 0, max = 1000, step=1)),
                                     column(4, numericInput("Venous",
                                                            label=HTML("<span class='tex2jax_ignore'>Venous phase\n</span>"),
                                                            value = 3,  min = 0, max = 1000, step=1)),
                                     ## Input 3
                                     column(12,tags$h4("Pancreatic tail diameter (PTD):")),
                                     column(12, numericInput("PTD",
                                                             label=NULL,
                                                             value = 1,  min = 0, max = 1000, step=1)),
                                     ## Input 4
                                     column(12,tags$h4("Pancreatic ductal elasticity (PDE):")),
                                     column(12, radioButtons("PDE", label=HTML("<span class='tex2jax_ignore'>Enter only if secretin enhanced MRCP was performed\n</span>"),
                                                             choices = c('Not perfomded', 'Present', 'Not present'), inline = T)),
                                     ## Action
                                     column(12,br()),
                                     column(12,actionButton("Action","Calculation",icon = icon("play-circle"),width="100%",class="btn-success"))
                                   )
                                 )
                                 
                       )
                ),
                
                column(5, tabsetPanel( id = "OCtabs",
                                       tabPanel(tags$h4( tags$strong("CP-MRI Score for CP"),
                                                         align = "center", style = "color:coral"),
                                                value = 1,
                                                br(), 
                                                
                                                column(12, htmlOutput("score")),
                                                column(12,htmlOutput("tbl_caption")),
                                                column(12, dataTableOutput("tbl"))
                                       )))
                
              )
              
              
  )
  
  
  
)



# library(imager)
server <- function(input, output) {
  
  ## Output the introduction
  output$opt_intro <- renderText({ 
    
    res <- 'Enter the average region of interest measurements of the pancreas in a homogenous part of the gland avoiding vessels, dilated duct, retroperitoneal fat or adjacent small bowel loop.  Fat fraction is calculated using DIXON series fat only and water only fraction.  Arterial-to-venous enhancement ratio (AVR) was calculated by measuring the T1W signal during the unenhanced, arterial, and venous phases. Pancreatic tail diameter (PTD) is measured perpendicular to the pancreatic duct (or axis of the pancreas).  Pancreatic ductal elasticity (PDE) is optional parameter to generate CP-SMRI score.  PDE is either “present” or “not present” based on increase in pancreatic ductal diameter more than 1 mm during any image of the post-secretin dynamic phase.'
    
    res
    
  })
  
  
  
  observeEvent(input$Action, {
    
    ## Calculate the FF and AVR
    FF <- input$fat / (input$fat + input$water)
    AVR <- (input$Arterial - input$Unenhanced) / (input$Venous - input$Unenhanced)
    
    
    ## Calculate CP - MRI score
    CP_MRI <- 4.47 + 8.57 * FF - 0.869 * AVR - (0.635 * input$PTD)
    print(CP_MRI)
    # print(input$)
    
    if(input$PDE == 'Not perfomded'){
      CP_SMRI <- NA
    }else{
      CP_SMRI <- 4.39 + 8.18 * FF - 0.858 * AVR - 0.598 * input$PTD + as.numeric(input$PDE == 'Not present') 
    }
    
    ## Output the CP-MRI score
    output$score <- renderText({ 
      
      isolate({
        
        res <- paste0('The calculated CP - MRI Score is : ', min(round(CP_MRI,1), 5), '; and the CP - SMRI score is : ', min(round(CP_SMRI, 1), 5), '.')
        
        res
        
      })
      
      
      
    })
    
    output$tbl_caption <- renderText({
      
      '<B>Table 1.</B> Diagnostic performance of CP-MRI and CM-SMRI scores. CP-MRI Score optimal threshold determined by the Youden’s index of 2.7 yields sensitivity of 0.82 and specificity of 0.73. CP-SMRI score optimal threshold determined by the Youden’s index of 2.7 yields sensitivity of 0.88 and specificity of 0.70.'
      
      
    })
    
    ## Output the table
    output$tbl <- renderDataTable({
      
      isolate({
        
        tbl <- matrix(c(100, 100, 99, 72, 27, 5, 2, 7, 43, 80, 95, 99, 100, 100, 99, 78, 40, 21,
                        2, 6, 40, 76, 92, 99), ncol = 4)
        
        tbl <- data.frame(tbl)
        names(tbl) <- c('Sens of CPR-MRI', 'Spec of CPR-MRI', 'Sens of CPR-SMRI', 'Spec of CPR-SMRI')
        
        tbl
        
      })
      
    })
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
