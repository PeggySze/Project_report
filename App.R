#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(reshape2)
library(DT)

# Load data
OR_family_df <- read.table("OR_famliy.txt",header = TRUE,stringsAsFactors = FALSE,sep = "\t")
RNA_seq_table <- read.table("RNA_seq_table.txt",header = TRUE,stringsAsFactors = FALSE,sep = "\t")
phastCons_table <- read.table("phastCons_table.txt",header = TRUE,stringsAsFactors = FALSE,sep = "\t")

# Define UI for application that draws a histogram
ui <- navbarPage(
    title = "Project Report",
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    id = "nav",
    
    # Olfactory System
    tabPanel("Olfactory System",
             fluidRow(
                 column(12,align="center",
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 850px;",
                                  h2(strong("Investigate Potential Biological Function of expressed pseudo ORs"),align = "center"),
                                  br(),
                                  br(),
                                  img(src ="olfactory_system.png",height =700,style="vertical-align:middle")))
             )),
    
    # Olfactory receptor gene family
    tabPanel("OR Family",
             fluidRow(
                 column(7,align="center",
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                  h3(strong("The composition of OR gene family in different species")),
                                  br(),
                                  br(),
                                  br(),
                                  plotOutput("OR_family")
                                  )),
                 column(5,
                        selectInput("species","Species",c("All",unique(as.character(OR_family_df$Class)))),
                        br(),
                        br(),
                        br(),
                        DT::dataTableOutput("table")
                        )
             )
        
    ),
    
    # Pseudo ORs expression
    tabPanel("Pseudo ORs expression",
             fluidRow(
                 column(5,align="center",
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                  h3(strong("OR gene expression profiles")),
                                  br(),
                                  br(),
                                  img(src="OR_expression_profiler.png",height =400),
                                  br(),
                                  br(),
                                  br(),
                                  br(),
                                  p("Saraiva, L. R.et al A transcriptomic atlas of mammalian olfactory mucosae reveals an evolutionary influence on food odor detection in humans.")
                        )),
                 column(7,
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                  h3(strong("Pseudo OR expression heatmap"),align = "center"),
                                  selectInput("expression_species","Species",c("Human","Macaque","Marmoset","Rat","Mouse","Dog")),
                                  img(src="processed_PseudoORs_heatmap.png",width=1000)
                        )
                 )
             )
        
    ),
    
    # Research Framework
    tabPanel("Research Framework",
             fluidRow(
                 column(6,align="center",
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                  h3(strong("Do expressed pseudo ORs have potential biological function?")),
                                  img(src="workflow1.png",height=350),
                                  tabsetPanel(id="dataset",
                                              tabPanel("Bulk RNA-seq",br(),DT::dataTableOutput("RNA_seq_table")),
                                              tabPanel("Genome Variants Database",br(),img(src="variants_dataset.png",height=300)),
                                              tabPanel("phastCons",DT::dataTableOutput("phastCons_table")))
                                  )
                        ),
                 column(6,align="center",
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 800px;",
                                  h3(strong("What's the potential function of pseudo ORs?"),align = "center"),
                                  br(),
                                  br(),
                                  img(src="workflow2.png"),
                                  tabsetPanel(
                                      tabPanel("scRNA-seq",br(),br(),img(src="scRNA-seq.png",height=450))
                                  )
                        )
                 )
             )
        
    )
)







# Define server logic required to draw a histogram
server <- function(input, output) {
    output$table <- DT::renderDataTable(DT::datatable({
        df <- OR_family_df[,1:5]
        if (input$species=="Primates"){
            df <- OR_family_df[which(OR_family_df$Class=="Primates"),1:5]
        }
        if (input$species=="Rodentia"){
            df <- OR_family_df[which(OR_family_df$Class=="Rodentia"),1:5]
        }
        if (input$species=="Other"){
            df <- OR_family_df[which(OR_family_df$Class=="Other"),1:5]
        }
        df
    }))
    
    output$OR_family <- renderPlot({
        df <- OR_family_df[,1:4]
        if (input$species=="Primates"){
            df <- OR_family_df[which(OR_family_df$Class=="Primates"),1:4]
        }
        if (input$species=="Rodentia"){
            df <- OR_family_df[which(OR_family_df$Class=="Rodentia"),1:4]
        }
        if (input$species=="Other"){
            df <- OR_family_df[which(OR_family_df$Class=="Other"),1:4]
        }
        df <- melt(df,id="Species",variable.name="OR_type",value.name = "number")
        ggplot(data = df,aes(x=Species,y=number,fill=OR_type))+
            geom_bar(stat = "identity",position = "stack")+
            theme_bw()+
            coord_flip()+
            theme(axis.text.x = element_text(face="bold",size=8))
    })
    
    output$RNA_seq_table <- DT::renderDataTable(DT::datatable({
        RNA_seq_table
    }))
    
    output$phastCons_table <- DT::renderDataTable(DT::datatable({
        phastCons_table
    }))
   
}

# Run the application 
shinyApp(ui = ui, server = server)
