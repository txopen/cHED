library(shiny)
library(shinythemes)
library(shinybusy)

library(DT)
library(gtsummary)
library(gt)
library(shinycssloaders)

fluidPage(theme = shinytheme("spacelab"),
          
          headerPanel(title ="", windowTitle = "cHED"),
          
          #titlePanel("HEADS | FMUP"),
          
          titlePanel(a(href="http://bioestatisticas.wixsite.com/bioestatisticas", target="_blank",
                       img(src='ob.jpg', align = "right",height=60,width=150))),
          
          a(href="https://txor.netlify.app/", target="_blank",
            h1("Transplants' Open Registry (TxOR)")),
          
          navbarPage("calculated HED (version 0.0.4)",
                     tabPanel("Home", icon = icon("calculator"),
                              sidebarPanel(
                                HTML('<p><a href="https://txopen.github.io/histoc/index.html">
                                     <img src="logo_histoc.png" width=100 height=100>
                                     </a></p>'),
                                h5('{histoc} is an R package for histocompatibility analysis performed on transplantation.'),
                                h5('It provides functions to test compatibility between donors and transplant candidates.')
                                
                              ),
                              mainPanel(h1("calculated HLA Evolutionary Divergence (cHED)"),
                                        br(),
                                        h5("An application to calculate HLA Evolutionary Divergence for Class I and II loci."),
                                        h5("Human Leukocyte Antigens (HLA) are very polymorphic proteins that play a pivotal role on transplantation. HLA-A,-B and -C (class I) are present on virtually all our cells (except red blood cells) while HLA-DR, HLA-DQ and HLA-DP (class II) are mainly on immune cells. While HLA polymorphism may help diversify humanity, it is a huge barrier for successful transplantation, which requires matching, as closely as possible, the HLA types of the recipient and donor."),
                                        h5("HLA evolutionary divergence (HED) is a metric which reflects immunopeptidome diversity, i.e, it's a quantifiable measure for Grantham distance between the peptide-binding domains of the two alleles of each HLA locus."),
                                        h3("HLA Class I"),
                                        fluidRow(
                                          column(3, #selectizeInput('a1', choices = NULL, label = "HLA-A* allele 1"),
                                                 selectizeInput("a1",
                                                             "HLA-A* allele 1",
                                                             choices = character(0))
                                                 ),
                                          column(3, selectizeInput("a2", 
                                                                "HLA-A* allele 2", 
                                                                choices = character(0))
                                                 ),
                                          column(6,htmlOutput(outputId = "hed_a"))
                                        ),
                                        
                                        fluidRow(
                                          column(3, selectizeInput("b1", 
                                                                "HLA-B* allele 1", 
                                                                choices = character(0))),
                                          column(3, selectizeInput("b2", 
                                                                "HLA-B* allele 2", 
                                                                choices = character(0))),
                                          column(6,htmlOutput(outputId = "hed_b"))
                                        ),
                                        
                                        fluidRow(
                                          column(3, selectizeInput("c1", 
                                                                "HLA-C* allele 1", 
                                                                choices = character(0))),
                                          column(3, selectizeInput("c2", 
                                                                "HLA-C* allele 2", 
                                                                choices = character(0))),
                                          column(6,htmlOutput(outputId = "hed_c"))
                                        ),
                                        h3("HLA Class II"),
                                        fluidRow(
                                          column(3, selectizeInput("dr1", 
                                                                "HLA-DRB1* allele 1", 
                                                                choices = character(0))),
                                          column(3, selectizeInput("dr2", 
                                                                "HLA-DRB1* allele 2", 
                                                                choices = character(0))),
                                          column(6,htmlOutput(outputId = "hed_dr"))
                                        ),
                                        
                                        fluidRow(
                                          column(3, selectizeInput("dq1", 
                                                                "HLA-DQB1* allele 1", 
                                                                choices = character(0))),
                                          column(3, selectizeInput("dq2", 
                                                                "HLA-DQB1* allele 2", 
                                                                choices = character(0))),
                                          column(6,htmlOutput(outputId = "hed_dq"))
                                        ),
                                        br(),
                                        br(),
                                        HTML('<p style="text-align:right">Bruno A Lima, Oficina de Bioestatística, 2022 <i class="fa fa-creative-commons"></i></p>'),
                                        HTML('<a href="mailto:bioestatisticas@gmail.com">Just e-mail me!</a>'),
                                        br(),
                                        br(),
                                        h4("Disclaimer: "),
                                        h5("This application is intended for research purposes only, not for clinical or commercial use. It is a non-profit service to the scientific community, provided on an 'AS-IS' basis without any warranty, expressed or implied. The authors can not be held liable in any way for the service provided here.")
                              )
                     ),
                     tabPanel("Upload data", icon = icon("file"),
                              # select data to use
                              sidebarPanel(
                                a("Upload files:"),
                                wellPanel(
                                  radioButtons("dataInput", "", 
                                               list("Example data"=1, 
                                                    "Upload your files"=2), selected=1),
                                  conditionalPanel(condition="input.dataInput=='2'",
                                                   h6("Your data files must be in the exact format as the provided in the example data"),
                                                   fileInput("file", "upload HLA typing",
                                                             accept = c("text/csv",
                                                                        "text/comma-separated-values,text/plain",
                                                                        ".csv")),
                                                   radioButtons("fileSepDF", 
                                                                "Files' delimiter:", 
                                                                list("Comma"=1, "Tab"=2, "Semicolon"=3, "Space"=4),
                                                                selected=3)
                                                   
                                  )
                                )
                              ),
                              # show selected data 
                              mainPanel(
                                conditionalPanel(condition="input.dataInput=='1'",
                                                 h4("Example data:"),
                                                 dataTableOutput(outputId = "expl"),
                                                 ),
                                conditionalPanel(condition="input.dataInput=='2'",
                                                 h4("Uploaded data with HLA typing and respective HED results:"),
                                                 dataTableOutput(outputId = "results"),
                                                 br(),
                                                 downloadButton("downloadData", "Download")
                                                 )
                                )
                              
                     ),
                     tabPanel("Notes", icon = icon("bars"),
                              mainPanel(
                                h3('Technical Notes:'),
                                h6('Class I score is computed as decribed in: https://github.com/txopen/HLA-HED.'),
                                h6('Class II scores is computes as described in: https://sourceforge.net/projects/granthamdist/'),
                                h6('Grantham distance metric is described in: https://www.science.org/doi/10.1126/science.185.4154.862'),
                                br(),
                                #HTML('<iframe width="560" height="315" src="https://youtu.be/S9WhqrkKOJM" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                                HTML('<a href="https://youtu.be/S9WhqrkKOJM">Just wacth a presentation!</a>'),
                                br(),
                                h3('How to cite:'),
                                HTML('<a href="https://www.lidsen.com/journals/transplantation/transplantation-08-01-208">Bruno A Lima. Calculated Human Leukocyte Antigens Evolutionary Divergence (cHED). OBM Transplantation 2024; Vol 8, Issue 1</a>'),
                                br(),
                                h3('Changelog:'),
                                h4('cHED v0.0.1 (2022-10-01)'),
                                h6('- first relased'),
                                h4('cHED v0.0.2 (2023-04-01)'),
                                h6('- added Dowload option'),
                                h4('cHED v0.0.3 (2023-08-01)'),
                                h6('- columns on "Upload data" renamed'),
                                h4('cHED v0.0.4 (2024-05-23)'),
                                h6('- on uploaded data, silenced error when "allele does not exist in sequence"')
                                
                              )
                              
                     )
                     )
          )