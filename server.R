library(DT)
library(tidyverse)
library(openxlsx)
library(gtsummary)

library(histoc)

# alelos classe II
drs <- names(seqs2)[startsWith(names(seqs2),'DR')]
dqs <- names(seqs2)[startsWith(names(seqs2),'DQ')]

function(input, output, session) {
  
  updateSelectizeInput(session, "a1", 
                       choices = names(seqs)[startsWith(names(seqs),'A')], 
                       server = TRUE)
  updateSelectizeInput(session, "a2", 
                       choices = names(seqs)[startsWith(names(seqs),'A')], 
                       server = TRUE)
  
  updateSelectizeInput(session, "b1", 
                       choices = names(seqs)[startsWith(names(seqs),'B')], 
                       server = TRUE)
  updateSelectizeInput(session, "b2", 
                       choices = names(seqs)[startsWith(names(seqs),'B')], 
                       server = TRUE)
  
  updateSelectizeInput(session, "c1", 
                       choices = names(seqs)[startsWith(names(seqs),'C')], 
                       server = TRUE)
  updateSelectizeInput(session, "c2", 
                       choices = names(seqs)[startsWith(names(seqs),'C')], 
                       server = TRUE)
  
  updateSelectizeInput(session, "dr1", 
                       choices = paste0(substr(drs,1,4),'*',substr(drs,5,6),':',substr(drs,7,8)), 
                       server = TRUE)
  updateSelectizeInput(session, "dr2", 
                       choices = paste0(substr(drs,1,4),'*',substr(drs,5,6),':',substr(drs,7,8)), 
                       server = TRUE)
  
  updateSelectizeInput(session, "dq1", 
                       choices = paste0(substr(dqs,1,4),'*',substr(dqs,5,6),':',substr(dqs,7,8)), 
                       server = TRUE)
  updateSelectizeInput(session, "dq2", 
                       choices = paste0(substr(dqs,1,4),'*',substr(dqs,5,6),':',substr(dqs,7,8)), 
                       server = TRUE)
  
  ## compute DRI for one donor
  heda<-reactive({
    histoc::cHED(input$a1, input$a2)
    })
  output$hed_a<-renderText({
    paste("<b>HED A* <b>=", heda())
  })
  
  hedb<-reactive({
    histoc::cHED(input$b1, input$b2)
  })
  output$hed_b<-renderText({
    paste("<b>HED B* <b>=", hedb())
  })
  
  hedc<-reactive({
    histoc::cHED(input$c1, input$c2)
  })
  output$hed_c<-renderText({
    paste("<b>HED C* <b>=", hedc())
  })
  
  heddr<-reactive({
    histoc::cHED(input$dr1, input$dr2)
  })
  output$hed_dr<-renderText({
    paste("<b>HED DRB1* <b>=", heddr())
    
  })
  
  heddq<-reactive({
    histoc::cHED(input$dq1, input$dq2)
  })
  output$hed_dq<-renderText({
    paste("<b>HED DQB1* <b>=", heddq())
  })
  
  output$expl<- renderDataTable({
    
    set.seed(1)
    
    A1 <- sample(names(seqs)[startsWith(names(seqs),'A')], 100, replace = T)
    A2 <- sample(names(seqs)[startsWith(names(seqs),'A')], 100, replace = T)
    B1 <- sample(names(seqs)[startsWith(names(seqs),'B')], 100, replace = T)
    B2 <- sample(names(seqs)[startsWith(names(seqs),'B')], 100, replace = T)
    C1 <- sample(names(seqs)[startsWith(names(seqs),'C')], 100, replace = T)
    C2 <- sample(names(seqs)[startsWith(names(seqs),'C')], 100, replace = T)
    
    DR1 <- sample(names(seqs2)[startsWith(names(seqs2),'DR')], 100, replace = T)
    DR2 <- sample(names(seqs2)[startsWith(names(seqs2),'DR')], 100, replace = T)
    DQ1 <- sample(names(seqs2)[startsWith(names(seqs2),'DQ')], 100, replace = T)
    DQ2 <- sample(names(seqs2)[startsWith(names(seqs2),'DQ')], 100, replace = T)
    
    expl <- data.frame(A1, A2, B1, B2, C1, C2,
                       DR1, DR2, DQ1, DQ2) %>% 
      dplyr::mutate(DR1 = paste0(substr(DR1,1,4),'*',substr(DR1,5,6),':',substr(DR1,7,8)),
                    DR2 = paste0(substr(DR2,1,4),'*',substr(DR2,5,6),':',substr(DR2,7,8)),
                    DQ1 = paste0(substr(DQ1,1,4),'*',substr(DQ1,5,6),':',substr(DQ1,7,8)),
                    DQ2 = paste0(substr(DQ2,1,4),'*',substr(DQ2,5,6),':',substr(DQ2,7,8)))
    
    p.cHED <- purrr::possibly(histoc::cHED, NA)
    
    expl <- expl %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(HED_A = histoc::cHED(A1,A2),
                   HED_B = histoc::cHED(B1,B2),
                   HED_C = histoc::cHED(C1,C2),
                   HED_DR = histoc::cHED(DR1,DR2),
                   HED_DQ = histoc::cHED(DQ1,DQ2)) %>% 
      ungroup()
    
      
  })
  
  
  # read candidates' file
  dataset <- reactive({
    
    file <- input$file
    
    if (is.null(file))
      return(NULL)
    
    if (input$fileSepDF == 1) {
      data<-read.csv(file$datapath)
    } else if (input$fileSepDF == 2) {
      read.delim(file$datapath)
    } else if (input$fileSepDF == 3) {
      data<-read.csv2(file$datapath)
    } else {data<-read.table(file$datapath, encoding = "UTF-8")}
    
    
    validate(
      need(identical(colnames(data),c("A1","A2","B1","B2","C1","C2",
                                      "DR1","DR2","DQ1","DQ2")),
           "Column names are not corrected!")
    )
    
    data %>% 
      dplyr::mutate_at(vars(A1,A2,B1,B2,C1, C2, DR1,DR2, DQ1, DQ2),as.character) 
    
    })
  
  output$results<- renderDataTable({
    dataset() %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(HED_A = histoc::cHED(A1,A2),
                    HED_B = histoc::cHED(B1,B2),
                    HED_C = histoc::cHED(C1,C2),
                    HED_DR = histoc::cHED(DR1,DR2),
                    HED_DQ = histoc::cHED(DQ1,DQ2)) %>% 
      ungroup()
    
    })
  
}
