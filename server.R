library(DT)
library(tidyverse)
library(openxlsx)
library(gtsummary)

library(histoc)

# alelos classe II
drs <- names(seqs2)[startsWith(names(seqs2),'DR')]
dqs <- names(seqs2)[startsWith(names(seqs2),'DQ')]

# silenciar erros da função cHED
p.cHED <- purrr::possibly(histoc::cHED, NA)

function(input, output, session) {
  
  updateSelectizeInput(session, "a1", 
                       choices = names(seqs)[startsWith(names(seqs),'A')], 
                       server = FALSE
                       #, options = list(maxOptions = 8000)
                       )
  updateSelectizeInput(session, "a2", 
                       choices = names(seqs)[startsWith(names(seqs),'A')], 
                       server = FALSE
                       #, options = list(maxOptions = 8000)
                       )
  
  updateSelectizeInput(session, "b1", 
                       choices = names(seqs)[startsWith(names(seqs),'B')], 
                       server = TRUE
                       #, options = list(maxOptions = 9000)
                       )
  updateSelectizeInput(session, "b2", 
                       choices = names(seqs)[startsWith(names(seqs),'B')], 
                       server = TRUE
                       #, options = list(maxOptions = 9000)
                       )
  
  updateSelectizeInput(session, "c1", 
                       choices = names(seqs)[startsWith(names(seqs),'C')], 
                       server = FALSE
                       #, options = list(maxOptions = 8000)
                       )
  updateSelectizeInput(session, "c2", 
                       choices = names(seqs)[startsWith(names(seqs),'C')], 
                       server = FALSE
                       #, options = list(maxOptions = 8000)
                       )
  
  updateSelectizeInput(session, "dr1", 
                       choices = paste0(substr(drs,1,4),'*',substr(drs,5,6),':',substr(drs,7,8)), 
                       server = TRUE
                       #, options = list(maxOptions = 8000)
                       )
  updateSelectizeInput(session, "dr2", 
                       choices = paste0(substr(drs,1,4),'*',substr(drs,5,6),':',substr(drs,7,8)), 
                       server = TRUE
                       #, options = list(maxOptions = 8000)
                       )
  
  updateSelectizeInput(session, "dq1", 
                       choices = paste0(substr(dqs,1,4),'*',substr(dqs,5,6),':',substr(dqs,7,8)), 
                       server = TRUE
                       #, options = list(maxOptions = 8000)
                       )
  updateSelectizeInput(session, "dq2", 
                       choices = paste0(substr(dqs,1,4),'*',substr(dqs,5,6),':',substr(dqs,7,8)), 
                       server = TRUE
                       #, options = list(maxOptions = 8000)
                       )
  
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
    
    A_1 <- sample(names(seqs)[startsWith(names(seqs),'A')], 100, replace = T)
    A_2 <- sample(names(seqs)[startsWith(names(seqs),'A')], 100, replace = T)
    B_1 <- sample(names(seqs)[startsWith(names(seqs),'B')], 100, replace = T)
    B_2 <- sample(names(seqs)[startsWith(names(seqs),'B')], 100, replace = T)
    C_1 <- sample(names(seqs)[startsWith(names(seqs),'C')], 100, replace = T)
    C_2 <- sample(names(seqs)[startsWith(names(seqs),'C')], 100, replace = T)
    
    DRB1_1 <- sample(names(seqs2)[startsWith(names(seqs2),'DR')], 100, replace = T)
    DRB1_2 <- sample(names(seqs2)[startsWith(names(seqs2),'DR')], 100, replace = T)
    DQB1_1 <- sample(names(seqs2)[startsWith(names(seqs2),'DQ')], 100, replace = T)
    DQB1_2 <- sample(names(seqs2)[startsWith(names(seqs2),'DQ')], 100, replace = T)
    
    ID <- paste0('ID',1:100)
    
    expl <- data.frame(ID, 
                       A_1, A_2, B_1, B_2, C_1, C_2,
                       DRB1_1, DRB1_2, DQB1_1, DQB1_2) %>% 
      dplyr::mutate(DRB1_1 = paste0(substr(DRB1_1,1,4),'*',substr(DRB1_1,5,6),':',substr(DRB1_1,7,8)),
                    DRB1_2 = paste0(substr(DRB1_2,1,4),'*',substr(DRB1_2,5,6),':',substr(DRB1_2,7,8)),
                    DQB1_1 = paste0(substr(DQB1_1,1,4),'*',substr(DQB1_1,5,6),':',substr(DQB1_1,7,8)),
                    DQB1_2 = paste0(substr(DQB1_2,1,4),'*',substr(DQB1_2,5,6),':',substr(DQB1_2,7,8)))
    
    expl <- expl %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(HED_A = histoc::cHED(A_1,A_2),
                   HED_B = histoc::cHED(B_1,B_2),
                   HED_C = histoc::cHED(C_1,C_2),
                   HED_DRB1 = histoc::cHED(DRB1_1,DRB1_2),
                   HED_DQB1 = histoc::cHED(DQB1_1,DQB1_2)) %>% 
      ungroup()
    
      
  })
  
  
  # read file
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
      need(identical(colnames(data),c("ID", "A_1","A_2","B_1","B_2","C_1","C_2",
                                      "DRB1_1","DRB1_2","DQB1_1","DQB1_2")),
           "Column names are not corrected!")
    )
    
    data %>% 
      dplyr::mutate_at(vars(A_1,A_2,B_1,B_2,C_1, C_2, DRB1_1,DRB1_2, DQB1_1, DQB1_2),as.character) %>% 
      dplyr::rowwise() %>%
      dplyr::mutate(HED_A = p.cHED(A_1,A_2),
                    HED_B = p.cHED(B_1,B_2),
                    HED_C = p.cHED(C_1,C_2),
                    HED_DRB1 = p.cHED(DRB1_1,DRB1_2),
                    HED_DQB1 = p.cHED(DQB1_1,DQB1_2)) %>%
      ungroup()
    
    })
  
  output$results<- renderDataTable({
    dataset() 
    })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("results", ".csv", sep = "")
    },
    content = function(file) {
      write.csv2(dataset(), file, row.names = FALSE, fileEncoding="latin1")
    }
  )
  
}
