#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(quantmod)
library(ggplot2)

#Komponen UI
ui = fluidPage(
  #Judul
  titlePanel(div(HTML("Diagram Pengendali Nonparametrik <em>Exponentially Weighted Moving Average Sign</em>"))),#Diedit
  theme = shinytheme('flatly'), #bisa pilih lihat di https://rstudio.github.io/shinythemes/
  #NavBar
  navbarPage("EWMA Sign", #Diedit
             #Panel: Home
             tabPanel("HOME",
                      div(
                        h3(HTML("<center>Penerapan Diagram Pengendali Nonparametrik <em>Exponentially Weighted Moving Average Sign</em> Untuk Analisis Pergerakan Harga Saham Sektor Properti</center>")#Edit , style="text-align:center"
                      )),
                      br(),
                      imageOutput("logo", height = "150px"),
                      br(),
                      h4("Disusun Oleh:",style="text-align:center"),
                      h4("Radian Lukman",style="text-align:center"),
                      h4("24050118140075",style="text-align:center"),
                      br(),
                      h4("Dosen Pembimbing:",style="text-align:center"),
                      h4("1. Prof. Mustafid, M.Eng., Ph.D",style="text-align:center"), #Edit Nama Dosbing
                      h4("2. Sugito, S.Si., M.Si",style="text-align:center"), #Edit Nama Dosbing
                      br(),br(),
                      div(
                        h3("Departemen Statistika",style="text-align:center"),
                        h3("Fakultas Sains dan Matematika",style="text-align:center"),
                        h3("Universitas Diponegoro",style="text-align:center"),
                        h3("2022",style="text-align:center")
                      )
             ),
             #Panel: Petunjuk Penggunaan
             tabPanel("PETUNJUK PENGGUNAAN",
                      h2("Petunjuk Penggunaan Aplikasi", style="text-align:justify"),
                      hr(),
                      div(
                      h4("1. Klik tab 'INPUT DATA', masukkan kode saham yang ingin digunakan (contoh: BSDE.JK).", 
                         style="text-align:justify"),
                      h4("2. Pilih rentang tanggal data harga penutupan saham yang diinginkan.", 
                         style="text-align:justify"),
                      h4("3. Klik 'RUN', maka akan terlihat statistik deskriptif dan data histori harga penutupan saham harian.", 
                         style="text-align:justify"),
                      h4("4. Klik tab 'DATA SAMPEL & PERIODE' dan masukkan jumlah sampel dan periode yang diinginkan, lalu klik 'RUN'.", 
                         style="text-align:justify"),
                      h4("5. Klik tab 'DIAGRAM PENGENDALI' dan masukkan nilai Lambda, k, dan target, lalu klik 'RUN'.", 
                         style="text-align:justify"),
                      h4(HTML("6. Data hasil transformasi EWMA <em>Sign</em> dan Diagram Pengendali EWMA <em>Sign</em> telah berhasil dibuat.", 
                         ),style="text-align:justify"),#Bisa Dicopas terus diurutkan cara penggunaan
                      )
             ),
             #Panel: Data
             tabPanel("INPUT DATA",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Saham"),
                          textInput("saham","Kode Saham:",value="bsde.jk"),
                          hr(),
                          h4("Periode Saham yang Diinginkan"),
                          dateInput("awal","Awal:", value = "2021-03-01"),
                          dateInput("akhir","Akhir:",value = "2022-03-04"),
                          hr(),
                          actionButton("jalankan", "RUN")
                        ),
                        mainPanel(
                          h4("Statistik Deskriptif"),
                          verbatimTextOutput("statdes"),
                          br(),
                          h4("Data Histori Harga Penutupan"),
                          DTOutput('datahis')
                          )
                        )
            ),
            #Panel Data Sampel & Periode
            tabPanel("DATA SAMPEL & PERIODE",
                     sidebarLayout(
                       sidebarPanel(
                         numericInput("sampel", "Jumlah Data Sampel :", 10, min = 1 ),
                         numericInput("periode", "Jumlah Periode :", 25, min = 1),
                         hr(),
                         actionButton("jalankan1", "RUN"),
                         downloadButton("d_datatrans", "Download Data")
                       ),
                       mainPanel(
                         h4("Data Sampel & Periode"),
                         DTOutput('datatrans')
                       )
                     )
            ),
            #Panel Diagram Pengendali
            tabPanel("DIAGRAM PENGENDALI",
                     sidebarLayout(
                       sidebarPanel(
                         h4("Parameter yang Digunakan :"),
                         numericInput("lambda", "Lambda :", 0.05, min = 0, max = 1),
                         numericInput("k", "k :", 2.70),
                         numericInput("target", "target :", 1050),
                         hr(),
                         actionButton("jalankan2", "RUN"),
                         downloadButton("Data Transformasi", "Download Data")
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Data Transformasi",
                                    DTOutput("datahasil")
                           ),
                           tabPanel("Diagram Pengendali",
                                    plotOutput("plothasil")
                                    )
                         )
                       )
                     )
            )
            
  )
)

#===============================================================================
#Fungsi
saham = function(code, from, to){
  price = getSymbols(code,auto.assign=FALSE,from=from,to=to)
  close.price = Cl(price)
  return(close.price)
}

stats_des = function(x){
  kolom = ncol(x)
  n = NULL
  rata = NULL
  minimum = NULL
  maks = NULL
  stddev = NULL
  for(i in 1:kolom){
    rata[i] = round(mean(x[,i]),3)
    minimum[i] = min(x[,i])
    maks[i] = max(x[,i])
    stddev[i] = round(sd(x[,i]),3)
    n[i] = length(x[,i])
  }
  baris = cbind(n, rata, stddev, minimum, maks)
  colnames(baris) = c("N","Mean", "Standar Deviasi", "Minimum", "Maksimum")
  rownames(baris) = names(x)
  print(baris)
}

manip = function(data=data, lambda, k, target){
  n = ncol(data)
  EWMAM0 = n/2
  n_row = nrow(data)
  data2 = data - target
  data2$M = rowSums(data2>0)
  data2$EWMAMi[1] = round((lambda*data2$M[1])+(1-lambda)*EWMAM0,3)
  if (n_row > 1) for (i in 2:n_row) data2$EWMAMi[i] = round(((lambda*data2$M[i])+(1-lambda)*data2$EWMAMi[i-1]),3)
  data2 = cbind(Periode = 1:n_row, data2)
  data2$UCL = round(((n/2) + k*sqrt((lambda/(2-lambda))*(n/4))),3) 
  data2$CL  = n/2
  data2$LCL = round(((n/2) - k*sqrt((lambda/(2-lambda))*(n/4))),3)
  return(as.data.frame(data2))
}

#===============================================================================
#Komponen Server
server <- function(input, output) {
  #Logo
  output$logo = renderImage({
    list(src="undip.png",
         width = '150px',
         height = '160px',
         style = "display : block; margin-left : auto; margin-right : auto")
  }, deleteFile = FALSE)
  
  #Data
  datainput = reactive({
    data = saham(code = input$saham, from = input$awal, to = input$akhir+1)
    data = as.data.frame(data)
  })
  observeEvent(input$jalankan,{
    output$datahis = renderDataTable({
      datainput() 
    })
    output$statdes = renderPrint({
      stats_des(datainput())
    })
  })
  #Data Sampel & Periode
  newdata = reactive({
    data = datainput()
    data1 = matrix(data[,1], nrow = input$periode, ncol = input$sampel, byrow = TRUE)
    newdata = as.data.frame(data1)
  })
  
  observeEvent(input$jalankan1,{
    output$datatrans = renderDataTable({
      newdata()
    })
  })
  
  output$d_datatrans = downloadHandler(
    filename = function(){
      paste("Data Sampel & Periode", ".csv", sep = "")
    },
    content = function(file){
      write.csv(newdata(), file)
    }
  )
  
  #Hasil
  newdata1 = reactive({
    manip(data = newdata(), lambda = input$lambda, k = input$k, target = input$target)
  })
  
  observeEvent(input$jalankan2,{
    output$datahasil = renderDataTable({
      newdata1()
    })
    
    output$plothasil = renderPlot({
      ggplot(newdata1(), aes(Periode, EWMAMi)) + geom_point(colour = "grey", size = 3)+ 
        geom_line(aes(Periode, EWMAMi), data = newdata1()) +
        geom_line(aes(Periode, CL, colour = "Center"), data = newdata1(), lwd = 1) +
        geom_line(aes(Periode, LCL, colour = "LCL"), data = newdata1(), lwd = 1) +
        geom_line(aes(Periode, UCL, colour = "UCL"), data = newdata1(), lwd = 1) +
        scale_colour_manual(values = c("blue", "red", "red")) +
        ggtitle(label = "Diagram Pengendali EWMA Sign",
                subtitle = paste("Lambda :", input$lambda, ", ", "k :", input$k, 
                                 ", dan target :", input$target, "\n" ))+
        xlab("Sample Ke-") + ylab("Deviasi")+ 
        labs(caption = paste("UCL =", newdata1()$UCL, "dan", "LCL =", newdata1()$LCL))+
        scale_x_continuous(breaks = seq(0,input$periode, 1))+
        theme(plot.title = element_text(color="black", size = 16, face="bold"),
              plot.subtitle = element_text(color="black", size = 14),
              plot.caption = element_text(color="black", size = 12),
              axis.title.x = element_text(color = "black", size = 12),
              axis.title.y = element_text(color = "black", size = 12),
              panel.grid = element_blank())
    })
  })
  
  output$hasil = downloadHandler(
    filename = function(){
      paste("Data Hasil", ".csv", sep = "")
    },
    content = function(file){
      write.csv(newdata1(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
