ui <- bootstrapPage(
  mainPanel(
    titlePanel("PCA and Eigen faces by Lakshmi Sampath Pagolu | R11684446"),

        tabsetPanel(

          tabPanel("PCA Data input",
                   tags$hr(),
                   p("upload your file:"),


                   tags$hr(),

                   fileInput('file1', 'Choose a CSV file to upload:',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )),
                   p("After uploading your CSV file, click on the 'Inspect the data' tab")

                 ), # end file  tab

          tabPanel("Inspect the data",
                   p("Here is a summary of the data"),
                   tableOutput('summary'),
                   tags$hr(),
                   p("Here is the raw data from the CSV file"),
                   DT::dataTableOutput('contents')
          ),


          tabPanel("Compute PCA",

                   p("Choose the columns of your data to include in the PCA"),
                   uiOutput("choose_columns_pca"),
                   tags$hr()
          ),



          tabPanel("PCA output",
                   verbatimTextOutput("pca_details")
          ),
          tabPanel("Eigen Faces",

                    fileInput('file2', 'Choose a CSV file to upload:',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                    )),
                    h2("face of the personID"),
                    p("enter the ID of the person"),
                    textInput("personID", "personID"),
                    plotOutput("plot2", width='auto'),
                    tags$hr(),
                    h2("Eigen face of the person"),
                    textInput("personID", "personID"),
                    textOutput("cov"),
                    textOutput("eigen_va"),
                    textOutput("eigen_ve"),
                    plotOutput("z_plot_2", width="auto"),
                    textOutput("eigen_v")


          )
        )))
