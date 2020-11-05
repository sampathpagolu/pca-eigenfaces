list.of.packages <- c("ggplot2",
                      "DT",
                      "GGally",
                      "psych",
                      "Hmisc",
                      "MASS",
                      "gridExtra",
                      "matrixStats",
                    "RSpectra"
                  )

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
options(shiny.maxRequestSize=30*1024^2)
# load all these
lapply(list.of.packages, require, character.only = TRUE)

server <- function(input, output) {

  # read in the CSV
  the_data_fn <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    the_data <-   read.csv(inFile$datapath)
    return(the_data)
  })
  # read in the CSV2
  the_data_fn2 <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2)) return(NULL)
    the_faces_data <-   read.csv(inFile2$datapath)
    the_faces_data<-the_faces_data[1:4096]
    return(the_faces_data)
  })


  # tableplot
  output$tableplot <- renderPlot({
    if(is.null(the_data_fn())) return()
    the_data <- the_data_fn()
    tableplot(the_data)

  })

  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })

  # display a summary of the CSV contents
  output$summary <-  renderTable({
    the_data <- the_data_fn()
    psych::describe(the_data)
  })

  output$choose_columns_pca <- renderUI({

      the_data <- the_data_fn()

      # we only want to show numeric cols
      the_data_num <- na.omit(the_data[,sapply(the_data,is.numeric)])



      colnames <- names(the_data_num)

      # Create the checkboxes and select them all by default
      checkboxGroupInput("columns", "Choose columns",
                         choices  = colnames,
                         selected = colnames)
    })
pca_objects <- reactive({
  # Keep the selected columns
  columns <-    input$columns
  the_data <- the_data_fn()
  the_data_subset <- the_data[, columns]
  pca <- function (x, ...) UseMethod("pca")

  pca<-
      function(x, retx = TRUE, center = TRUE, scale. = FALSE, tol = NULL, ...)
  {
      chkDots(...)
      x <- as.matrix(x)
      x <- scale(x, center = center, scale = scale.)
      cen <- attr(x, "scaled:center")
      sc <- attr(x, "scaled:scale")
      if(any(sc == 0))
          stop("cannot rescale a constant/zero column to unit variance")
      s <- svd(x, nu = 0)
      s$d <- s$d / sqrt(max(1, nrow(x) - 1))
      if (!is.null(tol)) {
          ## we get rank at least one even for a 0 matrix.
          rank <- sum(s$d > (s$d[1L]*tol))
          if (rank < ncol(x)) {
              s$v <- s$v[, 1L:rank, drop = FALSE]
              s$d <- s$d[1L:rank]
          }
      }
      dimnames(s$v) <-
          list(colnames(x), paste0("PC", seq_len(ncol(s$v))))
      r <- list(sdev = s$d, rotation = s$v,
                center = if(is.null(cen)) FALSE else cen,
                scale = if(is.null(sc)) FALSE else sc)
      if (retx) r$x <- x %*% s$v
      class(r) <- "pca"
      r
  }

  pca.formula <- function (formula, data = NULL, subset, na.action, ...)
  {
      mt <- terms(formula, data = data)
      if (attr(mt, "response") > 0L)
          stop("response not allowed in formula")
      cl <- match.call()
      mf <- match.call(expand.dots = FALSE)
      mf$... <- NULL
      ## need stats:: for non-standard evaluation
      mf[[1L]] <- quote(stats::model.frame)
      mf <- eval.parent(mf)
      ## this is not a `standard' model-fitting function,
      ## so no need to consider contrasts or levels
      if (.check_vars_numeric(mf))
          stop("PCA applies only to numerical variables")
      na.act <- attr(mf, "na.action")
      mt <- attr(mf, "terms")
      attr(mt, "intercept") <- 0L
      x <- model.matrix(mt, mf)
      res <- pca.default(x, ...)
      ## fix up call to refer to the generic, but leave arg name as `formula'
      cl[[1L]] <- as.name("pca")
      res$call <- cl
      if (!is.null(na.act)) {
          res$na.action <- na.act
          if (!is.null(sc <- res$x))
              res$x <- napredict(na.act, sc)
      }
      res
  }

  plot.pca <- function(x, main = deparse(substitute(x)), ...)
      screeplot.default(x, main = main, ...)

  print.pca <- function(x, print.x = FALSE, ...) {
      cat("Standard deviations:\n")
      print(x$sdev, ...)
      cat("\nRotation:\n")
      print(x$rotation, ...)
      if (print.x && length(x$x)) {
          cat("\nRotated variables:\n")
          print(x$x, ...)
      }
      invisible(x)
  }

  summary.pca <- function(object, ...)
  {
      chkDots(...)
      vars <- object$sdev^2
      vars <- vars/sum(vars)
      importance <- rbind("Standard deviation" = object$sdev,
                          "Proportion of Variance" = round(vars, 5),
                          "Cumulative Proportion" = round(cumsum(vars), 5))
      colnames(importance) <- colnames(object$rotation)
      object$importance <- importance
      class(object) <- "summary.pca"
      object
  }

  print.summary.pca <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...)
  {
      cat("Importance of components:\n")
      print(x$importance, digits = digits, ...)
      invisible(x)
  }

  predict.pca <- function(object, newdata, ...)
  {
      chkDots(...)
      if (missing(newdata)) {
          if(!is.null(object$x)) return(object$x)
          else stop("no scores are available: refit with 'retx=TRUE'")
      }
      if(length(dim(newdata)) != 2L)
          stop("'newdata' must be a matrix or data frame")
      nm <- rownames(object$rotation)
      if(!is.null(nm)) {
          if(!all(nm %in% colnames(newdata)))
              stop("'newdata' does not have named columns matching one or more of the original columns")
          newdata <- newdata[, nm, drop = FALSE]
      } else {
          if(NCOL(newdata) != NROW(object$rotation) )
              stop("'newdata' does not have the correct number of columns")
      }
      ## next line does as.matrix
      scale(newdata, object$center, object$scale) %*% object$rotation
  }

  .check_vars_numeric <- function(mf)
  {
      ## we need to test just the columns which are actually used.
      mt <- attr(mf, "terms")
      mterms <- attr(mt, "factors")
      mterms <- rownames(mterms)[apply(mterms, 1L, function(x) any(x > 0L))]
      any(sapply(mterms, function(x) is.factor(mf[,x]) || !is.numeric(mf[,x])))
  }
  pca_output <- pca(na.omit(the_data_subset))
  # pca_output<-pca(na.omit(the_data_subset))
  # data.frame of PCs
  pcs_df <- pca_output$x

  return(list(the_data = the_data,
       the_data_subset = the_data_subset,
       pca_output = pca_output,
       pcs_df = pcs_df))

})
output$plot2 <- renderPlot({
  AV<-the_data_fn2()
  id<-input$personID
  p<-AV[id, ]
  image(matrix(as.numeric(p),nrow=64,byrow=T), col=grey(seq(0,1, length=256)))

})
  read_data<- function(){
    AV<-the_data_fn2()
    p_id<-input$personID
    p<-AV[p_id, ]
    print(p_id)
    return(p)

  }
  eigen_face<-function(){
    # df<-the_data_fn2()
    # df<-data.matrix(df)
    # p_id<-input$personID
    #
    # correlation_matrix<-cor(data.matrix(df))
    # output$cov<-renderText({
    #   "Calculating correlation matrix"
    #   #correlation_matrix
    # })
    # eigens <- eigs(correlation_matrix, 45, which="LM")
    # eigenvalue<-eigens$values
    # eigenvectors<-eigens$vectors
    # output$eigen_va<-renderText({
    #   "Calculating eigen values"
    # #  eigenvectors
    # })
    # output$eigen_ve<-renderText({
    # "Calculating eigen face"
    # })
    # eigen_face<-eigenvectors%*%df[,p_id]
    # # eigen_face<-eigen_f%*%t(eigenvectors)
    # output$eigen_v<-renderText({
    #  "eigen face"
    #   eigen_face
    # })
    # return(eigen_face)
    df<-the_data_fn2()
    x<-df[1:4096]
    y<-df[4097]
    id<-seq(1,400, by=10)
    plt_img <- function(x){
      image(x, col=grey(seq(0, 1, length=256)))
    }
    #k=readline(prompt="input the subject id to see the average photo ")
    k<-as.integer(k)
    k<-id[k]
    AV=colMeans(data.matrix(x[k:k+9,]))
    # plt_img(matrix(AV,nrow=64,byrow=T))
    data<-data.matrix(x)
    # cat("Calculating correlation matrix")
    correlation_matrix<-cor(data)
    # cat("calculating eigen vectors and values")
    eigens <- eigs(correlation_matrix, 45, which="LM")
    eigenvalue<-eigens$values
    eigenvectors<-eigens$vectors
    cat("calculating distribution of first 45 vectors")
    dist<-(sum(eigenvalue)/sum(eigen(correlation_matrix)$values)*100)
    print(dist)
    # for (i in 1:6){
    #   plt_img(matrix(as.numeric(eigenvectors[, i]),nrow=64,byrow=T))}
    print("plotting the eigen face")
    img<-x[k, ]
    PF <- data.matrix(x[k,]) %*% eigenvectors
    R<- PF%*%t(eigenvectors)
    output$z_plot_2 <- renderPlot({
      # pca_df<-eigen_face()
    plt_img(matrix(unlist(R), nrow=64, byrow=T))
      # image(matrix(as.numeric(pca_df),nrow=64,byrow=T), col=grey(seq(0,1, length=256)))

    })

    PF <- data.matrix(x[k,]) %*% eigenvectors
    R<- PF%*%t(eigenvectors)
  }



  output$pca_details <- renderPrint({
    #
    print(pca_objects()$pca_output$rotation)
    print(summary(pca_objects()$pca_output))
    print(pca_objects()$pca_output$sdev)

  })



}
