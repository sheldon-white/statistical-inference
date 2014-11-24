library(knitr)
library(markdown)
options(markdown.HTML.options = "")
options(rstudio.markdownToHTML = 
  function(inputFile, outputFile) {      
    require(markdown)
    markdownToHTML(inputFile, outputFile, stylesheet='custom.css')   
  }
) 

knit2html("RepData_PeerAssessment2.Rmd", "RepData_PeerAssessment2.html", options="")