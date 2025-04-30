# 1. Put all files in a folder
# 2. Label all files so that they naturally fall in the order you want to merge them (1_, 2_, etc)
# 3. Change Working Directory, and Output Name in the code below

install.packages("pdftools")

library(pdftools)

setwd("D:/admin/budget/2022/cc/july2022")

x <- list.files()

#combine pdfs
pdf_combine(c(x), output="rankin_PCreconciliation_july2022.pdf")