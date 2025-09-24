Sys.setenv("R_TESTS" = "")
## Workaround for the error,
## "cannot open file 'startup.Rs': No such file or directory" in Windows 10

pkg <- c("lbaModel", "testthat", "rtdists")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nRunning testthat in the directory: ")
cat(getwd(), "\n")

home_dir <- "/media/yslin/Tui/01_Projects/lbaModel/tests/testthat"


cat("\n================= Group 0 tests =======================\n\n")
Group0 <- "Group0"
file0 <- file.path(home_dir, Group0, "0_test_fptpdf.r")
file1 <- file.path(home_dir, Group0, "1_test_n1pdf.r")
file2 <- file.path(home_dir, Group0, "2_rlba_norm.r")
file3 <- file.path(home_dir, Group0, "3_test_dlba.r")
test_file(file0)
test_file(file1)
test_file(file2)
test_file(file3)


cat("\n========================== Group 1 tests ==========================\n\n")
# Group1 <- "Group1"
# file0 <- file.path(home_dir, Group1, "0_plotting.r")
# test_file(file0)
