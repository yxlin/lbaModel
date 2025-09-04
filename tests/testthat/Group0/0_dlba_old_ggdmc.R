# q(save = "no")
cat("\n\n-------------------- Testing dlba --------------------")
rm(list = ls())
pkg <- c("lbaModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

param_list2mat <- function(param_list) {
    n_row <- length(param_list[[1]])
    n_col <- length(param_list)

    tmp <- matrix(NA, nrow = n_row, ncol = n_col)

    for (i in seq_len(n_col)) {
        tmp[, i] <- param_list[[i]]
    }
    t(tmp)
}


# old_model <- gdmc:::BuildModel(
#     p.map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
#     match.map = list(M = list("s1" = "r1", "s2" = "r2")),
#     factors = list(S = c("s1", "s2")),
#     constants = c(sd_v = 1, st0 = 0),
#     responses = c("r1", "r2"),
#     type = "norm"
# )

# old_p_vector <- c(
#     A = 0.15, B = 0.45, t0 = 0.2,
#     mean_v.true = 2.25,
#     mean_v.false = 0.15
# )

# old_dat <- gdmc:::simulate_one(old_model, n = 360, ps = old_p_vector, seed = 123)
# old_dat$C <- ifelse(
#     (old_dat$S == "s1" & old_dat$R == "r1") | (old_dat$S == "s2" & old_dat$R == "r2"),
#     "O",
#     "X"
# )
# propR <- table(old_dat$R) / nrow(old_dat)
# propC <- table(old_dat$C) / nrow(old_dat)
# print(round(c(propR, propC), 3))

params_tmp <- list(
    A = c(0.74, 0.74),
    b = c(1.25 + 0.74, 1.25 + 0.74),
    mean_v = c(2.52, 1.50),
    sd_v = c(1.0, 1.0),
    st0 = c(0.0, 0.0),
    t0 = c(0.04, 0.04)
)

params <- param_list2mat(params_tmp)
params
n_acc <- ncol(params)
is_positive_drift <- rep(TRUE, n_acc)

# lbaModel::dlba()
# res <- lbaModel::n1PDF(old_dat$RT, params, is_positive_drift, TRUE)
# cat("Print first 30 density values from the LBA node 1 function: \n")
# print(head(round(res, 2), 30))
