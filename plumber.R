library(plumber)
root <- pr("api/api.R")
pr_run(host="0.0.0.0",port=8000,root)


