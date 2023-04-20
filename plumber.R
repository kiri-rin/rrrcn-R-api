library(plumber)
root <- pr("api/distance_api.R")
root
pr_run(host="0.0.0.0",port=8000,root)


