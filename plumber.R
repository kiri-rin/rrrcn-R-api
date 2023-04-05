library(plumber)
root <- pr("api/distance_api.R")
root
pr_run(port=8000,root)

