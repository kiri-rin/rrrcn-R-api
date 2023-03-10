library(plumber)
root <- pr("api/distance_api.R")
root
pr_run(root)

