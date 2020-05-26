# store caches in an environment, not as global variables assigned with <<-
# https://stackoverflow.com/questions/41954302/where-to-create-package-environment-variables
assign(x = "sos4R_caches", value = new.env())
utils::globalVariables("sos4R_caches")