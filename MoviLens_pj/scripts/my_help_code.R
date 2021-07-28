# Aside notes #
# ----------- #

# get to know traing set
train_set %>% summarise(n_users = n_distinct(userId), n_movies = n_distinct(movieId))


# clean workspace but edx and validation
rm(list = ls()[! ls() %in% c('edx', 'validation')])


# clean all workspace
rm(list = ls())



# Save objects to .RData file
save(test_set, train_set, file = "mamt.RData")


# load objects
load("./../data/edx&validation_sets.RData")

# get current working directory
getwd()
