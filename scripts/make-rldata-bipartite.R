#!/usr/bin/env RScript --vanilla

if (!require(pacman)) install.packages("pacman")
pacman::p_load(RecordLinkage, assert, usethis)
set.seed(1)

# Make RLdata500_bipartite

dat = RLdata500
ids = identity.RLdata500
rows = 1:nrow(dat)
file = sapply(rows, function(i) {
  # If not duplicated, then assign file randomly.
  if (sum(ids==ids[i]) == 1) {
    return(sample(1:2, 1))
  }

  # Otherwise assign file based on natural order.
  return(1 + (rows[ids==ids[i]][1] == i))
})

RLdata500_bipartite = cbind(dat, file=file, id=ids)

# Make RLdata10000_bipartite

dat = RLdata10000
ids = identity.RLdata10000
rows = 1:nrow(dat)
file = sapply(rows, function(i) {
  # If not duplicated, then assign file randomly.
  if (sum(ids==ids[i]) == 1) {
    return(sample(1:2, 1))
  }

  # Otherwise assign file based on natural order.
  return(1 + (rows[ids==ids[i]][1] == i))
})

RLdata10000_bipartite = cbind(dat, file=file, id=ids)

# Save data.
data_path = file.path(usethis::proj_path(), "data-raw")
assert(dir.exists(data_path), msg=paste0("Directory\n\t'", data_path, "'\nnot found. "))
save(RLdata500_bipartite, RLdata10000_bipartite, file=file.path(data_path, "RLdata_bipartite.RData"))
