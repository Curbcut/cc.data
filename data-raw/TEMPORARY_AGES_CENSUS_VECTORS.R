# Function to always subset the vectors which match the parent
get_rows_from_parent <- function(vecs, parent_vec) {
  df <- vecs[vecs$parent_vector == parent_vec, ]
  df[!is.na(df$vector), ]
}

# Ages 2021 ---------------------------------------------------------------

cancensus::list_census_vectors("CA21")->vecs

vec_total <- vecs[grepl("Age", vecs$label), ]$vector[1]

# Using the vectors of Total - Age, get its children
total_age <- get_rows_from_parent(vecs, vec_total)

# Go down on all the categories
big_cat <- total_age$vector

all_ages <- lapply(big_cat, \(bcat) {

  # For every bigger category, go even deeper
  mid_cat <- get_rows_from_parent(vecs, bcat)
  out <- lapply(mid_cat$vector, \(mcat) {
    get_rows_from_parent(vecs, mcat)
  })

  # In some cases, we need to go even one level deeper
  out <- lapply(out, \(t) {
    if (sum(grepl(" to ", t$label)) > 0) {
      w <- which(grepl(" to ", t$label))
      for (i in w) {
        outt <- lapply(t$vector[w], \(lcat) {
          get_rows_from_parent(vecs, lcat)
        })
      }
      Reduce(rbind, c(outt, list(t[-w,])))
    } else {
      t
    }
  })

  Reduce(rbind, out)

})

all_ages_tb_2021 <- Reduce(rbind, all_ages)

all_ages_tb_2021


# Ages 2016 ---------------------------------------------------------------

cancensus::list_census_vectors("CA16")->vecs

vec_total <- vecs[grepl("Age", vecs$label), ]$vector[1]

# Using the vectors of Total - Age, get its children
total_age <- get_rows_from_parent(vecs, vec_total)

# Go down on all the categories
big_cat <- total_age$vector

all_ages <- lapply(big_cat, \(bcat) {

  # For every bigger category, go even deeper
  mid_cat <- get_rows_from_parent(vecs, bcat)
  out <- lapply(mid_cat$vector, \(mcat) {
    get_rows_from_parent(vecs, mcat)
  })

  # In some cases, we need to go even one level deeper
  out <- lapply(out, \(t) {
    if (sum(grepl(" to ", t$label)) > 0) {
      w <- which(grepl(" to ", t$label))
      for (i in w) {
        outt <- lapply(t$vector[w], \(lcat) {
          get_rows_from_parent(vecs, lcat)
        })
      }
      Reduce(rbind, c(outt, list(t[-w,])))
    } else {
      t
    }
  })

  Reduce(rbind, out)

})

all_ages_tb_2016 <- Reduce(rbind, all_ages)

all_ages_tb_2016


# Ages 2011 ---------------------------------------------------------------

vecs <- cancensus::list_census_vectors("CA11")

# Using the vectors of Total - Age, get its children
total_age_2011 <- get_rows_from_parent(vecs, "v_CA11F_5")

total_age_2011



# Ages 2006 ---------------------------------------------------------------

vecs <- cancensus::list_census_vectors("CA06")

# Using the vectors of Total - Age, get its children
total_age <- get_rows_from_parent(vecs, "v_CA06_2")

total_age_2006 <- lapply(total_age$vector, \(ta) {
  get_rows_from_parent(vecs, ta)
})

# MALE AND FEMALE
total_age_2006



# Ages 2001 ---------------------------------------------------------------

vecs <- cancensus::list_census_vectors("CA01")

# Using the vectors of Total - Age, get its children
total_age <- get_rows_from_parent(vecs, "v_CA01_5")

total_age_2001 <- lapply(total_age$vector, \(ta) {
  get_rows_from_parent(vecs, ta)
})

# MALE AND FEMALE
total_age_2001




# Ages 1996 ---------------------------------------------------------------


vecs <- cancensus::list_census_vectors("CA1996")

# Using the vectors of Total - Age, get its children
total_age <- get_rows_from_parent(vecs, "v_CA1996_5")

total_age_1996 <- lapply(total_age$vector, \(ta) {
  get_rows_from_parent(vecs, ta)
})

# MALE AND FEMALE
total_age_1996



# All dfs -----------------------------------------------------------------


all_ages_tb_2021
all_ages_tb_2016
total_age_2011
total_age_2006
total_age_2001
total_age_1996
