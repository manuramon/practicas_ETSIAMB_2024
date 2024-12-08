# Sample genotype data for five individuals and three loci (genetic markers)
genotype_data <- matrix(
  c(
    c("A", "A"), c("A", "A"), c("A", "A"), c("A", "A"), c("A", "A"),
    c("A", "A"), c("A", "A"), c("A", "A"), c("A", "A"), c("A", "A"),
    c("A", "A"), c("A", "B"), c("A", "B"), c("A", "B"), c("A", "B"),
    c("A", "B"), c("A", "B"), c("A", "B"), c("A", "B"), c("A", "B"),
    c("A", "B"), c("A", "B"), c("A", "A"), c("A", "B"), c("A", "B")
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(
    c("Individual1", "Individual2", "Individual3", "Individual4", "Individual5"),
    c("Locus1", "Locus2", "Locus3", "Locus4","Locus5")
  )
)

# Function to calculate the kinship coefficient between two individuals
calculate_kinship <- function(individual1, individual2) {
  kinship <- 0
  for (locus in colnames(genotype_data)) {
    alleles_ind1 <- genotype_data[individual1, locus]
    alleles_ind2 <- genotype_data[individual2, locus]
    # Check if alleles match, and update kinship coefficient
    if (identical(alleles_ind1, alleles_ind2)) {
      kinship <- kinship + 1
    }
  }
  return(kinship / ncol(genotype_data))  # Normalize by the number of loci
}

# Calculate kinship coefficients between all pairs of individuals
kinship_matrix <- matrix(0, nrow = nrow(genotype_data), ncol = nrow(genotype_data))
rownames(kinship_matrix) <- colnames(kinship_matrix) <- rownames(genotype_data)

for (i in 1:nrow(genotype_data)) {
  for (j in 1:nrow(genotype_data)) {
    if (i == j) {
      kinship_matrix[i, j] <- 1  # Self-kinship
    } else {
      kinship_matrix[i, j] <- calculate_kinship(i, j)
    }
  }
}

# Print the kinship matrix
print(kinship_matrix)
