#!/usr/bin/env Rscript

#' Setup Workshop Environment with renv
#'
#' This script sets up the R environment for the workshop using renv.
#' It reads package requirements from docker/setting_files/packages.json
#' and installs the specified versions.

# Check if renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing renv...\n")
    install.packages("renv")
}

library(renv)

# Check if jsonlite is installed (needed to read JSON)
if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Installing jsonlite...\n")
    install.packages("jsonlite")
}

library(jsonlite)

# Initialize renv project if not already initialized
if (!file.exists("renv.lock")) {
    cat("Initializing renv project...\n")
    renv::init(bare = TRUE)
} else {
    cat("renv project already initialized.\n")
}

# Read packages from JSON file
cat("Reading package requirements from docker/setting_files/packages.json...\n")
json_file <- "docker/setting_files/packages.json"

if (!file.exists(json_file)) {
    stop("Error: ", json_file, " not found!")
}

packages_data <- jsonlite::fromJSON(json_file)
packages_list <- packages_data$packages

cat("Found", nrow(packages_list), "packages to install.\n\n")

# Install packages with specified versions
cat("Installing packages...\n")
for (i in 1:nrow(packages_list)) {
    pkg <- packages_list$package[i]
    ver <- packages_list$version[i]

    cat(sprintf("  [%d/%d] Installing %s version %s...\n", i, nrow(packages_list), pkg, ver))

    # Construct package string with version
    pkg_string <- paste0(pkg, "@", ver)

    tryCatch(
        {
            renv::install(pkg_string)
            cat(sprintf("    ✓ Successfully installed %s\n", pkg))
        },
        error = function(e) {
            cat(sprintf("    ✗ Error installing %s: %s\n", pkg, e$message))
        }
    )
}

# Create renv snapshot
cat("\nCreating renv snapshot...\n")
renv::snapshot()

cat("\n✅ Environment setup complete!\n")
cat("\nTo activate this environment in the future, run:\n")
cat("  renv::restore()\n")
