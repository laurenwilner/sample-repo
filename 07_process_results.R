#-------------------------------------------------
# PSPS: Process analytic results 
# March 2025
#-------------------------------------------------

# setup -------------------------------------------------
if (!requireNamespace('pacman', quietly = TRUE)){install.packages('pacman')}
pacman::p_load(tidyverse, ggforce, MetBrewer)

results_dir <- ("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/results")

# read in and concat results -----------------------------
# pull directories of results and construct file names
files <- list()
results_dirs <- list.files(results_dir, full.names = TRUE)
for(dir in results_dirs){
  results_files <- list.files(dir, full.names = TRUE)
  for(file in results_files){
    # add to list of files 
    files <- c(files, file)
  }
}

# remove old results and exposure summaries 
files <- files[!grepl("Previous", files)]
files <- files[!grepl("Exposure", files)]

# read in and create a column for the file name
results <- data.frame()
for(f in files){
    print(f)
    df <- read_csv(f)

    # drop random row name cols
    df <- df %>% 
        select(c("Exposure", "OR", "CI_Lower", "CI_Upper", "p")) %>% 
        rename_all(tolower)

    # create names for the dfs
        # first remove everything before the slash
        f <- gsub(".*/", "", f)
        # remove the file extension
        f <- gsub(".csv", "", f)
        # remove results prefix 
        f <- gsub("results_", "", f)
    # pull out the cause group by taking the letters after the last underscore in f
        c <- gsub(".*_", "", f)

    df$model <- f
    df$cause <- c

    results <- rbind(results, df)
}

# make a plot of each model --------------------------------

# list of plots
plots <- list()

for(c in unique(results$cause)){
    p <- ggplot(data = results %>% filter(cause == c), aes(x = exposure, y = or, color = model)) + 
            geom_point() + 
            geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, color = model)) + 
            geom_hline(yintercept = 1, linetype = "dashed") + 
            ggtitle(c) +
            theme_minimal() + 
            theme(legend.position = "bottom") + 
            labs(x = "exposure", y = "odds Ratio", color = "model") + 
            facet_wrap(~model, scales = "free") + 
            theme(axis.text.x = element_text(angle = 90, hjust = .5),   
                legend.position = "none") +
            scale_color_met_d("Hokusai3") + 
            scale_y_continuous()
    plots[[length(plots) + 1]] <- p
}

# save the plot
pdf("~/Desktop/Desktop/epidemiology_PhD/00_repos/psps_ca_analysis/results/psps_results_mar2025.pdf", height = 13, width = 10)
for(p in plots){
    print(p)
}
dev.off()
