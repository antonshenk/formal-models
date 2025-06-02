# Load required libraries
library(deSolve)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(viridisLite)
library(lhs)  
library(plotly)
library(cluster)
library(factoextra)
library(GGally) 
library(metR)
library(ggtext)
library(viridis)
library(dplyr)
library(ggrepel)
library(gridExtra) 
library(grid)
library(scales)
library(tidyr)
library(fmsb)
library(patchwork)
library(ggh4x)
library(gt)
library(gtExtras)

# Define the system of equations for AGI governance dynamics
agi_governance_system <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # Unpack state variables
    C_us <- state[1]  # US capability level
    C_ch <- state[2]  # China capability level
    S_us <- state[3]  # US safety investment
    S_ch <- state[4]  # China safety investment
    V <- state[5]     # Verification capability (global)
    G <- state[6]     # Governance strength (global)
    R <- state[7]     # Global risk level
    
    # Calculate strategic intervention probabilities
    I_us_ch <- max(0, intervention_intensity * (C_ch - C_us - intervention_threshold) * (1 - V))  # US intervention on China
    I_ch_us <- max(0, intervention_intensity * (C_us - C_ch - intervention_threshold) * (1 - V))  # China intervention on US
    
    # Modified capability development equations with safety penalty parameter
    dC_us <- research_efficiency_us * (1 - safety_penalty * S_us) * C_us * (1 - C_us/max_capability) - 
      I_ch_us * (C_us - min(C_us, C_ch))
    dC_ch <- research_efficiency_ch * (1 - safety_penalty * S_ch) * C_ch * (1 - C_ch/max_capability) - 
      I_us_ch * (C_ch - min(C_us, C_ch))
    
    # Safety investment equations
    dS_us <- risk_response * (R - S_us) + governance_response * (G - S_us)
    dS_ch <- risk_response * (R - S_ch) + governance_response * (G - S_ch)
    
    # Verification capability dynamics
    dV <- verification_improvement * G - verification_decay * V
    
    # Governance strength dynamics
    dG <- governance_adjustment * ((S_us + S_ch)/2 - G)
    
    # Global risk equation - grows with capabilities, reduced by safety
    weighted_capabilities <- (C_us^2 + C_ch^2)
    weighted_safety <- (S_us * C_us + S_ch * C_ch + 0.1)  # Add small constant to avoid division by zero
    dR <- risk_factor * (weighted_capabilities / weighted_safety - R)
    
    return(list(c(dC_us, dC_ch, dS_us, dS_ch, dV, dG, dR)))
  })
}

# Set up parameters
params <- c(
  research_efficiency_us = 0.2,
  research_efficiency_ch = 0.2,
  risk_response = 0.1,
  governance_response = 0.1,
  verification_improvement = 0.2,
  verification_decay = 0.1,
  governance_adjustment = 0.1,
  risk_factor = 0.2,
  intervention_intensity = 0.3,   
  intervention_threshold = 0.2,
  max_capability = 1.0,          
  safety_penalty = 0.5           # New parameter: 0.5 means safety investment has half the impact on slowing capability
)

# Initial conditions
initial_state <- c(
  C_us = 0.3,    # US initial capability
  C_ch = 0.25,   # China initial capability
  S_us = 0.4,    # US initial safety investment
  S_ch = 0.3,    # China initial safety investment
  V = 0.2,       # Initial verification capability
  G = 0.3,       # Initial governance strength
  R = 0.1        # Initial global risk
)

# Time settings
times <- seq(0, 100, by = 0.1)

# Solve the system
solution <- ode(
  y = initial_state,
  times = times,
  func = agi_governance_system,
  parms = params,
  method = "lsoda"
)

# Convert to data frame for plotting
solution_df <- as.data.frame(solution)
names(solution_df) <- c("time", "US_Capability", "China_Capability", "US_Safety", 
                        "China_Safety", "Verification", "Governance", "Global_Risk")

##########################################################
# 1. HIGH-DIMENSIONAL RISK LANDSCAPE VISUALIZATION
##########################################################

# First, let's modify the run_global_sensitivity function to store equilibrium states
run_global_sensitivity_with_equilibria <- function(param_ranges, n_samples = 1000, t_eval = 1000) {
  # Create Latin Hypercube Sample of parameter space
  param_combinations <- randomLHS(n_samples, length(param_ranges))
  
  # Scale to actual parameter ranges
  for (i in 1:length(param_ranges)) {
    param_combinations[,i] <- qunif(param_combinations[,i], 
                                    min = param_ranges[[i]][1], 
                                    max = param_ranges[[i]][2])
  }
  
  # Create results dataframe
  results <- data.frame(param_combinations)
  names(results) <- names(param_ranges)
  
  # Add columns for outcomes
  results$peak_risk <- NA
  results$final_risk <- NA
  results$stability_metric <- NA
  
  # Create storage for equilibrium states
  equilibria <- matrix(NA, nrow = n_samples, ncol = 7) # 7 state variables
  colnames(equilibria) <- c("C_us", "C_ch", "S_us", "S_ch", "V", "G", "R")
  
  # Add columns for stability check
  results$is_stable <- NA
  
  for (i in 1:nrow(results)) {
    # Set up parameters from this row
    sim_params <- params
    for (j in 1:length(param_ranges)) {
      sim_params[names(param_ranges)[j]] <- results[i, j]
    }
    
    # Run simulation to equilibrium
    eq_times <- seq(0, t_eval, by = 10)
    sim_result <- ode(
      y = initial_state, 
      times = eq_times, 
      func = agi_governance_system, 
      parms = sim_params
    )
    
    # Store the final equilibrium state
    equilibria[i,] <- sim_result[nrow(sim_result), 2:8]
    
    # Extract risk metrics
    results$peak_risk[i] <- max(sim_result[,8])
    results$final_risk[i] <- sim_result[nrow(sim_result),8]
    results$stability_metric[i] <- sd(diff(sim_result[,8]))
    
    # Check stability by perturbation
    perturbed_state <- initial_state * (1 + rnorm(7, mean = 0, sd = 0.01))
    perturbed_sim <- ode(
      y = perturbed_state, 
      times = eq_times, 
      func = agi_governance_system, 
      parms = sim_params
    )
    
    perturbed_eq <- perturbed_sim[nrow(perturbed_sim), 2:8]
    distance <- sqrt(sum((equilibria[i,] - perturbed_eq)^2))
    results$is_stable[i] <- ifelse(distance < 0.1, 1, 0)
  }
  
  # Add equilibrium states to results
  results <- cbind(results, equilibria)
  
  return(results)
}

# Run more comprehensive analysis with equilibria
param_ranges <- list(
  research_efficiency_us = c(0.1, 0.4),
  research_efficiency_ch = c(0.1, 0.4),
  risk_response = c(0.05, 0.3),
  governance_response = c(0.05, 0.3),
  verification_improvement = c(0.1, 0.5),
  verification_decay = c(0.05, 0.2),
  governance_adjustment = c(0.05, 0.3),
  risk_factor = c(0.1, 0.4),
  intervention_intensity = c(0.1, 0.9),
  intervention_threshold = c(0.1, 0.5),
  max_capability = c(1.0, 10.0),
  safety_penalty = c(0.1, 1.0)
)

# Run simulations (this will take time)
full_results <- run_global_sensitivity_with_equilibria(param_ranges, n_samples = 1000)

# Save results to avoid recomputing
write.csv(full_results, "agi_governance_full_results.csv", row.names = FALSE)

##########################################################
# 2. DATA ANALYSIS & PARAMETER IMPORTANCE
##########################################################

# Calculate correlation between parameters and outcomes
cor_matrix <- cor(full_results[, c(names(param_ranges), "final_risk", "peak_risk")], method = "spearman")
param_risk_correlations <- cor_matrix["final_risk", names(param_ranges)]

# Create parameter importance dataframe
param_importance <- data.frame(
  parameter = names(param_risk_correlations),
  correlation = abs(param_risk_correlations)
)
param_importance <- param_importance[order(-param_importance$correlation),]

# Plot parameter importance
# First, create more readable parameter labels
param_labels <- c(
  "safety_penalty" = "Safety-Capability Tradeoff",
  "max_capability" = "Maximum AGI Capability Level",
  "verification_decay" = "Verification System Degradation",
  "governance_adjustment" = "Governance Adaptation Rate",
  "risk_response" = "Response to Risk Signals",
  "research_efficiency_ch" = "China Research Efficiency",
  "verification_improvement" = "Verification Technology Improvement",
  "governance_response" = "Governance Influence",
  "intervention_threshold" = "Intervention Threshold",
  "intervention_intensity" = "Strategic Intervention Intensity",
  "research_efficiency_us" = "US Research Efficiency",
  "risk_factor" = "Risk Accumulation Rate"
)

# Create data frame with readable labels
plot_data <- param_importance %>%
  mutate(readable_name = param_labels[parameter])

# Set RAND-appropriate color palette
rand_blue <- "#1E3D6B"
rand_light_blue <- "#5B85AA"
rand_gray <- "#7C878E"

# Create professional plot
ggplot(plot_data, aes(x = reorder(readable_name, correlation), y = correlation)) +
  geom_bar(stat = "identity", fill = rand_blue, width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", correlation)), 
            hjust = -0.2, size = 3.5, color = rand_gray) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(plot_data$correlation) * 1.15),
                     breaks = seq(0, 0.8, by = 0.2),
                     expand = c(0, 0)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = rand_blue),
    plot.subtitle = element_text(size = 11, color = rand_gray),
    plot.caption = element_text(size = 9, color = rand_gray, hjust = 0),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 30, 20, 10),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Key Drivers of AGI Governance Risk",
    subtitle = "Absolute Spearman correlation with global risk at equilibrium",
    caption = "Note: Based on 1,000 simulations with Latin Hypercube parameter sampling.\nRAND Corporation AGI Governance Model - DRAFT",
    x = NULL,
    y = "Impact on Global Risk Level"
  )

##########################################################
# 3. HIGH-DIMENSIONAL VISUALIZATIONS
##########################################################

# A. 3D Surface Plot for top 3 parameters
# Get top 3 most important parameters
top3_params <- param_importance$parameter[1:3]
param1 <- top3_params[1]
param2 <- top3_params[2]
param3 <- top3_params[3]

# Create a smooth interpolated surface
grid_resolution <- 30
x_vals <- seq(min(full_results[[param1]]), max(full_results[[param1]]), length.out = grid_resolution)
y_vals <- seq(min(full_results[[param2]]), max(full_results[[param2]]), length.out = grid_resolution)

# Create a grid of points for interpolation
grid_points <- expand.grid(
  x = x_vals,
  y = y_vals
)

# Prepare the z_matrix to store interpolated risk values
z_matrix <- matrix(NA, nrow = length(x_vals), ncol = length(y_vals))

# Fill the matrix with interpolated risk values
for(i in 1:length(x_vals)) {
  for(j in 1:length(y_vals)) {
    # Find nearby points for interpolation
    nearby_points <- full_results %>%
      mutate(
        dist = sqrt((get(param1) - x_vals[i])^2 + (get(param2) - y_vals[j])^2)
      ) %>%
      arrange(dist) %>%
      head(20)  # Use 20 closest points
    
    # Calculate weighted average based on distance
    weights <- 1/(nearby_points$dist + 0.001)  # Add small constant to avoid division by zero
    z_matrix[i,j] <- weighted.mean(nearby_points$final_risk, weights)
  }
}

# Convert the 3D surface data to a format suitable for 2D visualization
surface_df <- expand.grid(
  safety_penalty = x_vals,
  max_capability = y_vals
)
surface_df$risk <- as.vector(z_matrix)

# Set RAND color palette
rand_blue <- "#003F6A"
rand_red <- "#B22222" 
rand_gray <- "#555555"
rand_light_blue <- "#5B85AA"

# Create a professional static visualization
ggplot(surface_df, aes(x = safety_penalty, y = max_capability, z = risk)) +
  # Add filled contours for the risk surface
  geom_contour_filled(aes(fill = after_stat(level)), bins = 4) +
  # Add contour lines with labels
  geom_contour(color = "white", alpha = 0.8, linewidth = 0.3) +
  geom_contour(breaks = c(1.5, 2.0, 2.5), color = "white", linewidth = 0.6) +
  metR::geom_text_contour(breaks = c(1.5, 2.0, 2.5), stroke = 0.2, size = 3) +
  # Use a professional RAND-style color scale
  scale_fill_manual(
    values = c("#FABC3F", "#E85C0D", "#C7253E", "#821131"),  # Blue to red progression
    name = "Global Risk",
    labels = c("Low", "Medium-Low", "Medium-High", "High"),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      keywidth = unit(1.2, "lines"),
      keyheight = unit(0.8, "lines")
    )
  ) +
  # Fine-tune the appearance
  labs(
    title = "AGI Risk Landscape: Safety-Capability vs. Maximum Capability",
    x = "Safety-Capability Tradeoff\n(Higher values = stronger tradeoff)",
    y = "Maximum AGI Capability Level\n(Higher values = more powerful systems)",
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations with Latin Hypercube parameter sampling | DRAFT"
  ) +
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#003F6A", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "#555555"),
    plot.caption = element_text(size = 8, color = "#555555", hjust = 0.5, margin = margin(t = 10)),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE"),
    # Legend settings
    legend.position = c(0.95, 0.05),
    legend.justification = c(1, 0),
    legend.direction = "horizontal",
    legend.key.size = unit(0.8, "lines"),
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 8),
    legend.margin = margin(3, 3, 3, 3),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.background = element_rect(fill = "white", color = "#BBBBBB"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
    panel.background = element_rect(fill = "#FCFCFC", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# B. Parallel coordinates plot for high-dimensional visualization
# Use the top 6 most important parameters
top6_params <- param_importance$parameter[1:6]

# Create normalized version for better visualization
normalized_results <- full_results %>%
  select(all_of(top6_params), final_risk, is_stable) %>%
  mutate(across(all_of(top6_params), function(x) (x - min(x)) / (max(x) - min(x)))) %>%
  # Add risk categories for coloring
  mutate(risk_category = cut(final_risk, breaks = 4, labels = c("Low", "Medium", "High", "Very High")))

# Define RAND color palette
rand_colors <- c(
  "Low" = "#4575B4",
  "Medium" = "#91BFDB", 
  "High" = "#FC8D59", 
  "Very High" = "#D73027"
)

# Create readable parameter labels
param_labels <- c(
  "safety_penalty" = "Safety-Capability\nTradeoff",
  "max_capability" = "Maximum AGI\nCapability Level",
  "risk_response" = "Risk Response\nRate",
  "verification_decay" = "Verification\nDecay Rate",
  "intervention_threshold" = "Intervention\nThreshold",
  "research_efficiency_ch" = "China Research\nEfficiency",
  "research_efficiency_us" = "US Research\nEfficiency",
  "governance_response" = "Governance\nResponse Rate",
  "intervention_intensity" = "Intervention\nIntensity",
  "verification_improvement" = "Verification\nImprovement Rate",
  "governance_adjustment" = "Governance\nAdjustment Rate",
  "risk_factor" = "Risk\nFactor"
)

# Calculate average parameter values for each risk category
risk_averages <- normalized_results %>%
  group_by(risk_category) %>%
  summarize(across(all_of(top6_params), mean)) %>%
  pivot_longer(cols = all_of(top6_params), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, levels = top6_params))

# Sample and add ID column to individual lines for background context
sampled_data <- normalized_results %>%
  sample_n(500) %>%
  # Add unique identifier column
  mutate(id = row_number()) %>%
  pivot_longer(cols = all_of(top6_params), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(variable = factor(variable, levels = top6_params))

# Create the professional plot
ggplot() +
  # Add background individual lines with very low alpha for context
  geom_line(data = sampled_data, 
            aes(x = variable, y = value, group = id, color = risk_category),
            alpha = 0.03, size = 0.4) +
  
  # Add bold lines showing the average parameter values by risk category
  geom_line(data = risk_averages,
            aes(x = variable, y = value, group = risk_category, color = risk_category),
            size = 1.8) +
  
  # Add points at each average value for emphasis
  geom_point(data = risk_averages,
             aes(x = variable, y = value, color = risk_category),
             size = 3) +
  
  # Use the RAND color palette
  scale_color_manual(values = rand_colors, name = "Risk Level") +
  
  # Use the readable parameter labels
  scale_x_discrete(labels = param_labels) +
  
  # Set clean theme with professional typography
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#003F6A"),
    plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add informative labels
  labs(
    title = "Key Parameter Patterns Leading to Different AGI Risk Levels",
    subtitle = "Higher safety-capability tradeoffs and lower maximum capability levels result in reduced risk",
    y = "Normalized Parameter Value (Low to High)",
    x = NULL,
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations with Latin Hypercube parameter sampling | DRAFT"
  )

##########################################################
# 4. STABLE EQUILIBRIA ANALYSIS
##########################################################

# Filter only stable equilibria
stable_equilibria <- full_results[full_results$is_stable == 1, ]

# Perform clustering on equilibrium states to identify distinct regimes
# We'll use the state variables: C_us, C_ch, S_us, S_ch, V, G, R
eq_vars <- c("C_us", "C_ch", "S_us", "S_ch", "V", "G", "R")
eq_data <- stable_equilibria[, eq_vars]

# Scale the data for better clustering
eq_data_scaled <- scale(eq_data)

# Determine optimal number of clusters using silhouette method
fviz_nbclust_result <- fviz_nbclust(eq_data_scaled, kmeans, method = "silhouette", k.max = 10)
print(fviz_nbclust_result)

# Let's say optimal number is k (from the plot), perform k-means clustering
k <- 2  # Adjust based on the plot
set.seed(123)
km <- kmeans(eq_data, centers = k, nstart = 25)

# Add cluster assignment back to results
stable_equilibria$cluster <- km$cluster

# Calculate cluster centers (in original scale)
cluster_centers_scaled <- km$centers
# Convert back to original scale
cluster_centers <- data.frame(matrix(NA, nrow = k, ncol = length(eq_vars)))
colnames(cluster_centers) <- eq_vars
for (i in 1:length(eq_vars)) {
  var_sd <- sd(eq_data[, i])
  var_mean <- mean(eq_data[, i])
  for (j in 1:k) {
    cluster_centers[j, i] <- cluster_centers_scaled[j, i] * var_sd + var_mean
  }
}

# Add row names for clarity
rownames(cluster_centers) <- paste("Regime", 1:k)

# Display the equilibrium regimes
print(cluster_centers)

# Create visualization of equilibrium clusters
# 1. PCA visualization of clusters
pca_result <- prcomp(eq_data_scaled)
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  cluster = factor(stable_equilibria$cluster)
)

# Extract the loadings to understand what each PC represents
pc_loadings <- pca_result$rotation
# Calculate absolute contributions to identify the most important variables for each PC
pc1_contributions <- abs(pc_loadings[,1])
pc2_contributions <- abs(pc_loadings[,2])

# Identify the top 3 contributors to each PC
top_pc1 <- names(sort(pc1_contributions, decreasing = TRUE)[1:3])
top_pc2 <- names(sort(pc2_contributions, decreasing = TRUE)[1:3])

# Create a summary of the loading directions
pc1_summary <- paste(
  ifelse(pc_loadings[top_pc1[1], 1] > 0, "Higher", "Lower"), " ", top_pc1[1], ", ",
  ifelse(pc_loadings[top_pc1[2], 1] > 0, "Higher", "Lower"), " ", top_pc1[2], ", ",
  ifelse(pc_loadings[top_pc1[3], 1] > 0, "Higher", "Lower"), " ", top_pc1[3],
  sep = ""
)

pc2_summary <- paste(
  ifelse(pc_loadings[top_pc2[1], 2] > 0, "Higher", "Lower"), " ", top_pc2[1], ", ",
  ifelse(pc_loadings[top_pc2[2], 2] > 0, "Higher", "Lower"), " ", top_pc2[2], ", ",
  ifelse(pc_loadings[top_pc2[3], 2] > 0, "Higher", "Lower"), " ", top_pc2[3],
  sep = ""
)

# Create readable variable labels
var_labels <- c(
  "C_us" = "US Capability",
  "C_ch" = "China Capability",
  "S_us" = "US Safety Investment",
  "S_ch" = "China Safety Investment",
  "V" = "Verification Capability",
  "G" = "Governance Strength",
  "R" = "Global Risk"
)

# Define RAND colors
rand_blue <- "#003F6A"
rand_orange <- "#E85C0D"
rand_colors <- c(rand_blue, rand_orange)
light_blue <- alpha(rand_blue, 0.1)
light_orange <- alpha(rand_orange, 0.1)

# Create descriptive names for the clusters based on their characteristics
cluster_descriptions <- data.frame(
  cluster = c("1", "2"),
  name = c("Cooperative Governance", "Competitive Development"),
  description = c("Lower capabilities, higher safety, stronger governance", 
                  "Higher capabilities, lower safety, weaker governance")
)

# Calculate centroids for each cluster for labeling
cluster_centroids <- aggregate(cbind(PC1, PC2) ~ cluster, data = pca_data, FUN = mean)
cluster_centroids$name <- cluster_descriptions$name[match(cluster_centroids$cluster, cluster_descriptions$cluster)]

pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +

  # Add gridlines first so they appear behind points
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  
  # Add points with slight transparency
  geom_point(alpha = 0.6, size = 2.5) +
  
  # Add confidence ellipses
  # stat_ellipse(linewidth = 1.2) +
  
  # Add regime labels
  geom_label_repel(
    data = cluster_centroids,
    aes(label = ifelse(cluster == 1, "Cooperative Governance", "Competitive Development")),
    fontface = "bold",
    size = 5,
    fill = "white",
    color = rand_colors[as.numeric(cluster_centroids$cluster)],
    box.padding = 0.5,
    point.padding = 0.5,
    force = 3
  ) +
  
  # Use RAND colors
  scale_color_manual(
    values = rand_colors,
    labels = c("Cooperative Governance", "Competitive Development"),
    name = "Governance Regime"
  ) +
  scale_fill_manual(
    values = c(light_blue, light_orange),
    guide = "none"
  ) +
  
  # Use truly informative axis labels based on the contribution analysis
  xlab(paste0("Governance Intensity | ", round(summary(pca_result)$importance[2,1]*100), "% of Variance", "\nCombines Safety Investment, Governance, Risk & Capability Levels")) +
  ylab(paste0("Verification Capability | ", round(summary(pca_result)$importance[2,2]*100), "% of Variance", "\n", round(pc2_contributions["V"]*100), "% Verification Capability")) +
  
  # Professional theme
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = rand_blue, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 15)),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#EEEEEE", linewidth = 0.2),
    axis.title = element_text(face = "bold", size = 11),
    plot.margin = margin(30, 30, 30, 30)
  ) +
  
  # Informative title and subtitle
  labs(
    title = "Bistability in AGI Governance Equilibria",
    subtitle = "Two distinct stable governance regimes emerge with little stable middle ground",
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT"
  )

# Print the enhanced plot
print(pca_plot)


# 2. Parallel coordinates plot of equilibrium clusters
# Define RAND color palette
rand_blue <- "#003F6A"
rand_orange <- "#E85C0D" 
rand_colors <- c(rand_blue, rand_orange)
rand_gray <- "#555555"

# Create descriptive variable names and proper order
var_labels <- c(
  "C_us" = "US Capability",
  "C_ch" = "China Capability",
  "S_us" = "US Safety\nInvestment",
  "S_ch" = "China Safety\nInvestment",
  "V" = "Verification\nCapability",
  "G" = "Governance\nStrength",
  "R" = "Global\nRisk"
)

# Define preferred variable order for more logical presentation
var_order <- c("C_us", "C_ch", "S_us", "S_ch", "V", "G", "R")

# First, create the long-format data from stable_equilibria
eq_long <- stable_equilibria %>%
  select(all_of(eq_vars), cluster) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = all_of(eq_vars), 
               names_to = "variable", 
               values_to = "value")

# Calculate average values for each regime
regime_avgs <- stable_equilibria %>%
  group_by(cluster) %>%
  summarize(across(all_of(eq_vars), mean)) %>%
  pivot_longer(cols = all_of(eq_vars), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    regime = ifelse(cluster == 1, "Cooperative Governance", "Competitive Development"),
    variable = factor(variable, levels = var_order)
  )

# Now sample a subset of lines for background (after eq_long is created)
set.seed(123) # For reproducibility
sampled_data <- eq_long %>%
  mutate(variable = factor(variable, levels = var_order)) %>%
  group_by(cluster) %>%
  sample_frac(0.2) %>% # Sample 20% from each cluster
  ungroup() %>%
  mutate(regime = ifelse(cluster == 1, "Cooperative Governance", "Competitive Development"))

# Create the professional visualization
ggplot() +
  # Add background grid
  geom_vline(xintercept = 1:length(var_order), color = "gray90", linetype = "solid") +
  
  # Add light background lines with low alpha for context
  geom_line(
    data = sampled_data, 
    aes(x = variable, y = value, group = id, color = regime),
    alpha = 0.08, linewidth = 0.5
  ) +
  
  # Add bold average lines for each regime
  geom_line(
    data = regime_avgs,
    aes(x = variable, y = value, group = regime, color = regime),
    linewidth = 2.5
  ) +
  
  # Add points at average values
  geom_point(
    data = regime_avgs,
    aes(x = variable, y = value, color = regime),
    size = 4, shape = 21, fill = "white", stroke = 2
  ) +
  
  # Add value labels for averages
  geom_text(
    data = regime_avgs,
    aes(x = variable, y = value, label = sprintf("%.2f", value), color = regime),
    vjust = -1.2, fontface = "bold", size = 3
  ) +
  
  # Use RAND color palette
  scale_color_manual(
    values = rand_colors,
    name = "Governance Regime",
    labels = c("Cooperative Governance", "Competitive Development")
  ) +
  
  # Use readable axis labels
  scale_x_discrete(labels = var_labels) +
  
  # Clean, professional theme
  theme_minimal(base_size = 12, base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = rand_blue, hjust = 0.5),
    plot.subtitle = element_text(size = 11, color = rand_gray, hjust = 0.5),
    plot.caption = element_text(size = 9, color = rand_gray, hjust = 0.5),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#EEEEEE"),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.margin = margin(t = 20, r = 30, b = 20, l = 30),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  # Add clear descriptive labels
  labs(
    title = "Distinct Characteristics of AGI Governance Equilibrium Regimes",
    subtitle = "Cooperative regimes feature higher safety and governance with lower risk, while competitive regimes show the inverse",
    y = "Average Value",
    x = NULL,
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT"
  ) +
  
  # Add dividing lines between variable categories
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "gray50", alpha = 0.7) +
  annotate("text", x = 1.5, y = -0.5, label = "Capabilities", size = 3, fontface = "bold") +
  annotate("text", x = 3.5, y = -0.5, label = "Safety", size = 3, fontface = "bold") +
  annotate("text", x = 6, y = -0.5, label = "System Properties", size = 3, fontface = "bold")

# 3. Analysis of parameter configurations leading to each regime
# Average parameter values for each cluster
param_by_cluster <- stable_equilibria %>%
  group_by(cluster) %>%
  summarize(across(names(param_ranges), mean),
            count = n(),
            risk = mean(R),
            capability_gap = mean(abs(C_us - C_ch)),
            avg_capability = mean((C_us + C_ch)/2),
            avg_safety = mean((S_us + S_ch)/2),
            verification = mean(V),
            governance = mean(G),
            .groups = "drop") 

# 4. Radar chart for equilibrium regime characteristics
# Define RAND color palette
rand_blue <- "#003F6A"
rand_orange <- "#E85C0D"

# Create a data frame with the regime characteristics (using actual values)
# This assumes param_by_cluster contains the needed data
regime_data <- data.frame(
  Characteristic = c(
    "Global Risk", "Global Risk",
    "Capability Gap", "Capability Gap",
    "Average Capability", "Average Capability",
    "Safety Investment", "Safety Investment",
    "Verification", "Verification",
    "Governance Strength", "Governance Strength"
  ),
  Regime = rep(c("Cooperative Governance", "Competitive Development"), 6),
  Value = c(
    param_by_cluster$risk[1], param_by_cluster$risk[2],                      # Risk values
    param_by_cluster$capability_gap[1], param_by_cluster$capability_gap[2],  # Capability gap
    param_by_cluster$avg_capability[1], param_by_cluster$avg_capability[2],  # Avg capability
    param_by_cluster$avg_safety[1], param_by_cluster$avg_safety[2],          # Safety
    param_by_cluster$verification[1], param_by_cluster$verification[2],      # Verification
    param_by_cluster$governance[1], param_by_cluster$governance[2]           # Governance
  )
)

# Create an alternative visualization using a grouped bar chart
# This is easier to read than a problematic radar chart
ggplot(regime_data, aes(x = Characteristic, y = Value, fill = Regime)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            position = position_dodge(width = 0.9),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = c("Cooperative Governance" = rand_blue, 
                               "Competitive Development" = rand_orange)) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = rand_blue),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 10),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(
    title = "AGI Governance Regime Characteristics",
    subtitle = "Comparison of cooperative versus competitive governance equilibria",
    y = "Value",
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT"
  )

# Alternative approach: Create a neat table visualization with arrows
# Convert to wide format for better comparison
regime_comparison <- regime_data %>%
  pivot_wider(names_from = Regime, values_from = Value) %>%
  mutate(
    Difference = round(`Cooperative Governance` - `Competitive Development`, 2),
    Direction = ifelse(Difference > 0, "↑", "↓"),
    Significance = case_when(
      abs(Difference) > 1 ~ "***",
      abs(Difference) > 0.5 ~ "**",
      abs(Difference) > 0.2 ~ "*",
      TRUE ~ ""
    ),
    DirectionText = case_when(
      Direction == "↑" & Characteristic == "Global Risk" ~ "Higher Risk",
      Direction == "↓" & Characteristic == "Global Risk" ~ "Lower Risk",
      Direction == "↑" ~ "Higher",
      Direction == "↓" ~ "Lower"
    )
  )

# Create table plot
table_plot <- ggplot(regime_comparison, aes(y = reorder(Characteristic, abs(Difference)))) +
  geom_segment(aes(x = `Competitive Development`, xend = `Cooperative Governance`, 
                   yend = reorder(Characteristic, abs(Difference)),
                   color = ifelse(Difference > 0, rand_blue, rand_orange)),
               arrow = arrow(length = unit(0.3, "cm")), size = 1.2) +
  geom_point(aes(x = `Competitive Development`), color = rand_orange, size = 4) +
  geom_point(aes(x = `Cooperative Governance`), color = rand_blue, size = 4) +
  geom_text(aes(x = `Competitive Development`, 
                label = sprintf("%.2f", `Competitive Development`)),
            hjust = 1.5, color = rand_orange, fontface = "bold") +
  geom_text(aes(x = `Cooperative Governance`, 
                label = sprintf("%.2f", `Cooperative Governance`)),
            hjust = -0.5, color = rand_blue, fontface = "bold") +
  geom_text(aes(x = (`Cooperative Governance` + `Competitive Development`)/2,
                label = paste0(Significance, " ", Direction)),
            vjust = -0.5, fontface = "bold", size = 4) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = rand_blue),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(face = "bold", size = 11),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 20, r = 50, b = 20, l = 20)
  ) +
  labs(
    title = "Comparison of AGI Governance Regimes",
    subtitle = "Differences between cooperative (blue) and competitive (orange) equilibria",
    caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT\n*** Large difference, ** Medium difference, * Small difference"
  )

# Print the table visualization - this is likely the clearest option
print(table_plot)

# Define the perfect RAND color palette
rand_blue <- "#003F6A"
rand_orange <- "#E85C0D"
rand_light_blue <- alpha(rand_blue, 0.15)
rand_light_orange <- alpha(rand_orange, 0.15)
text_color <- "#2F2F2F"

# Define clean characteristic names
char_names <- c(
  "Global Risk",
  "Capability Gap",
  "Capability Level",
  "Safety Investment",
  "Verification",
  "Governance"
)

# Create the data frame for the radar chart
# Extract the needed values from param_by_cluster
regime1_values <- c(
  param_by_cluster$risk[1],           # Global Risk
  param_by_cluster$capability_gap[1], # Capability Gap
  param_by_cluster$avg_capability[1], # Capability Level
  param_by_cluster$avg_safety[1],     # Safety
  param_by_cluster$verification[1],   # Verification
  param_by_cluster$governance[1]      # Governance
)

regime2_values <- c(
  param_by_cluster$risk[2],           # Global Risk
  param_by_cluster$capability_gap[2], # Capability Gap
  param_by_cluster$avg_capability[2], # Capability Level
  param_by_cluster$avg_safety[2],     # Safety
  param_by_cluster$verification[2],   # Verification
  param_by_cluster$governance[2]      # Governance
)

# Calculate the range for each characteristic for proper scaling
value_ranges <- data.frame(
  characteristic = char_names,
  min_val = pmin(regime1_values, regime2_values) * 0.9, # Add 10% padding
  max_val = pmax(regime1_values, regime2_values) * 1.1  # Add 10% padding
)

# Create a data frame for the radar
n_chars <- length(char_names)
angles <- seq(0, 2 * pi, length.out = n_chars + 1)
angles <- angles[-(n_chars + 1)]

# Create a custom plotting function for a world-class radar chart
plot_radar <- function() {
  # Create the plot canvas
  par(mar = c(1, 1, 2, 1), bg = "white", family = "sans")
  plot(0, 0, type = "n", xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), 
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  # Add title and subtitle
  title(main = "AGI Governance Regime Characteristics", 
        col.main = rand_blue, font.main = 2, cex.main = 1.4)
  mtext("Comparative profile of cooperative versus competitive equilibria", 
        side = 3, line = 0.5, col = text_color, cex = 0.9)
  
  # Draw grid lines
  for (i in 1:5) {
    radius <- i * 0.2
    polygon(radius * sin(angles), radius * cos(angles), 
            border = "gray90", lty = "dashed", lwd = 0.7)
  }
  
  # Draw axis lines
  for (i in 1:n_chars) {
    arrows(0, 0, 1.1 * sin(angles[i]), 1.1 * cos(angles[i]), 
           length = 0, lwd = 0.8, col = "gray70")
  }
  
  # Calculate normalized values for each regime
  regime1_norm <- vector("numeric", n_chars)
  regime2_norm <- vector("numeric", n_chars)
  
  for (i in 1:n_chars) {
    # Normalize values between 0 and 1 based on the range
    min_val <- value_ranges$min_val[i]
    max_val <- value_ranges$max_val[i]
    range_val <- max_val - min_val
    
    # Ensure we don't divide by zero
    if (range_val > 0) {
      regime1_norm[i] <- (regime1_values[i] - min_val) / range_val
      regime2_norm[i] <- (regime2_values[i] - min_val) / range_val
    } else {
      regime1_norm[i] <- 0.5
      regime2_norm[i] <- 0.5
    }
  }
  
  # Draw polygons for regimes
  # Cooperative (Blue)
  x_coop <- regime1_norm * sin(angles)
  y_coop <- regime1_norm * cos(angles)
  polygon(x_coop, y_coop, border = rand_blue, col = rand_light_blue, lwd = 2.5)
  
  # Competitive (Orange)
  x_comp <- regime2_norm * sin(angles)
  y_comp <- regime2_norm * cos(angles)
  polygon(x_comp, y_comp, border = rand_orange, col = rand_light_orange, lwd = 2.5)
  
  # Add points at each value
  points(x_coop, y_coop, pch = 16, col = rand_blue, cex = 1.2)
  points(x_comp, y_comp, pch = 16, col = rand_orange, cex = 1.2)
  
  # Add value labels next to each point
  text_offset <- 0.05  # Small offset for value labels
  
  # Add labels for Regime 1 (Cooperative)
  for (i in 1:n_chars) {
    # Position based on angle
    hjust <- ifelse(sin(angles[i]) < 0, 1.2, -0.2)
    vjust <- ifelse(cos(angles[i]) < 0, -0.2, 1.2)
    
    label <- sprintf("%.2f", regime1_values[i])
    text(x_coop[i], y_coop[i], labels = label, 
         col = rand_blue, cex = 0.8, pos = NULL,
         offset = 0.5, font = 2,
         adj = c(hjust, vjust))
  }
  
  # Add labels for Regime 2 (Competitive)
  for (i in 1:n_chars) {
    # Position based on angle but slightly offset from Regime 1
    hjust <- ifelse(sin(angles[i]) < 0, 1.2, -0.2)
    vjust <- ifelse(cos(angles[i]) < 0, -0.8, 1.8)
    
    label <- sprintf("%.2f", regime2_values[i])
    text(x_comp[i], y_comp[i], labels = label, 
         col = rand_orange, cex = 0.8, pos = NULL,
         offset = 0.5, font = 2,
         adj = c(hjust, vjust))
  }
  
  # Add characteristic labels at the end of each axis
  # Position them carefully to avoid rotation
  for (i in 1:n_chars) {
    # Adaptive positioning based on angle
    x_pos <- 1.25 * sin(angles[i])
    y_pos <- 1.25 * cos(angles[i])
    
    # Adjust horizontal justification based on position
    hjust <- 0.5
    if (sin(angles[i]) < -0.1) hjust <- 1
    if (sin(angles[i]) > 0.1) hjust <- 0
    
    # Adjust vertical justification based on position
    vjust <- 0.5
    if (cos(angles[i]) < -0.1) vjust <- 1
    if (cos(angles[i]) > 0.1) vjust <- 0
    
    # Draw the label with proper justification
    text(x_pos, y_pos, char_names[i], 
         col = text_color, font = 2, cex = 0.9,
         adj = c(hjust, vjust))
  }
  
  # Add a legend
  legend(-1.5, -1.3, 
         legend = c("Cooperative Governance", "Competitive Development"),
         col = c(rand_blue, rand_orange), 
         lwd = 3, 
         cex = 0.9, 
         bty = "n",
         horiz = TRUE)
  
  # Add explanation boxes
  # Cooperative Governance
  rect(-1.65, -0.8, -0.35, -0.3, 
       col = alpha("white", 0.85), border = alpha(rand_blue, 0.3))
  text(-1.6, -0.4, adj = c(0, 0.5), cex = 0.9, col = rand_blue, font = 2,
       labels = "Cooperative Governance:")
  text(-1.57, -0.5, adj = c(0, 0.5), cex = 0.8,
       labels = "• Lower capability levels")
  text(-1.57, -0.6, adj = c(0, 0.5), cex = 0.8, 
       labels = "• Higher safety & governance")
  text(-1.57, -0.7, adj = c(0, 0.5), cex = 0.8, font = 2,
       labels = "• Lower global risk")
  
  # Competitive Development
  rect(0.35, -0.8, 1.65, -0.3, 
       col = alpha("white", 0.85), border = alpha(rand_orange, 0.3))
  text(0.4, -0.4, adj = c(0, 0.5), cex = 0.9, col = rand_orange, font = 2,
       labels = "Competitive Development:")
  text(0.43, -0.5, adj = c(0, 0.5), cex = 0.8,
       labels = "• Higher capability levels")  
  text(0.43, -0.6, adj = c(0, 0.5), cex = 0.8,
       labels = "• Lower safety & governance")
  text(0.43, -0.7, adj = c(0, 0.5), cex = 0.8, font = 2,
       labels = "• Higher global risk")
  
  # Add attribution
  mtext("RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT", 
        side = 1, line = 0.2, cex = 0.7, col = "gray50")
}

# Open a PNG device for high-quality output
png("world_class_radar_chart.png", width = 8, height = 7, units = "in", res = 300)
plot_radar()
dev.off()

# Also display in the current device when running interactively
plot_radar()


# # 5. Path to Equilibrium - Transition Analysis
# # Define RAND color palette
# rand_blue <- "#003F6A"  
# rand_orange <- "#E85C0D"
# rand_colors <- c(rand_blue, rand_orange)
# rand_gray <- "#555555"
# 
# # Select representative parameter sets for each equilibrium regime
# representative_params <- stable_equilibria %>%
#   group_by(cluster) %>%
#   slice(1) %>%
#   ungroup() %>%
#   select(all_of(names(param_ranges)))
# 
# # For each regime, run a full simulation with time series
# regime_simulations <- list()
# regime_names <- c("Cooperative Governance", "Competitive Development")
# 
# for (i in 1:nrow(representative_params)) {
#   # Set up parameters for this regime
#   regime_params <- params
#   for (j in 1:length(names(param_ranges))) {
#     param_name <- names(param_ranges)[j]
#     regime_params[param_name] <- representative_params[i, param_name]
#   }
#   
#   # Run simulation
#   sim_times <- seq(0, 50, by = 0.5)
#   sim <- ode(
#     y = initial_state, 
#     times = sim_times, 
#     func = agi_governance_system, 
#     parms = regime_params
#   )
#   
#   # Store result with descriptive regime name
#   regime_simulations[[i]] <- as.data.frame(sim)
#   colnames(regime_simulations[[i]]) <- c("time", "C_us", "C_ch", "S_us", "S_ch", "V", "G", "R")
#   regime_simulations[[i]]$regime <- regime_names[i]
# }
# 
# # Combine all simulations
# all_sims <- bind_rows(regime_simulations)
# 
# # Create better variable names for plotting
# var_names <- c(
#   "R" = "Global Risk",
#   "C_us" = "US Capability",
#   "C_ch" = "China Capability",
#   "S_us" = "US Safety Investment",
#   "S_ch" = "China Safety Investment",
#   "V" = "Verification Capability",
#   "G" = "Governance Strength"
# )
# 
# # Organize variables by category
# capability_vars <- c("C_us", "C_ch")
# safety_vars <- c("S_us", "S_ch")
# system_vars <- c("V", "G", "R")
# 
# # Define RAND color palette
# rand_blue <- "#003F6A"
# rand_orange <- "#E85C0D"
# 
# # Extract data and rename regimes for clarity
# key_sims <- bind_rows(
#   regime_simulations[[1]] %>% mutate(Regime = "Cooperative Governance"),
#   regime_simulations[[2]] %>% mutate(Regime = "Competitive Development")
# )
# 
# # Reshape data for better visualization
# long_data <- key_sims %>%
#   select(time, Regime, C_us, C_ch, S_us, S_ch, V, G, R) %>%
#   pivot_longer(
#     cols = c(C_us, C_ch, S_us, S_ch, V, G, R),
#     names_to = "variable",
#     values_to = "value"
#   ) %>%
#   mutate(
#     category = case_when(
#       variable %in% c("C_us", "C_ch") ~ "Capability",
#       variable %in% c("S_us", "S_ch") ~ "Safety Investment",
#       variable %in% c("V", "G") ~ "System Properties",
#       variable == "R" ~ "Risk"
#     ),
#     country = case_when(
#       variable %in% c("C_us", "S_us") ~ "United States",
#       variable %in% c("C_ch", "S_ch") ~ "China",
#       TRUE ~ "Global"
#     ),
#     measure = case_when(
#       variable %in% c("C_us", "C_ch") ~ "Capability",
#       variable %in% c("S_us", "S_ch") ~ "Safety",
#       variable == "V" ~ "Verification",
#       variable == "G" ~ "Governance",
#       variable == "R" ~ "Risk"
#     )
#   )
# 
# # Create comprehensive visualization with facets
# p <- ggplot(long_data, aes(x = time, y = value, color = Regime, linetype = country)) +
#   geom_line(size = 1) +
#   
#   # Create nested facets by category and measure
#   facet_nested(
#     category + measure ~ ., 
#     scales = "free_y",
#     nest_line = TRUE
#   ) +
#   
#   # Create custom scales
#   scale_color_manual(values = c("Cooperative Governance" = rand_blue, 
#                                 "Competitive Development" = rand_orange)) +
#   scale_linetype_manual(values = c("United States" = "solid", 
#                                    "China" = "dashed",
#                                    "Global" = "solid")) +
#   
#   # Add the vertical divider for regime divergence
#   geom_vline(xintercept = 25, linetype = "dotted", color = "gray50", alpha = 0.7) +
#   
#   # Create clean theme
#   theme_minimal(base_size = 12, base_family = "Arial") +
#   theme(
#     plot.title = element_text(face = "bold", size = 16, color = rand_blue, hjust = 0.5),
#     plot.subtitle = element_text(size = 12, hjust = 0.5),
#     strip.text = element_text(face = "bold", size = 11),
#     strip.background = element_rect(fill = "#F5F5F5", color = NA),
#     panel.spacing.y = unit(1, "lines"),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     legend.box = "vertical",
#     axis.title = element_text(face = "bold"),
#     legend.title = element_text(face = "bold")
#   ) +
#   
#   # Add labels
#   labs(
#     title = "Evolution of AGI Governance Variables Over Time",
#     subtitle = "Comparing trajectories across cooperative and competitive regimes",
#     x = "Time", 
#     y = "Value",
#     linetype = "Actor",
#     caption = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT"
#   )
# 
# # Print and save the figure
# ggsave("comprehensive_agi_governance.png", p, width = 12, height = 10, dpi = 300)
# print(p)

# Create a summary report of all equilibrium regimes
# Define RAND color palette
rand_blue <- "#003F6A"
rand_orange <- "#E85C0D"
rand_light_blue <- "#EEF3F7"

# Add descriptive regime names and format percentage manually
equilibrium_summary <- equilibrium_summary %>%
  mutate(regime_name = case_when(
    cluster == 1 ~ "Cooperative Governance",
    cluster == 2 ~ "Competitive Development",
    TRUE ~ paste("Regime", cluster)
  )) %>%
  # Calculate percentage and format it with % symbol manually
  mutate(
    percentage = count / sum(count) * 100,
    percentage_fmt = paste0(round(percentage, 1), "%")
  )

# Organize variables into logical groups
outcome_vars <- c("risk", "avg_capability", "avg_safety", "verification", "governance", "capability_gap")
parameter_vars <- c("safety_penalty", "verification_improvement", "intervention_intensity")

# Basic table that should work with any gt version
basic_table <- equilibrium_summary %>%
  select(regime_name, count, percentage_fmt, all_of(c(outcome_vars, parameter_vars))) %>%
  rename(
    "Governance Regime" = regime_name,
    "Count" = count,
    "% of Stable Outcomes" = percentage_fmt,
    "Global Risk" = risk,
    "Capability Level" = avg_capability,
    "Safety Investment" = avg_safety,
    "Verification" = verification,
    "Governance" = governance, 
    "Capability Gap" = capability_gap,
    "Safety-Capability Tradeoff" = safety_penalty,
    "Verification Improvement" = verification_improvement,
    "Intervention Intensity" = intervention_intensity
  ) %>%
  gt() %>%
  # Set table title
  tab_header(
    title = "AGI Governance Equilibrium Regimes",
    subtitle = "Characteristics of stable governance outcomes identified through cluster analysis"
  ) %>%
  # Format numbers consistently
  fmt_number(
    columns = c("Global Risk", "Capability Level", "Safety Investment", 
                "Verification", "Governance", "Capability Gap", 
                "Safety-Capability Tradeoff", "Verification Improvement", 
                "Intervention Intensity"),
    decimals = 2
  ) %>%
  # Add simple column grouping
  tab_spanner(
    label = "Regime Frequency",
    columns = c("Count", "% of Stable Outcomes")
  ) %>%
  tab_spanner(
    label = "Equilibrium Outcomes",
    columns = c("Global Risk", "Capability Level", "Safety Investment", 
                "Verification", "Governance", "Capability Gap")
  ) %>%
  tab_spanner(
    label = "Key Parameter Values",
    columns = c("Safety-Capability Tradeoff", "Verification Improvement", "Intervention Intensity")
  ) %>%
  # Add minimal styling
  tab_options(
    table.border.top.style = "none",
    table.border.bottom.style = "none"
  ) %>%
  # Add source note
  tab_source_note(
    source_note = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT"
  )

# Print the basic table
print(basic_table)

# Alternative approach using kableExtra if you have it installed
# This is often more reliable across different R environments
if (requireNamespace("kableExtra", quietly = TRUE)) {
  library(kableExtra)
  
  # Create formatted data for the table
  kbl_data <- equilibrium_summary %>%
    select(regime_name, count, percentage, all_of(c(outcome_vars, parameter_vars))) %>%
    rename(
      "Governance Regime" = regime_name,
      "Count" = count,
      "% of Outcomes" = percentage,
      "Global Risk" = risk,
      "Capability Level" = avg_capability,
      "Safety Investment" = avg_safety,
      "Verification" = verification,
      "Governance" = governance, 
      "Capability Gap" = capability_gap,
      "Safety Tradeoff" = safety_penalty,
      "Verification Improvement" = verification_improvement,
      "Intervention Intensity" = intervention_intensity
    ) %>%
    mutate(`% of Outcomes` = sprintf("%.1f%%", `% of Outcomes`))
  
  # Create the kable table with styling
  kbl_table <- kbl_data %>%
    kableExtra::kbl(caption = "AGI Governance Equilibrium Regimes") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), 
                              full_width = FALSE,
                              font_size = 12) %>%
    kableExtra::add_header_above(c(" " = 1, "Regime Frequency" = 2, 
                                   "Equilibrium Outcomes" = length(outcome_vars), 
                                   "Key Parameter Values" = length(parameter_vars))) %>%
    kableExtra::row_spec(0, background = rand_light_blue, bold = TRUE) %>%
    kableExtra::row_spec(1, background = "#E8F4F9") %>%
    kableExtra::row_spec(2, background = "#FDF2EA") %>%
    kableExtra::footnote(general = "RAND Corporation AGI Governance Model | Based on 1,000 simulations | DRAFT")
  
  print(kbl_table)
}
