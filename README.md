# AGI Governance Dynamics Simulator

## Overview

The **AGI Governance Dynamics Simulator** is a modeling framework designed to explore the interplay between global governance, safety investments, and capability development in Artificial General Intelligence (AGI). It uses a system of differential equations to simulate dynamic interactions between major actors, such as the United States and China, as well as global governance mechanisms, safety investments, and risk dynamics.

This tool is ideal for policymakers, researchers, and analysts seeking to understand the long-term implications of AGI governance decisions and their impact on global risk.

---

## Features

1. **Dynamic System Modeling**:
   - Simulates the evolution of AGI capabilities, safety investments, governance strength, verification systems, and global risk.
   - Incorporates strategic interactions between actors with intervention mechanisms.

2. **Global Sensitivity Analysis**:
   - Uses Latin Hypercube Sampling (LHS) to explore the relationship between parameter variations and system outcomes.
   - Identifies key drivers of global risk and governance stability.

3. **Equilibrium Regime Identification**:
   - Clusters stable equilibria into distinct governance regimes (e.g., cooperative vs. competitive).
   - Provides insights into the characteristics of each regime.

4. **High-Dimensional Visualizations**:
   - Generates professional plots, including radar charts, 3D surface plots, parallel coordinate plots, and PCA-based clustering visualizations.

5. **Transition Analysis**:
   - Tracks the evolution of key metrics over time for different governance regimes.

6. **Parameter Importance Analysis**:
   - Quantifies the influence of parameters on global risk and governance outcomes using correlation metrics.

---

## Installation and Setup

### Prerequisites
Ensure the following are installed on your system:
1. **R** (version 4.0 or higher)
2. **RStudio** (optional but recommended)
3. Required R libraries:
   - `deSolve`
   - `ggplot2`
   - `tidyverse`
   - `reshape2`
   - `viridisLite`
   - `lhs`
   - `plotly`
   - `cluster`
   - `factoextra`
   - `GGally`
   - `metR`
   - `ggtext`
   - `viridis`
   - `dplyr`
   - `ggrepel`
   - `gridExtra`
   - `grid`
   - `scales`
   - `tidyr`
   - `fmsb`
   - `patchwork`
   - `ggh4x`
   - `gt`
   - `gtExtras`

### Steps
1. Clone or download the repository containing the application code.
2. Open the R script in RStudio (or your preferred R environment).
3. Run the script to execute the simulations and generate visualizations.

---

## Key Components

### 1. System of Equations
The simulator models interactions between major actors (e.g., the United States and China) and global governance systems. Key variables include:
- **Capabilities**: AGI capability levels for each actor.
- **Safety Investments**: Efforts by actors to mitigate risks.
- **Verification Systems**: Global mechanisms to verify compliance and reduce intervention intensity.
- **Governance Strength**: Effectiveness of global governance mechanisms.
- **Global Risk**: Aggregated risk level influenced by capabilities and safety investments.

### 2. Global Sensitivity Analysis
Explores the impact of varying parameters (e.g., research efficiency, safety-capability tradeoff) on system outcomes. Results include:
- Peak and final global risk levels.
- Stability metrics for equilibrium states.

### 3. Visualization
Generates professional plots to illustrate:
- High-dimensional risk landscapes.
- Parameter importance and correlations.
- Equilibrium regime characteristics.
- Transition paths to stable governance states.

### 4. Equilibrium Regime Analysis
Clusters stable equilibria into governance regimes using k-means clustering and PCA. Identifies:
- Cooperative Governance: Lower capabilities, higher safety investments, stronger governance.
- Competitive Development: Higher capabilities, lower safety investments, weaker governance.

---

## Usage Instructions

### Running Simulations
1. **Define Parameters**:
   - Set initial conditions and parameter ranges in the script.
   - Adjust settings such as intervention intensity, safety-capability tradeoff, and governance adjustment rate.

2. **Solve the System**:
   - Use `ode()` to simulate the dynamics over time.
   - Results are stored in data frames for further analysis.

3. **Global Sensitivity Analysis**:
   - Run `run_global_sensitivity_with_equilibria()` to explore parameter space and generate equilibrium metrics.

4. **Visualize Results**:
   - Use `ggplot2` and related libraries to create plots.
   - Save outputs as PNG or interactive HTML files.

### Analyzing Results
- Examine the correlation matrix to identify key drivers of global risk.
- Use clustering and PCA to understand distinct governance regimes.
- Generate radar charts, bar charts, and tables to summarize findings.

---

## Example Visualizations

### 1. Key Drivers of Global Risk
A bar chart showing the absolute correlation between parameters (e.g., safety-capability tradeoff, intervention intensity) and global risk at equilibrium.

### 2. High-Dimensional Risk Landscape
A 3D surface plot illustrating the interaction between top parameters and global risk levels.

### 3. Equilibrium Regime Characteristics
Radar charts and tables comparing cooperative and competitive governance regimes across metrics like risk, safety, verification, and governance strength.

### 4. Transition Paths
Line plots showing the evolution of capabilities, safety investments, and risk over time for different regimes.

---

## Known Issues and Limitations

1. **Computational Intensity**:
   - Large-scale sensitivity analyses with many samples may take significant time.
   - Consider reducing the number of samples for faster results.

2. **Data Dependency**:
   - Results depend heavily on initial conditions and parameter ranges. Ensure assumptions are well-justified.

3. **Simplified Models**:
   - The equations simplify complex real-world dynamics and may not capture all factors influencing AGI governance.

---

## License

This project is open-source and licensed under [MIT License](https://opensource.org/licenses/MIT).

---

## Contact

For questions, feedback, or contributions, feel free to reach out via GitHub or email.

--- 

Explore the dynamics of AGI governance and risk today!
