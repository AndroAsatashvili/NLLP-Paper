################################################################################
################################################################################
#                   Parameters and Shock 
################################################################################
################################################################################
# Truncate data (pre-pandemic)?

#df <- df %>%
  #filter(date < as.Date("2020-01-01"))


h <- 24             # Horizon
p <- 0.10           # Significance level
nwlag <- h         # Newey-West lag

x <- df$`mp_surprises$MPS_ORTH`  # Shock variable
y <- df$unrate                 # Dependent variable



################################################################################
################################################################################
#                   Data Preparation 
################################################################################
################################################################################

# Generate 'long - difference' dependent variable
for (i in 0:h) {
  df[[paste0("y", i)]] <- lead(y, i) - lag(y)
}

# Initialize storage for coefficients and standard errors
b <- rep(NA, h + 1)
b_se <- rep(NA, h + 1)
delta <- rep(NA, h + 1)
delta_se <-rep(NA, h + 1)


################################################################################
################################################################################
#                   Local Projection: Non - Linear (1)
################################################################################
################################################################################


for (i in 0:h) {
  model <- lm(
    df[[paste0("y", i)]] ~  
      x + lag(x) + lag(x, 2) + 
      lag(df$ldcpi, 1) + lag(df$ldcpi, 2)   + lag(df$ldcpi, 3) + 
      lag(df$GSCPI, 1) + 
      (lag(df$GSCPI) : x) + (lag(df$GSCPI) : lag(x)) + (lag(df$GSCPI) : lag(x, 2)) + (lag(df$GSCPI) : lag(x, 3)) + 
      lag(df$ldINDPROD, 1) + lag(df$ldINDPROD, 2) + lag(df$ldINDPROD, 3) + 
      lag(df$shadow) + lag(df$shadow, 2) +  lag(df$shadow, 3) + 
      lag(df$unrate) + lag(df$unrate, 2) + lag(df$unrate, 3) 
    ,
    data = df
  )
  
  # Store coefficients for the shock and interaction term
  b[i + 1] <- coef(model)["x"]
  delta[i + 1] <- coef(model)["x:lag(df$GSCPI)"]
  
  # Store Newey-West adjusted standard errors
  nw_se <- sqrt(diag(NeweyWest(model, lag = nwlag)))
  b_se[i + 1] <- nw_se["x"]
  delta_se[i + 1] <- nw_se["x:lag(df$GSCPI)"]
}

# Calculate combined effect and confidence bands
combined_effect <- b + delta
combined_se <- sqrt(b_se^2 + delta_se^2)  # Assuming independence


################################################################################
################################################################################
#                   Plotting
################################################################################
################################################################################


# Data for plotting
irf_plot_data <- data.frame(
  Horizon = seq(0, h, 1),
  Baseline_Effect = b,
  Interaction_Effect = delta,
  Combined_Effect = combined_effect,
  Baseline_u = b + b_se,
  Baseline_l = b - b_se,
  Baseline_Lower = b - qnorm(1 - p / 2) * b_se,
  Baseline_Upper = b + qnorm(1 - p / 2) * b_se,
  Interaction_u = delta + delta_se,
  Interaction_l = delta - delta_se,
  Interaction_Lower = delta - qnorm(1 - p / 2) * delta_se,
  Interaction_Upper = delta + qnorm(1 - p / 2) * delta_se,
  Combined_u = combined_effect + combined_se,
  Combined_l = combined_effect - combined_se,
  Combined_Lower = combined_effect - qnorm(1 - p / 2) * combined_se,
  Combined_Upper = combined_effect + qnorm(1 - p / 2) * combined_se
)



# 1. Baseline Effect Plot
baseline_plot <- ggplot(irf_plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = Baseline_l, ymax = Baseline_u), fill = "blue", alpha = 0.1) +
  geom_ribbon(aes(ymin = Baseline_Lower, ymax = Baseline_Upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = Baseline_Effect), color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Meses",
    y = "%",
    title = "Base"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + 
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank()
  )

# 2. Interaction Effect Plot
interaction_plot <- ggplot(irf_plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = Interaction_l, ymax = Interaction_u), fill = "red", alpha = 0.1) +
  geom_ribbon(aes(ymin = Interaction_Lower, ymax = Interaction_Upper), fill = "red", alpha = 0.2) +
  geom_line(aes(y = Interaction_Effect), color = "red", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Meses",
    y = "%",
    title = "Efecto Adicional en GSCPI"
  ) +
  scale_y_continuous(limits = c(-15, 10)) + 
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank()
  )

# 3. Combined Effect Plot
combined_plot <- ggplot(irf_plot_data, aes(x = Horizon)) +
  geom_ribbon(aes(ymin = Combined_l, ymax = Combined_u), fill = "green", alpha = 0.1) +
  geom_ribbon(aes(ymin = Combined_Lower, ymax = Combined_Upper), fill = "green", alpha = 0.2) +
  geom_line(aes(y = Combined_Effect), color = "green", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Meses",
    y = "%",
    title = "Efecto Total sobre GSCPI"
  ) +
  scale_y_continuous(limits = c(-12, 1)) + 
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank()
  )


# Combine Baseline Effect and Combined Effect into one ggplot
combined_baseline_plot <- ggplot(irf_plot_data, aes(x = Horizon)) +
  # Baseline Effect
  geom_ribbon(aes(ymin = Baseline_Lower, ymax = Baseline_Upper), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = Baseline_Effect), color = "blue", size = 1, linetype = "solid") +
  
  # Combined Effect
  geom_ribbon(aes(ymin = Combined_Lower, ymax = Combined_Upper), fill = "green", alpha = 0.2) +
  geom_line(aes(y = Combined_Effect), color = "green", size = 1, linetype = "dashed") +
  
  # Zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  # Labels and theme
  labs(
    x = "Meses",
    y = "%",
    title = "Base y efecto combinado de GSCPI en CPI"
  ) +
  scale_y_continuous(limits = c(-25, 10)) + 
  theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center the title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Rotate x-axis labels and increase font size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 1),  # Add black box around the graph
    axis.line = element_blank(),  # Remove axis lines to place axes outside the box
    axis.ticks.length = unit(-0.25, "cm"),  # Adjust tick lengths to move labels outside the box
    legend.position = "top",  # Place legend at the top
    legend.title = element_blank()
  )
gridExtra::grid.arrange(baseline_plot, interaction_plot, combined_baseline_plot, nrow = 2, ncol = 2)

