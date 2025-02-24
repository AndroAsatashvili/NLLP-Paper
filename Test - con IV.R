################################################################################
################################################################################
#                   Parameters and Shock 
################################################################################
################################################################################

# Truncate data (pre-pandemic)?
df <- df %>%
  filter(date < as.Date("2020-01-01"))

h <- 24             # Horizon
p <- 0.10           # Significance level
nwlag <- h         # Newey-West lag



df$p <- df$`mp_surprises$MPS_ORTH`  # Shock variable

x <- lm(dshadow ~ p + ldcpi + unrate , data = df)$fitted.values
#df <- df[-1,]
df$x <- x









y <- df$lcpi                 # Dependent variable



################################################################################
################################################################################
#                   Data Preparation 
################################################################################
################################################################################
df$GSCPI <- (df$GSCPI - mean(df$GSCPI))/(sd(df$GSCPI))  


# Generate 'long - difference' dependent variable
for (i in 0:h) {
  df[[paste0("y", i)]] <- lead(y, i) - lag(y)
}

# Initialize storage for coefficients and standard errors
b_s1 <- rep(NA, h + 1)
se_s1 <- rep(NA, h + 1)

b_s2 <- rep(NA, h + 1)
se_s2 <- rep(NA, h + 1)

b_s3 <- rep(NA, h + 1)
se_s3 <- rep(NA, h + 1)

b_s4 <- rep(NA, h + 1)
se_s4 <- rep(NA, h + 1)



################################################################################
################################################################################
#                   Regime Switching (trigger)
################################################################################
################################################################################

w <- df$revolsl        # Regime Switching Variable (trigger)

lambda_w <- 129600  # lambda 
gamma_w  <- 8       # gamma 
use_hp_w <- TRUE    # use H-P filter?
lag_switching_w <- TRUE  # Lag?

fw <- trigger_w(w)     # Function




df <- df %>%
  mutate(across(
    where(is.numeric),
    list(
      s1 = ~ . * (fw),     # High regime
      s2 = ~ . * (1 - fw) # Low regime
    ),
    .names = "{.col}_{.fn}" # Add "_s1", "_s2"
  ))



################################################################################
################################################################################
#                   Local Projection: Non - Linear (2)
################################################################################
################################################################################

for (i in 0:h) {
  model <- lm(
    df[[paste0("y", i)]] ~
      
      # Regime 1
      x_s1 + lag(x_s1, 1) + lag(x_s1, 2) + 
      lag(df$ldcpi_s1, 1) + lag(df$ldcpi_s1, 2) + 
      lag(df$GSCPI_s1, 1) +
      (lag(df$GSCPI_s1, 1) : x_s1) +  (lag(df$GSCPI_s1, 1) : lag(x_s1, 1)) +  (lag(df$GSCPI_s1, 1) : lag(x_s1, 2)) + 
      lag(df$ldINDPROD_s1, 1) + lag(df$ldINDPROD_s1, 2) + 
      lag(df$unrate_s1, 1) + lag(df$unrate_s1, 2) + 
      
      # Regimen 2
      x_s2 + lag(x_s2, 1) + lag(x_s2, 2) +
      lag(df$ldcpi_s2, 1) + lag(df$ldcpi_s2, 2) + 
      lag(df$GSCPI_s2, 1) + 
      (lag(df$GSCPI_s2, 1) : x_s2) + (lag(df$GSCPI_s2, 1) : lag(x_s2, 1)) +  (lag(df$GSCPI_s2, 1) : lag(x_s2, 2)) + 
      lag(df$ldINDPROD_s2, 1) + lag(df$ldINDPROD_s1, 2) + 
      lag(df$unrate_s2, 1) + lag(df$unrate_s2, 2) 
    ,
    data = df
  )
  
  # Store coefficients
  b_s1[i + 1] <- coef(model)["x_s1"]
  se_s1[i + 1] <- sqrt(diag(NeweyWest(model, lag = nwlag))["x_s1"])
  b_s3[i + 1] <- coef(model)["x_s1:lag(df$GSCPI_s1, 1)"]
  se_s3[i + 1] <- sqrt(diag(NeweyWest(model, lag = nwlag))["x_s1:lag(df$GSCPI_s1, 1)"])
  b_s2[i + 1] <- coef(model)["x_s2"]
  se_s2[i + 1] <- sqrt(diag(NeweyWest(model, lag = nwlag))["x_s2"])
  b_s4[i + 1] <- coef(model)["x_s2:lag(df$GSCPI_s2, 1)"]
  se_s4[i + 1] <- sqrt(diag(NeweyWest(model, lag = nwlag))["x_s2:lag(df$GSCPI_s2, 1)"])
}

b_s3 <- b_s1 + b_s3
se_s3 <- sqrt(se_s1^2 + se_s3^2) 

b_s4 <- b_s2 + b_s4
se_s4 <-sqrt(se_s2^2 + se_s4^2) 


################################################################################
################################################################################
#                   Plotting
################################################################################
################################################################################

# Calculate confidence bands
bb <- data.frame(
  b_s1 = b_s1, # Include coefficients for s1
  b_s2 = b_s2, # Include coefficients for s2
  b_s3 = b_s3, # Include coefficients for s3
  b_s4 = b_s4, # Include coefficients for s4
  
  # Upper and lower bounds for s1
  u11 = b_s1 + se_s1,
  d11 = b_s1 - se_s1,
  u21 = b_s1 + qnorm(1 - p / 2) * se_s1,
  d21 = b_s1 - qnorm(1 - p / 2) * se_s1,
  
  # Upper and lower bounds for s2
  u12 = b_s2 + se_s2,
  d12 = b_s2 - se_s2,
  u22 = b_s2 + qnorm(1 - p / 2) * se_s2,
  d22= b_s2 - qnorm(1 - p / 2) * se_s2,
  
  # Upper and lower bounds for s3
  u13 = b_s3 + se_s3,
  d13 = b_s3 - se_s3,
  u23 = b_s3 + qnorm(1 - p / 2) * se_s3,
  d23 = b_s3 - qnorm(1 - p / 2) * se_s3,
  
  # Upper and lower bounds for s4
  u14 = b_s4 + se_s4,
  d14 = b_s4 - se_s4,
  u24 = b_s4 + qnorm(1 - p / 2) * se_s4,
  d24 = b_s4 - qnorm(1 - p / 2) * se_s4
)




# Compute global Y-axis limits
y_min <- min(bb$d21, bb$d22, bb$d23, bb$d24, bb$u21, bb$u22, bb$u23, bb$u24, 
             bb$d11, bb$d12, bb$d13, bb$d14, bb$u11, bb$u12, bb$u13, bb$u14)
y_max <- max(bb$d21, bb$d22, bb$d23, bb$d24, bb$u21, bb$u22, bb$u23, bb$u24, 
             bb$d11, bb$d12, bb$d13, bb$d14, bb$u11, bb$u12, bb$u13, bb$u14)

# Plot for scenario s1
plot_s1 <- ggplot(bb, aes(x = seq(0, h, 1))) +
  geom_ribbon(aes(ymin = d21, ymax = u21), fill = "blue", alpha = 0.1) +
  geom_ribbon(aes(ymin = d11, ymax = u11), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = b_s1), color = "darkblue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Meses", y = "%", title = "GSCPI = 0, Baja Deuda") +
  scale_y_continuous(limits = c(y_min, y_max)) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none"
  )

# Plot for scenario s2
plot_s2 <- ggplot(bb, aes(x = seq(0, h, 1))) +
  geom_ribbon(aes(ymin = d22, ymax = u22), fill = "green", alpha = 0.1) +
  geom_ribbon(aes(ymin = d12, ymax = u12), fill = "green", alpha = 0.2) +
  geom_line(aes(y = b_s2), color = "darkgreen", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Meses", y = "%", title = "GSCPI = 0, Alta Deuda") +
  scale_y_continuous(limits = c(y_min, y_max)) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none"
  )

# Plot for scenario s3
plot_s3 <- ggplot(bb, aes(x = seq(0, h, 1))) +
  geom_ribbon(aes(ymin = d23, ymax = u23), fill = "red", alpha = 0.1) +
  geom_ribbon(aes(ymin = d13, ymax = u13), fill = "red", alpha = 0.2) +
  geom_line(aes(y = b_s3), color = "darkred", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Meses", y = "%", title = "GSCPI > 0, Baja Deuda") +
  scale_y_continuous(limits = c(y_min, y_max)) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none"
  )

# Plot for scenario s4
plot_s4 <- ggplot(bb, aes(x = seq(0, h, 1))) +
  geom_ribbon(aes(ymin = d24, ymax = u24), fill = "orange", alpha = 0.1) +
  geom_ribbon(aes(ymin = d14, ymax = u14), fill = "orange", alpha = 0.2) +
  geom_line(aes(y = b_s4), color = "darkorange", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(x = "Meses", y = "%", title = "GSCPI > 0, Alta Deuda") +
  scale_y_continuous(limits = c(y_min, y_max)) + 
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.line = element_blank(),
    axis.ticks.length = unit(-0.25, "cm"),
    legend.position = "none"
  )

# Arrange all four plots in a grid
grid.arrange(plot_s3, plot_s4, plot_s1, plot_s2, nrow = 2)