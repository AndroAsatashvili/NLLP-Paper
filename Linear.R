################################################################################
################################################################################
#                   Parameters and Shock 
################################################################################
################################################################################

h <- 24             # Horizon
p <- 0.10           # Significance level
nwlag <- h + 1      # Newey-West lag

x <- df$`mp_surprises$MPS_ORTH`  # Shock variable
y <- df$lcpi                 # Dependent variable



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
se <- rep(NA, h + 1)


################################################################################
################################################################################
#                   Local Projection: Linear 
################################################################################
################################################################################

for (i in 0:h) {
  model <- lm(
    df[[paste0("y", i)]] ~ x + lag(x, 1) + lag(x, 2) + lag(x, 3) + lag(x, 4) + lag(x, 5) + lag(x, 6) +
      lag(x, 7) + lag(x, 8) + lag(x, 9) + lag(x, 10) + lag(x, 11) + lag(x, 12) +
      
      lag(df$ldcpi, 1) + lag(df$ldcpi, 2) + lag(df$ldcpi, 3) + lag(df$ldcpi, 4) +
      lag(df$ldcpi, 5) + lag(df$ldcpi, 6) + lag(df$ldcpi, 7) + lag(df$ldcpi, 8) +
      lag(df$ldcpi, 9) + lag(df$ldcpi, 10) + lag(df$ldcpi, 11) + lag(df$ldcpi, 12) +
      
      lag(df$ldINDPROD, 1) + lag(df$ldINDPROD, 2) + lag(df$ldINDPROD, 3) + lag(df$ldINDPROD, 4) +
      lag(df$ldINDPROD, 5) + lag(df$ldINDPROD, 6) + lag(df$ldINDPROD, 7) + lag(df$ldINDPROD, 8) +
      lag(df$ldINDPROD, 9) + lag(df$ldINDPROD, 10) + lag(df$ldINDPROD, 11) + lag(df$ldINDPROD, 12) +
      
      lag(df$shadow, 1) + lag(df$shadow, 2) + lag(df$shadow, 3) + lag(df$shadow, 4) +
      lag(df$shadow, 5) + lag(df$shadow, 6) + lag(df$shadow, 7) + lag(df$shadow, 8) +
      lag(df$shadow, 9) + lag(df$shadow, 10) + lag(df$shadow, 11) + lag(df$shadow, 12) +
      
      
      lag(df$unrate, 1) + lag(df$unrate, 2) + lag(df$unrate, 3) + lag(df$unrate, 4) +
      lag(df$unrate, 5) + lag(df$unrate, 6) + lag(df$unrate, 7) + lag(df$unrate, 8) +
      lag(df$unrate, 9) + lag(df$unrate, 10) + lag(df$unrate, 11) + lag(df$unrate, 12)
    ,
    data = df
  )
  
  # Store coefficients and Newey-West adjusted standard errors
  b[i + 1] <- coef(model)["x"]
  se[i + 1] <- sqrt(diag(NeweyWest(model, lag = nwlag))["x"])
}


################################################################################
################################################################################
#                   Plotting
################################################################################
################################################################################

# Calculate confidence bands
bb <- data.frame(
  u1 = b + se,                           # 1 SE upper band
  d1 = b - se,                           # 1 SE lower band
  u2 = b + qnorm(1 - p / 2) * se,        # CI upper band
  d2 = b - qnorm(1 - p / 2) * se         # CI lower band
)

# Plot the Impulse Response Function (IRF) with confidence bands
ggplot(bb, aes(x = seq(0, h, 1))) +
  geom_ribbon(aes(ymin = d2, ymax = u2), fill = "blue", alpha = 0.1) +
  geom_ribbon(aes(ymin = d1, ymax = u1), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = b), color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    x = "Months",
    y = "%",
    title = "IRF de CPI Acumulada"
  ) +
 scale_y_continuous(limits = c(-20, 10)) + 
theme_minimal(base_size = 15) +  # Increase base font size
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    axis.line = element_blank(),  
    axis.ticks.length = unit(-0.25, "cm"),  
    legend.position = "top", 
    legend.title = element_blank()
  )

















