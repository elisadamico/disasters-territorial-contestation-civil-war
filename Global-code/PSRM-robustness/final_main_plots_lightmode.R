# Manual plotting script from DiD results - LIGHT MODE VERSION
# Modified for publication-ready figures with white backgrounds and black text

# Load required libraries
library(ggplot2)
library(dplyr)
library(gridExtra)

# Create output directory
output_dir <- "../output/figures_lightmode"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Extract data manually from results
# REPRESSION - LOW CONTESTATION
data_rep_low <- data.frame(
  time = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
           17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33),
  att = c(-1.3244, -0.2442, 0.2237, 0.5855, 
          -0.1312, -0.6306, -0.0048, 0.7127, -0.5794, 0.4406, 0.1468, 0.4195, 
          0.2354, 1.4120, 0.4799, 0.5715, 0.6815, 0.9326, 0.9142, 0.7811, 
          0.9403, 0.8821, 0.9370, 0.8856, 0.8976, 0.8338, 0.9300, 0.9144, 
          0.6327, 0.9580, 0.8234, 1.0526, 0.9125, 0.9643, 0.8102, 1.0113, 
          1.0262, 1.0152, 1.0186, 0.5297, 0.5340, 0.6623, 0.9585),
  se = c(1.1060, 0.6157, 0.4223, 0.4380, 
         0.1659, 0.5472, 0.5965, 0.4071, 0.2975, 0.2383, 0.3817, 0.2890, 
         0.2197, 0.7303, 0.1830, 0.2473, 0.3047, 0.2212, 0.2278, 0.2433, 
         0.2329, 0.2243, 0.2475, 0.2411, 0.2253, 0.2305, 0.2347, 0.2286, 
         0.2487, 0.2409, 0.2450, 0.2503, 0.2536, 0.2620, 0.2960, 0.2524, 
         0.2826, 0.3256, 0.3217, 0.3514, 0.3558, 0.4252, 0.5847)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# REPRESSION - HIGH CONTESTATION
data_rep_high <- data.frame(
  time = c(-9, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
           27, 28, 29, 30, 31, 32),
  att = c(1.2674, -0.2131, 2.6753, 0.4751, 0.5272, 0.1336, 1.3931, 
          -0.2027, -2.2290, 0.6006, 0.8576, -0.0817, -0.1304, 1.7522, 2.2913, 
          -0.8273, 0.3591, 0.7978, 0.7671, 0.7050, -2.4230, 1.7041, -1.2048, 
          0.2827, 0.1380, -0.4371, 0.8390, -3.4879, 0.0616, 0.9626, 0.6468, 
          -0.8165, -1.4522, 0.8405, 0.8065, 3.9817, 3.2513, 4.8143, 2.9318),
  se = c(1.2860, 2.2596, 4.0200, 1.4204, 0.9219, 1.4330, 2.1380, 
         1.3563, 2.3225, 1.3636, 0.9982, 0.7427, 0.4374, 4.4742, 1.6312, 
         2.1025, 1.4449, 1.3078, 1.4497, 1.3801, 2.5103, 1.4270, 3.0648, 
         1.4653, 1.4241, 2.3966, 1.4115, 3.7415, 1.8712, 1.9584, 1.7712, 
         2.0977, 2.0729, 1.8318, 3.8943, 3.6186, 4.6990, 3.9587, 3.8310)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# MILITARY - LOW CONTESTATION
data_mil_low <- data.frame(
  time = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
           17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33),
  att = c(-0.5144, 0.0301, 0.3964, 0.2330, 
          -0.0189, -0.1695, 0.0564, 0.2673, -0.4177, 0.2149, 0.3920, 0.4918, 
          0.2324, 0.6810, 0.3455, 0.4686, 0.3505, 0.5567, 0.5344, 0.4200, 
          0.5423, 0.5004, 0.5486, 0.5188, 0.5856, 0.4614, 0.5491, 0.5307, 
          0.4044, 0.5760, 0.5208, 0.6062, 0.5502, 0.5618, 0.4937, 0.6063, 
          0.6032, 0.5380, 0.5424, 0.4011, 0.4016, 0.4740, 0.6449),
  se = c(0.1797, 0.1784, 0.2535, 0.2204, 
         0.0708, 0.1072, 0.1292, 0.4320, 0.2204, 0.0907, 0.2498, 0.2817, 
         0.0729, 0.3050, 0.1124, 0.1354, 0.0890, 0.0749, 0.0795, 0.0880, 
         0.0806, 0.0798, 0.0781, 0.0829, 0.0988, 0.0838, 0.0733, 0.0737, 
         0.0821, 0.0799, 0.0812, 0.0860, 0.0740, 0.0891, 0.0940, 0.0887, 
         0.0903, 0.1026, 0.1060, 0.1099, 0.1105, 0.1405, 0.1909)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# MILITARY - HIGH CONTESTATION
data_mil_high <- data.frame(
  time = c(-9, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
           27, 28, 29, 30, 31, 32),
  att = c(0.5166, -0.1302, 0.4162, -0.1882, 0.3980, -0.0661, 1.0649, 
          -0.3807, -0.4406, 0.3373, 0.2623, -0.1201, 0.1579, 0.8086, 0.4878, 
          -0.2442, 0.3872, 1.0187, 0.3816, 0.0972, -0.2305, 0.5707, 1.1196, 
          -0.0459, 0.1737, 2.1045, 0.2040, 0.3199, 0.3241, 1.4171, 0.4757, 
          0.0681, -0.0252, 0.7059, 0.9350, 1.7772, 1.5729, 0.5376, 0.6673),
  se = c(0.5859, 0.4238, 0.5336, 0.4778, 0.4003, 0.4185, 1.2471, 
         0.4613, 0.3916, 0.4909, 0.5596, 0.2776, 0.3211, 1.1402, 0.5593, 
         0.3121, 0.3481, 0.4113, 0.4639, 0.3510, 0.3307, 0.3232, 0.8378, 
         0.3284, 0.4159, 0.4192, 0.4472, 0.8526, 0.4908, 0.8437, 0.6228, 
         0.6273, 0.5647, 0.8983, 1.0996, 2.2910, 2.3050, 1.0575, 1.4298)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# TERRITORIAL - LOW CONTESTATION
data_terr_all_low <- data.frame(
  time = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
           17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33),
  att = c(-0.0521, -0.0044, 0.0087, 0.0199, 
          -0.0091, -0.0260, 0.0071, -0.0419, -0.0837, -0.0090, 0.0984, -0.0013, 
          -0.0026, 0.0455, 0.0073, -0.0018, -0.0010, 0.0301, 0.0268, 0.0245, 
          0.0279, 0.0274, 0.0260, 0.0255, 0.0280, 0.0238, 0.0295, 0.0294, 
          0.0230, 0.0300, 0.0290, 0.0347, 0.0316, 0.0308, 0.0303, 0.0343, 
          0.0305, 0.0331, 0.0306, 0.0120, 0.0156, 0.0182, 0.0232),
  se = c(0.0420, 0.0264, 0.0146, 0.0107, 
         0.0064, 0.0212, 0.0254, 0.1770, 0.0806, 0.0127, 0.1350, 0.0101, 
         0.0104, 0.0269, 0.0075, 0.0110, 0.0170, 0.0085, 0.0088, 0.0105, 
         0.0092, 0.0095, 0.0086, 0.0090, 0.0086, 0.0092, 0.0088, 0.0086, 
         0.0095, 0.0090, 0.0092, 0.0097, 0.0089, 0.0103, 0.0100, 0.0091, 
         0.0099, 0.0156, 0.0169, 0.0089, 0.0077, 0.0087, 0.0128)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# TERRITORIAL - HIGH CONTESTATION
data_terr_all_high <- data.frame(
  time = c(-9, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
           27, 28, 29, 30, 31, 32),
  att = c(0.0359, -0.0417, 0.0974, -0.0258, 0.0130, 0.0406, 0.9433, 
          -0.0341, -0.0738, -0.0148, -0.0202, -0.0181, -0.0050, 0.0820, -0.0082, 
          0.2031, -0.0520, -0.0335, 0.0006, -0.0427, -0.0518, -0.0615, -0.0464, 
          -0.0196, -0.0990, -0.0054, -0.0971, -0.0783, -0.0366, -0.0802, -0.0163, 
          -0.1494, -0.0473, -0.0966, -0.0381, -0.0332, -0.1723, 0.0107, -0.1320),
  se = c(0.1139, 0.0776, 0.1258, 0.0910, 0.0760, 0.1194, 0.9584, 
         0.0377, 0.0832, 0.0327, 0.0599, 0.0729, 0.0382, 0.1216, 0.0474, 
         0.2450, 0.0595, 0.0399, 0.0387, 0.0622, 0.0599, 0.0516, 0.0516, 
         0.0456, 0.0644, 0.0399, 0.0848, 0.1014, 0.0585, 0.0751, 0.0516, 
         0.0980, 0.0965, 0.0756, 0.1028, 0.0706, 0.1029, 0.0684, 0.1207)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# DISPLACEMENT - LOW CONTESTATION
data_idp_low <- data.frame(
  time = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 
           0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
           17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31),
  att = c(26055.89, 26418.10, 32237.56, 
          -9639.15, 14970.30, 13142.11, -13519.50, 8559.68, 7837.61, -12705.13, 
          2340.02, 2346.71, 17321.76, -3925.07, -433.33, 12242.27, -1262.74, 
          22537.77, -23479.05, -24504.47, -44889.54, -14943.77, 24732.13, 
          82738.59, 199382.93, 220205.21, 165345.67, 253488.56, 276162.33, 
          295334.32, 393520.31, -42131.62, 267188.84, -54713.12, -54174.44, 
          -48881.95, -34360.09, -36926.24, -47670.42, 738944.82, 1168359.74),
  se = c(11807.79, 7913.41, 10728.49, 
         10421.56, 10546.33, 8112.37, 10223.71, 11292.60, 10141.84, 4710.84, 
         3350.45, 5163.06, 13709.95, 5086.55, 4724.77, 13751.69, 4636.19, 
         20443.82, 8338.63, 9597.54, 15563.48, 18788.49, 24637.31, 51684.87, 
         46284.36, 45410.24, 46525.77, 52674.42, 61491.58, 56688.64, 70532.98, 
         17061.95, 72438.50, 14454.74, 16501.60, 13309.26, 21105.13, 21300.59, 
         19138.53, 111714.08, 171890.69)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# DISPLACEMENT - HIGH CONTESTATION
data_idp_high <- data.frame(
  time = c(-9, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 
           27, 28, 29, 30, 31),
  att = c(-11254.88, 18132.05, -50514.59, 68617.12, -13922.99, 13236.46, 
          -70909.82, -31920.23, -34689.42, -81741.51, -55769.47, -87814.31, 
          -53905.78, -53980.14, -136397.23, -134881.31, -83100.06, -166005.41, 
          -194589.98, -144812.83, -173510.79, -306049.99, -284942.12, -188800.71, 
          -208281.59, -302745.68, -271391.01, -296956.04, -392234.86, -327893.08, 
          -356115.49, -158346.31, -334965.12, -452077.91, -295100.27, -304140.89, 
          -384548.16, -2509.57),
  se = c(25227.51, 16907.79, 21158.32, 19020.22, 15495.32, 12065.73, 
         65052.90, 16128.82, 20124.06, 40871.09, 34601.57, 35931.12, 28593.80, 
         50098.96, 49055.47, 44737.25, 69674.25, 52810.49, 58927.65, 48060.99, 
         63858.09, 69276.37, 58061.75, 100284.06, 74672.13, 78951.84, 77116.61, 
         81587.22, 94392.83, 79548.14, 97625.35, 83270.56, 75635.14, 100947.73, 
         158229.99, 160235.64, 137857.92, 259110.86)
) %>%
  mutate(
    ci_lower = att - 1.96 * se,
    ci_upper = att + 1.96 * se,
    period = factor(ifelse(time < 0, "Pre", "Post"), levels = c("Pre", "Post"))
  )

# Create styled plots with black background

# Repression - Low (Light Purple)
plot_rep_low_styled <- ggplot(data_rep_low, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#E1BEE7", "Post" = "#B39DDB"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#E1BEE7", "Post" = "#B39DDB"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (Low)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Repression - High (Dark Purple)
plot_rep_high_styled <- ggplot(data_rep_high, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#9575CD", "Post" = "#4B0082"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#9575CD", "Post" = "#4B0082"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (High)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Military - Low (Light Magenta)
plot_mil_low_styled <- ggplot(data_mil_low, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#FFB3E1", "Post" = "#FF69C1"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#FFB3E1", "Post" = "#FF69C1"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (Low)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Military - High (Dark Magenta)
plot_mil_high_styled <- ggplot(data_mil_high, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#FF3DAD", "Post" = "#AF0071"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#FF3DAD", "Post" = "#AF0071"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (High)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Territorial - Low (Light Orange) with manual limits
plot_terr_all_low_styled <- ggplot(data_terr_all_low, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#FDD0A2", "Post" = "#FDAE6B"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#FDD0A2", "Post" = "#FDAE6B"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  coord_cartesian(ylim = c(-0.11, 0.11)) +
  labs(title = "Contestation (Low)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Territorial - High (Dark Orange) with manual limits
plot_terr_all_high_styled <- ggplot(data_terr_all_high, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#FD9E4D", "Post" = "#F16913"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#FD9E4D", "Post" = "#F16913"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Contestation (High)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# IDP - Low (Light Yellow)
plot_idp_low_styled <- ggplot(data_idp_low, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#FCFFA4", "Post" = "#F7F979"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#FCFFA4", "Post" = "#F7F979"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (Low)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# IDP - High (Dark Yellow/Gold)
plot_idp_high_styled <- ggplot(data_idp_high, aes(x = time, y = att, color = period)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = period), 
              alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", alpha = 0.8) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey50", alpha = 0.8) +
  scale_color_manual(values = c("Pre" = "#F1ED5F", "Post" = "#E4CD3D"),
                     labels = c("Pre-Treatment", "Post-Treatment")) +
  scale_fill_manual(values = c("Pre" = "#F1ED5F", "Post" = "#E4CD3D"),
                    labels = c("Pre-Treatment", "Post-Treatment")) +
  labs(title = "Contestation (High)", x = "Time to Treatment (Months)", y = "ATTs") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey70"),
    panel.grid.minor = element_line(color = "grey80"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5, 
                              family = "serif", color = "black"),
    axis.text = element_text(family = "serif", color = "black"),
    axis.title = element_text(family = "serif", color = "black"),
    legend.background = element_rect(fill = "white"),
    legend.text = element_text(family = "serif", color = "black"),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Create grids with black backgrounds
grid_repression <- grid.arrange(
  plot_rep_low_styled + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_rep_high_styled + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  nrow = 2
)

grid_military <- grid.arrange(
  plot_mil_low_styled + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_mil_high_styled + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  nrow = 2
)

grid_territorial <- grid.arrange(
  plot_terr_all_low_styled + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_terr_all_high_styled + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  nrow = 2
)

grid_idps <- grid.arrange(
  plot_idp_low_styled + theme(plot.margin = unit(c(0.5, 0.5, 0.2, 0.5), "cm")),
  plot_idp_high_styled + theme(plot.margin = unit(c(0.2, 0.5, 0.5, 0.5), "cm")),
  nrow = 2
)

# Save light mode versions with exact filenames used in paper
library(gridExtra)

# Save individual grid plots as PNG files to match tex references
ggsave(paste0(output_dir, "/repdid.png"), grid_repression,
       width = 12, height = 8, dpi = 300, bg = "white")

ggsave(paste0(output_dir, "/milactdid.png"), grid_military,
       width = 12, height = 8, dpi = 300, bg = "white")

ggsave(paste0(output_dir, "/tertakdid.png"), grid_territorial,
       width = 12, height = 8, dpi = 300, bg = "white")

ggsave(paste0(output_dir, "/idpdid.png"), grid_idps,
       width = 12, height = 8, dpi = 300, bg = "white")

cat("Light mode figures saved to:", output_dir, "\n")
cat("Generated files matching .tex references:\n")
cat("- repdid.png\n")
cat("- milactdid.png\n")
cat("- tertakdid.png\n")
cat("- idpdid.png\n")
cat("Note: africa_contestation_map.png needs separate generation from spatial code\n")