
library(dplyr)

mechaCar <- read.csv('MechaCar_mpg.csv')

model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mechaCar)

summary(model)

coil <- read.csv('Suspension_Coil.csv')
psi <- coil$PSI

total_summary <- coil %>%
  summarise(mean = mean(psi), median = median(psi), variance = sd(psi)^2, SD = sd(psi))

lot_summary <- coil %>%
  group_by(Manufacturing_Lot) %>%
  summarise(mean = mean(PSI), median = median(PSI), variance = sd(PSI)^2, SD = sd(PSI))

t.test(x=psi, mu=1500)
