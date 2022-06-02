library(dplyr)

#import MechaCar dataset
mechaCar <- read.csv('MechaCar_mpg.csv',check.names = F, stringsAsFactors = F)

#create linear model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechaCar)

#generate summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = mechaCar))

#import Suspension dataset
suspensionCoil <- read.csv('Suspension_Coil.csv',check.names = F, stringsAsFactors = F)

#create summary table
total_summary <- summarize(suspensionCoil, Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

#summary table grouping by lot
lot_summary = suspensionCoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

#compare sample versus population means
t.test(suspensionCoil$PSI, mu = 1500)

#compare sample vs population means of individual lots
t.test(mu = 1500, subset(suspensionCoil,Manufacturing_Lot =="Lot1")$PSI)
t.test(mu = 1500, subset(suspensionCoil,Manufacturing_Lot =="Lot2")$PSI)
t.test(mu = 1500, subset(suspensionCoil,Manufacturing_Lot =="Lot3")$PSI)