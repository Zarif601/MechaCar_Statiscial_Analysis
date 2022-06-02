# MechaCar_Statiscial_Analysis

## Linear Regression to Predict MPG

1. The variables vehicle_length and ground_clearance (as well as the intercept) provided a non-random amount of variance to the mpg values in the dataset.

2. The slope of the linear model cannot be cosnidered zero because out p-value is 5.35e-11 which is much smaller than our assumed significance level of 0.05%.

3. Since our r-value is 0.72 and our p-value is significant, we can say that our linear model does predict mpg of MechaCar prototypes effectively. However we can see that the intercept provided a non-random amount of variance to the mpg values. This may mean that the significant features such as vehicle_length and ground_clearance may need scaling or there may be other factors influencing the mpg of the prototypes. Hence, more effective models could be made to better predict the mpg of MechaCar prototypes than our model.

![Linear Regression screenshot](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/Linear%20Regression%20screenshot.PNG)

## Summary Statistics on Suspencion Coils

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 PSI. From the total_summary dataframe we can see that the overall variance is 62.29 which is well under our threshold of 100. However, looking at the lot_summary dataframe we can observe that even though the variance of suspension coils in Lot 1 and Lot 2 meet the requirements, the variance of suspension coils in Lot 3 is 170.29 which far exceeds the 100 PSI variance threshold and hence doesn't meet the design specification.

![total_summary](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/total_summary.PNG)

![lot_summary](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/lot_summary.PNG)

## T-Tests on Suspension Coils

Performing t-test on the PSI across all manufacturing lots we found that the p-value was 0.06 while our significance level was 0.05%. Since the p-value was larger than our significance level, we can conclude that the PSI across all manufacturing lots was not statistically different from the population mean of 1500 PSI.

![All_Lots](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/T-Test%20All%20Lots.PNG)

While performing t-test on the PSI of individual lots, the p-values of Lot1, Lot2, and Lot3 were 1, 0.61 and 0.42 respectively. Here the p-values of both Lot1 and Lot2 were larger than the significance level of 0.05%. However, the p-value of Lot3 was smaller than our significance level, indicating that only for Lot3, the PSI there was statistically different from the population mean of 1500.


![Lot1](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/T-Test%20Lot1.PNG)

![Lot2](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/T-Test%20Lot2.PNG)

![Lot3](https://github.com/Zarif601/MechaCar_Statiscial_Analysis/blob/main/Resources/T-Test%20Lot3.PNG)

## Study Design: MechaCar vs Competition

To study the performance of MechaCar against its competitors we can use t-tests to quantify how MechaCar performs across different categories versus its competition. 

1. Metrics that consumers could be  interested in are, for example, cost, fuel efficienty, horsepower, safety rating, etc. and comparing there metrics of MechaCar with its competitiors could tell us how MechaCar can perform against them.

2. The null-hypothesis is that there is no statistical difference between the mean of the chosen metric from above between MechaCar and its competitior. The alternative hypothesis is that there is a statistical difference between the mean of the chosen metric between MechaCar and competitors.

3. The statistical test we can use here is two-sample t-test, one sample being the MechaCar dataset and the other one would be that of its competitor. This way we can compare the mean of MechaCar metrics and metrics of its competition to figure out how MechaCar would perform in comparison.

4. To do such a test we would need sufficient data regarding the chosen metrics. For example, if we chose fuel efficiency as our metric, we would need sufficient fuel efficiecy data from a good amount of sample from both MechaCar and the competitor against which its performance is going to be compared.
