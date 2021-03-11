/*Our purpose is to investigate what parameters influence a fuel economy of cars. 
The data contains the following columns:
mpg - miles traveled per gallon of consumed fuel;
cylinders - number of cylinders;
displacement - engine displacement in cubic inches;
horsepower - imperial horsepower;
weight - in pounds;
acceleration - acceleration time from 0 to 60 miles per hour in seconds;
year - year of production;
origin - 1-the USA; 2-Europe; 3-Japan.*/

/*Importing data to SAS table */
proc import datafile='/path_to_file/cars.csv' 
out=cars dbms=csv replace;
getnames=yes;
run;

/*There are 8 records with missing values. We will delete them.*/
data cars;
set cars;
if nmiss(of _numeric_) + cmiss(of _character_) > 0 then delete;
run;

/*Drawing a matrix of scatterplots.*/
proc sgscatter data=cars;
	matrix mpg--origin;
run;

/*Plots representing relationships between 'mpg' from the one side and 'horsepower', 
'displacement' and 'weight' from the other look like hyperbolas. Because we want to use 
linear models it seems reasonable to add following variables to the data. 
We will also add the variable 'logmpg' that is a logarithm of 'mpg'.*/
data cars;
set cars;
inv_weight=10000/weight;
inv_horsepower=1000/horsepower;
inv_displacement=1000/displacement;
logmpg = log(mpg);
run;

/*Our data includes next categorical variables: 'cylinders', 'year', 'origin', 'name'.
Let us standarize all other ones.*/
proc stdize data=cars;
var mpg displacement horsepower weight acceleration inv_weight inv_horsepower inv_displacement;
run;

/*We will perform general linear model selection without variables 'name' because it has 
unique values.*/
proc glmselect data=cars plot=criteria;
class cylinders origin year;
model mpg=cylinders--origin inv_weight inv_horsepower inv_displacement
/selection=stepwise(select=sl) sle=.05 sls=.05 stat=all;
run;

/*Variables 'cylinders', 'acceleration', 'year', 'inv_weight', 'inv_horsepower', 
'inv_displacement' are selected as significant. It shows that inverse dependence between 
'mpg' and 'horsepower', 'weight', 'displacement' is more probable than linear one. 
Now let us check diagnostics of the selected model.*/
proc glm data=cars plots(unpack)=diagnostics;
class cylinders origin year;
model mpg= cylinders acceleration year inv_weight inv_horsepower inv_displacement/solution;
run;

/*We can see a lack of homoscedasticity in the model above. We will use logmpg (logarithm of mpg)
as a dependent variable to fix it.*/
proc glmselect data=cars plot=criteria;
class cylinders origin year;
model logmpg=cylinders--origin inv_weight inv_horsepower inv_displacement
/selection=stepwise(select=sl) sle=.05 sls=.05 stat=all;
run;

/*Variables 'cylinders', 'acceleration', 'weight', 'inv_horsepower', 'inv_displacement',
'year' are selected as significant. Diagnostics of the selected model:*/
proc glm data=cars plots(unpack)=diagnostics;
class cylinders origin year;
model logmpg= cylinders weight acceleration year inv_horsepower inv_displacement
/solution tolerance;
run;

/*Variables 'weight' and 'inv_displacement' have too low tolerance of type II (<0.1). 
We will remove 'inv_displacement' because it has the lowest one.*/
proc glm data=cars plots(unpack)=diagnostics;
class cylinders origin year;
model logmpg= cylinders weight acceleration year inv_horsepower/solution tolerance;
output out=residuals residual=residual;
run;

/*Let us test residuals of the model for normality.*/
proc univariate data=residuals normal;
var residual;
run;
/*Kolmogorov-Smirnov test for normality has a p-value more than 0.15 so we cannot reject
hypothesis about normality of residuals. */