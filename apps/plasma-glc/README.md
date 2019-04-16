## Data Analysis App for Plasma Glycopeptides

This is a shiny app with statistical analysis and visualization of plasma glycoproteome from the 48 samples. Click [here](http://www.chenghaozhu.net/studies/alzheimer/apps/plasma-glc/) to visit the online instance of the app.

## Overview

This page is a overview of the sample variable and clinical characteristics of each sample/patient. Clicking on the column will generate a basic summary and a distribution plot on the right. The table shows in here is the third tab in the original spreadsheet, except the last three columns are made by me. The **Clinical Group** is generated from the **PATHDX1** column and I only kept whether *AD* or *normal*. The **apoE** column is the a conbination of ApoE(1) and ApoE(2). While the ISTD is the first row in the first tab of the spreadsheet. According to our goal, the **Clinical Group** column is probably appropriate to be used for statistic analysis using linear models below.

## Linear Models

### Model builder

This page allows you to biuld linear model by providing your own model equation by the following steps.

#### 1. Add Variables

Click the **Add Variables**, and add the variable(s) that you would like to include in your model. The variables are all from the table shown in the *Overview* page. If you are not sure what each variables are, you can see it in the *Overview* page. It must be indicated whether the variable is a factor (categorical) or a numeric variable. Click **Confirm** to finish adding variables.

#### 2. Input formula

The variables present in the formula must be confirmed in the previous step. If any special character present in the variable name, quote it using the \` (backquote, on the left top of your keyboard, below the `esc` key). Examples:

~ `Clinical Group`
~ `Clinical Group` + AGE
~ `Clinical Group` * AGE + SEX

Click the **Submit** button to submit the formula.

#### 3. Select coefficient

To perform statistic estimization using the provided formula, you need to select which coefficient to test. The selected coefficient will be used to compute p values. And the result table will be printed out.

### Box plot builder

Once the linear models were built, you can visualze the box plot by clicking on each row of the result table. The box plot is drawn using four variables.

#### x variable

The x axis variable, which should be the coefficient you chose in the linear model builder.

#### Column facet

The variable to split the plot vertically into two or more panels.

#### Row facet

The variable to split the plot horizontally into two or more panels.

#### Color Variable

The variable to give each point a color.


## Correlations.

This page allows you to build correlation tests against variables. Only **AGE** and **BRAAK** are included in here.