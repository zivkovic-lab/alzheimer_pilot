April 21, 2021
- Get CV, add histogram and box plot or CV
- add input$cv outlet to the calDE_val function
- FIX ME: boxplot title and DE table didn't match
- SOLUTION: set all input$cv = 1
   
April 22, 2021
- FIX ME: interaction session, boxplot title and DE table didn't match
- SOLUTION: `calDE_val()`, < :point_right: <=
   ```r
   data[[datatype]]$fdata$cv[is.na(data[[datatype]]$fdata$cv)] <- 1
   dataf <- subset_features(data[[datatype]], data[[datatype]]$fdata$cv<as.numeric(cv))
   ```

    ```r
   data[[datatype]]$fdata$cv[is.na(data[[datatype]]$fdata$cv)] <- 1
   dataf <- subset_features(data[[datatype]], data[[datatype]]$fdata$cv<=as.numeric(cv))
   ```
- Discussion
    1. age: we already balance the age
    2. sex are matched
    3. ethnicity: interesting, should be included
    4. age of year: throw away. It skewed with AD and ctrl. Maybe look at the distribution of age of samples between diagnosis. sialylated increase but non-sialylated decreased, which we don't know how to explain, maybe driven by AD
    5. ApoE: include this one
    6. BMI: APOD glycopeptides stands out
    7. Sex:diagnosis: include this one
    deglycosylated IgG are more proinflammatory.

| Intercept                  | ctrl, male                      | Baseline level of ctrl male cases |
|----------------------------|---------------------------------|-----------------------------------|
| SuperDxAD                  | AD, M-ctrl, M                   | Effect of AD for male             |
| adc_genderFemale           | F, ctrl-M, ctrl                 | Differences on ctrl subjects      |
| SuperDxAD:adc_genderFemale | (F, AD-M, AD)-(F, ctrl-M, ctrl) | Interaction                       |

- Modify the final model: "~SuperDx*adc_gender+adc_ethnicity2+apoe4"
