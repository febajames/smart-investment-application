library(dplyr)
library(formattable)
# A function to run the logistic model


risk_propensity <- function(model_data, sample_data){
  
  
  logistic.regressor <- glm(loan_status ~ grade + dti + open_acc + inq_last_6mths + annual_inc + 
                              home_ownership + purpose + loan_amnt + int_rate + revol_bal + 
                              revol_util + emp_dur, 
                            family = "binomial", data = model_data)
  repayment_probability <- predict(logistic.regressor, newdata = sample_data, type = "response")
  # round the probabilitites to 4 places
  repayment_probability <- round(repayment_probability,4) * 100
  
  # choose only the columns to be displayed
  disp_dt <- sample_data[,c("ID","grade","sub_grade","int_rate","purpose", "loan_amnt","emp_dur",
                            "annual_inc")]
  
  outpt <- cbind(disp_dt, repayment_probability)
  
    
  outpt <- outpt[order(-repayment_probability),] 
  return(as.datatable(formattable(outpt, list(repayment_probability = normalize_bar("lime",0.2)))))
  #return(outpt)
  
}