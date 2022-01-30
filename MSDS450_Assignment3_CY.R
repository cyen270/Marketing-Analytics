library(AlgDesign)

attr_names <- c('Brand', 'Checking', 'Savings', 'Invest', 'Crypto', 'Credit',
                'Debit', 'Rewards', 'MobilePay', 'Application', 'Checkbook',
                'CustService', 'AcctOpen', 'Fraud', 'Fees')

full_fact <- gen.factorial(levels=2, nVars=15, center=TRUE, varNames=attr_names)


# Design with only main effects
fract_main <- optFederov(~., data=full_fact, nTrials=20, nRepeats=100)
fract_main_eval <- eval.design(~., fract_main$design, confounding=TRUE)

# Design with interaction terms
full_fact2
full_fact2$`BrandChecking` <- full_fact2$Brand * full_fact$Checking
full_fact2$`BrandCredit` <- full_fact2$Brand * full_fact$Credit
full_fact2$`CreditFee` <- full_fact2$Credit * full_fact$Fees

fract_interaction <- optFederov(~., data=full_fact2, nTrials=20, nRepeats=20)
fract_interaction_eval <- eval.design(~., fract_interaction$design, confounding=TRUE)

dat<-gen.factorial(2,3,center=TRUE)
desFM<-optFederov(~.^2,dat,nTrials=8)
