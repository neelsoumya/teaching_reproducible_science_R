################################################################################################
# Example code to perform logistic regression
#   	and plot ROC curve and 
#	    precision recall curve	
#     and calculate AUC and AUPR
#
#     1. Performs test and train split
#     2. Performs cross-validation and picks best model
#     3. Uses the best model to predict and generate ROC and AUPR curves on test set
#
# Acknowledgements:
#	Adapted from
#	https://stackoverflow.com/questions/18449013/r-logistic-regression-area-under-curve
#	answer by user wush978	
# and
# https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
# answer by user arun
# 
# Usage: 
#     nohup R --no-save < logistic_regression_roc_curve_withCV.R
#
# Installation:
#     install.packages("pROC")
#     install.packages("precrec")
#     install.packages("PRROC")
#
################################################################################################

############################
# Load libraries
############################
library(pROC)
library(precrec)
library(PRROC)
library(boot)
library(ggplot2)
# LIBRARY_PREFIX <- "https://egret.psychol.cam.ac.uk/rlib/"
# source(paste0(LIBRARY_PREFIX, "cris_common.R"))

source("https://raw.githubusercontent.com/neelsoumya/rlib/master/cris_common.R")



###############################
# Load synthetic data
###############################
# df_metagene_score_final is a data frame with two columns:
#	flag_yes_no: target (1 or 0)
#	metagene_score: continuous variable

#setwd("~/logistic_regression_roc_curve")
df_metagene_score_final = read.csv('metagene_score.csv', 
                                  sep = ',', header = TRUE, 
                                  stringsAsFactors=FALSE, na.strings="..")


########################
# Test train split
########################
#set.seed(1)
TRAIN = sample(c(TRUE,FALSE),
                  nrow(df_metagene_score_final),
                  replace = TRUE)

TEST = (!TRAIN)

df_metagene_score_final_TRAIN = df_metagene_score_final[TRAIN,]
df_metagene_score_final_TEST  = df_metagene_score_final[TEST,]

# TODO
# cris$visualize_fixed_effects_from_lmer(name of lmer or glmer model)
# cris$fixed_effects_from_lmer(name of lmer or glmer model)

###############################
# Train logistic regression
#     on training set
###############################

########################
# Fit glm
########################
# call repeatedly
# for a fixed training set

i_fold_cv = 5          # number of folds for cross-validation
i_num_repeat_cv = 100  # number of times to perform repeated cross-validation
f_cv_error_best = -1   # variable to store best cross-validation error
glm_object_best = NULL # variable to store best glm() object for best cross-validation error

for (i_temp_counter in seq(1,i_num_repeat_cv)) 
{
        
        ########################
        # create GLM 
        #  on TRAINING SET
        #  or VALIDATION SET
        ########################
        mylogit <- glm(flag_yes_no ~ metagene_score, 
                       data = df_metagene_score_final_TRAIN, 
                       family = "binomial")
  
        ########################
        # AIC score
        ########################
        mylogit$aic
        
        # cost function for logistic
        cost <- function(r, pi=0) mean(abs(r-pi)>0.5)
        cost_classification <- function(r, pi) mean(abs(r-pi) > 0.5)
        cost_negloglikelihood <- function(r, pi) -sum(r*log(pi)+(1-r)*log(1-pi)   )
        
        ########################
        # cross-validate
        #   on TRAINING set
        #  or VALIDATION SET
        ########################
        cv_err <- cv.glm(data = df_metagene_score_final_TRAIN,
                         K = i_fold_cv,
                         cost = cost,
                         glmfit = mylogit
        )
        
        ########################
        # CV error
        #  on VALIDATION SET
        #  or TRAINING SET
        ########################
        cv_err$delta[1]
        
        # if first time then store cv error and glm object
        if (i_temp_counter == 1)
        {
            f_cv_error_best = cv_err$delta[1]
            glm_object_best = mylogit
        }
        
        # if new iteration has better error then store that
        if (cv_err$delta[1] < f_cv_error_best )
        {
            f_cv_error_best = cv_err$delta[1]
            glm_object_best = mylogit
          
        }
        
        
        
}
# end for loop


########################
# Best estimates
########################
f_cv_error_best
glm_object_best


#######################################
# Check linear model distributions
#######################################
miscstat$check_distribution(model = glm_object_best)


#######################################
# Visualize parameter distributions
# holds for linear mixed effects models lmer()
#######################################
# cris$visualize_fixed_effects_from_lmer(lmer_result = glm_object_best)
# cris$fixed_effects_from_lmer(lmer_result = glm_object_best)


###############################
# predict on TEST set
#   using best model
###############################
prob = predict(glm_object_best, 
               type=c("response"),
               newdata = df_metagene_score_final_TEST)

df_metagene_score_final_TEST$prob = prob


###############################
# Generate ROC curves
#   on TEST SET
###############################
g <- pROC::roc(flag_yes_no ~ prob, data=df_metagene_score_final_TEST)

plot(g)

cat("AUC is:", g$auc)


############################
# Generate 
# Precision recall curve
#     on TEST SET
############################
# the precision recall curve 
# is better suited for cases in which there are class imbalances.
mmdata_flag = mmdata(df_metagene_score_final_TEST$metagene_score,
                     df_metagene_score_final_TEST$flag_yes_no)
smcurves <- evalmod(mmdata_flag, raw_curves = TRUE)

plot(smcurves, raw_curves = FALSE)

############################
# even better AUC and AUPR 
# curves with areas reporred
#   on TEST SET
############################

fg <- prob[df_metagene_score_final_TEST$flag_yes_no == 1]
bg <- prob[df_metagene_score_final_TEST$flag_yes_no == 0]

# ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
plot(roc)

# PR Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = TRUE)
plot(pr)

cat("AUPR is:", pr$auc.integral)


##################################
# Better AUPR plot using ggplot
##################################
source("convert_aupr_to_ggplot.R")

i_y_line_threshold_signif_aupr = length(which(df_metagene_score_final_TEST$flag_yes_no == 1))/(length(which(df_metagene_score_final_TEST$flag_yes_no == 1)) + length(which(df_metagene_score_final_TEST$flag_yes_no == 0)))

convert_aupr_to_ggplot(i_y_line_threshold_signif=i_y_line_threshold_signif_aupr, 
                                   prroc_object=pr,
                                   str_filename_save="aupr_ggplot_CV.pdf")



##################################
# get feature coefficient plot
#  and feature importance plot
##################################
summary(glm_object_best)

as.table((glm_object_best$coefficients))


idx_order = order(glm_object_best$coefficients, decreasing = TRUE)

df_intercept_active = as.data.frame(glm_object_best$coefficients)
colnames(df_intercept_active)[1] <- "coeff"
df_intercept_active$gene_name = rownames(df_intercept_active)

#df_intercept_active = sqldf("select * from df_intercept_active
#                           where gene_name not in ('(Intercept)') ")

#theme_set(theme_classic())
theme_set(theme_gray())
gp <- ggplot(data=df_intercept_active, aes(x=gene_name, y=coeff))
gp <- gp + geom_bar(stat = "identity")
gp <- gp + xlab("Feature Name")
gp <- gp + ylab("Coefficient")
gp <- gp + coord_flip()
gp
ggsave(filename = "feature_importance_plot.pdf", useDingbats=FALSE)

