convert_aupr_to_ggplot <- function(i_y_line_threshold_signif, 
                                   prroc_object,
                                   str_filename_save)
{
          #################################################################################
          # function that takes in a PRROC object,
          # AUPR curve significance threshold,
          # and filename
          # It then saves a AUPR plot in ggplot pretty format
          #################################################################################
    
          require(ggplot2)
          require(PRROC)
          require(grid)
          #################################################################################
          # convert to ggplot
          theme_set(theme_classic())
          y <- as.data.frame(prroc_object$curve)
          gp <- ggplot(y, aes(y$V1, y$V2))
          gp <- gp + geom_path() 
          gp <- gp + xlim(0,1)
          gp <- gp + ylim(0,1)
          gp <- gp + geom_hline(yintercept=i_y_line_threshold_signif, linetype="dashed")
          gp <- gp + xlab("Recall")
          gp <- gp + ylab("Precision")
          
          
          ############
          # add text
          my_text <- paste("AUC = ", as.character(round(prroc_object$auc.integral, digits = 2)
                                                  ))
          my_grob = grid.text(my_text,  gp=gpar(col="black", fontsize=14, fontface="bold"))
          gp <- gp + annotation_custom(my_grob)
          
          
          gp
          ggsave(filename = str_filename_save)
          #################################################################################      
          
          
}  


