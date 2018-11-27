
svyplot <- function(fit = CellMean, output = "all"){
    library(ggpubr)
    library(tidyverse)
    library(Rmisc)
    
    data <- fit$model[,c(1,2)]
    names(data) <- c("y", "x")
    data %>%
    group_by(x) %>%
    dplyr::summarise(
    N = n(),
    Mean = mean(y),
    SD = sd(y)
    ) -> dd
    
    
    errorplot <- ggplot(dd, aes(x=dd$x, y=dd$Mean, fill=as.factor(dd$x))) +
    geom_col(position=position_dodge(0.1)) +
    geom_errorbar(aes(ymin=dd$Mean-dd$SD, ymax=dd$Mean+dd$SD), width=.4, position=position_dodge(0.1)) +
    geom_point(position=position_dodge(0.1)) +
    labs(x = names(dd$x), y = names(dd$Mean), fill = names(dd$x), title = "Errorbarplot") +
    theme(legend.text = element_text(size=5), legend.position="right") +
    guides(col = guide_legend(nrow = 1)) +
    theme_classic()+ ggpubr::rotate_x_text(45)
    
    boxplot <- ggplot(data, aes(x=data$x, y=data$y, fill=as.factor(data$x))) +
    geom_boxplot() +
    labs(x = names(data$x), y = names(data$y), fill = names(data$x), title = "Boxplot") +
    theme(legend.text = element_text(size=5), legend.position="right")+
    guides(col = guide_legend(nrow = 1))+
    theme_classic()+ ggpubr::rotate_x_text(45) +
    scale_y_continuous(limits=c(-0.2,1.2))
    
    res <- rstudent(fit)
    
    point <- ggplot() +
    geom_point(aes(x = c(1:length(data$y)), y = res)) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_hline(yintercept = c(-2,2), color = "red", size = c(0.4)) +
    labs(title = "Covariate residuals plot", x = "x", y = "residuals")+
    theme_classic()
    
    
    qqplot <- ggqqplot(data = res)
    
    
    if(output == "error"){
        multiplot(point, qqplot, cols = 2)
    }else if(output == "all"){
        multiplot(errorplot,boxplot,  point, qqplot, cols = 2)
    }else if (output == "table"){
        dd
    }else if(output == "errorbar"){
        multiplot(errorplot,boxplot, cols = 2)
    }
    
}
