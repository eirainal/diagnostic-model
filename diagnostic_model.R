library(caret)
set.seed(123)

# Model evaluation
dataset = read.table(file = "dataset.csv", header = TRUE, sep = ",",   
                     row.names = 1, stringsAsFactors = FALSE)
dataset$type = factor(dataset$type, order = TRUE, levels = c("Heal", "OSCC"))

# Prediction results
pred <- predict(model, newdata = dataset, type = "vote")

# Model accuracy assessment: ROC curve and calculation of AUC value
roc.info <- roc(dataset$type, 
                pred[, 1],  # Extract the corresponding prediction metric from the random forest model
                plot = TRUE, 
                legacy.axes = TRUE, 
                percent = FALSE, 
                xlab = "False positive percentage", 
                ylab = "True positive percentage",
                col = "#4daf4a", 
                lwd = 4, 
                ci = TRUE,
                print.auc = TRUE)

# Extract the data of interest from roc.info
roc.df <- data.frame(
  tpp = roc.info$sensitivities,  # tpp = True positive rate = Sensitivity
  fpp = (1 - roc.info$specificities),  # fpp = False positive rate = 1 - Specificity
  thresholds = roc.info$thresholds  # thresholds = Cutoff values
)

head(roc.df)

tail(roc.df)

roc.df[roc.df$tpp > 0.6 & roc.df$tpp < 0.8, ]

cutoff <- roc.df$thresholds[which.max(roc.df$tpp - roc.df$fpp)] 
cutoff

# Plot ROC using ggplot2
library(ggplot2)
roc_obj <- roc(dataset$type, pred[, 1])
auc <- auc(roc_obj) %>% as.numeric %>% round(3)
ci <- ci.auc(roc_obj) %>% as.numeric %>% round(3) 
cutoff <- roc.df[which.max(roc.df$tpp - roc.df$fpp), 1:2] %>% as.numeric %>% round(3) 

ggroc(roc_obj, legacy.axes = TRUE, color = "#2878b5", lwd = 1.5) +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color = "grey", linetype = "dashed", lwd = 1) +
  geom_point(aes(x = cutoff[2], y = cutoff[1]), color = "black", size = 3, shape = 16) +
  geom_text(x = 0.67, y = 0.39, label = paste0("AUC: ", auc, "\n",
                                               "95% CI: (", ci[1], ", ", ci[3], ")\n",
                                               "Sensitivity: ", cutoff[1], "\n",
                                               "Specificity: ", 1 - cutoff[2]), 
            size = 4, color = "black", vjust = 0.5, hjust = 0.5) +
  labs(title = "ROC Curve", x = "1 - Specificity", y = "Sensitivity") +
  # scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  # scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
        # panel.grid.major = element_blank(),
        legend.title = element_blank(),
        # axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        plot.title = element_blank())
ggsave("test.pdf", width = 5, height = 5)



