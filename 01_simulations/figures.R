rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(tidyverse)
library(ggh4x)

files <- list.files()
files_subset_DMW <- files[startsWith(files, "DMW_results_individual")] # 12 ohne v2

list(file = "DMW_results_individual_SV1FJ_M390_lam0.058_sig1_alpha-1.386.rds", M = 390, lambda = 0.058, sigma = 1, alpha = -1.386)
list(file = "DMW_results_individual_SV1FJ_M78_lam0.058_sig1_alpha-1.386.rds", M = 78, lambda = 0.058, sigma = 1, alpha = -1.386)


"DMW_results_individual_SV1FJ_M390_lam0.058_sig1_alpha-1.386.rds"
# List of files to read, with associated parameters
file_list <- list(
  list(file = "DMW_results_individual_SV1FJ_M78_lam0.118_sig1_alpha-0.1.rds", M = 78, lambda = 0.118, sigma = 1, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M390_lam0.118_sig1_alpha-0.1.rds", M = 390, lambda = 0.118, sigma = 1, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M78_lam0.058_sig1_alpha-0.00137.rds", M = 78, lambda = 0.058, sigma = 1, alpha = -0.00137),
  list(file = "DMW_results_individual_SV1FJ_M390_lam0.058_sig1_alpha-0.00137.rds", M = 390, lambda = 0.058, sigma = 1, alpha = -0.00137),
  list(file = "DMW_results_individual_SV1F_M78_alpha-0.1.rds", M = 78, lambda = NA, sigma = NA, alpha = -0.1),
  list(file = "DMW_results_individual_SV1F_M390_alpha-0.1.rds", M = 390, lambda = NA, sigma = NA, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M78_lam0.058_sig1_alpha-0.1.rds", M = 78, lambda = 0.058, sigma = 1, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M78_lam0.058_sig2_alpha-0.1.rds", M = 78, lambda = 0.058, sigma = 2, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M390_lam0.058_sig1_alpha-0.1.rds", M = 390, lambda = 0.058, sigma = 1, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M390_lam0.058_sig2_alpha-0.1.rds", M = 390, lambda = 0.058, sigma = 2, alpha = -0.1),
  list(file = "DMW_results_individual_SV1FJ_M390_lam0.058_sig1_alpha-1.386.rds", M = 390, lambda = 0.058, sigma = 1, alpha = -1.386),
  list(file = "DMW_results_individual_SV1FJ_M78_lam0.058_sig1_alpha-1.386.rds", M = 78, lambda = 0.058, sigma = 1, alpha = -1.386),
  list(file = "DMW_results_individual_SV2F_M78.rds", M = 78, lambda = NA, sigma = NA, alpha = NA),
  list(file = "DMW_results_individual_SV2F_M390.rds", M = 390, lambda = NA, sigma = NA, alpha = NA)
)


# Read each file, add the metadata, and bind them into a single data frame
combined_data <- file_list %>%
  map_dfr(
    ~ readRDS(.x$file) %>% 
      mutate(
        M = .x$M, lambda = .x$lambda, sigma = .x$sigma, alpha = .x$alpha
      )
  )


# Display the resulting combined data frame
combined_data


results <-
  bind_rows(
    combined_data %>% 
      mutate(
        symbol = paste(symbol,"M",M,"lam",lambda, "al", alpha ,"sig",sigma, sep = "_"),
        #glue::glue("{symbol}_M{M}_lam{lambda}_sig{sigma}_alpha{alpha}")
      ),
  )  


labels <-
  tibble(
    symbol = unique(results$symbol),
    label = factor(
      unique(results$symbol),
      levels = unique(results$symbol),
      labels = c(
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.118, alpha == -0.1 )) ),
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.118, alpha == -0.1 )) ),
        
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058, alpha == -0.00137 )) ),
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058, alpha == -0.00137 )) ),
        
        expression(plain(SV1F) ~ (list(alpha == -0.1 )) ),
        expression(plain(SV1F) ~ (list(alpha == -0.1 )) ),
        
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058,  alpha == -0.1 ))  ),
        expression(plain(SV1FJ) ~ (list(sigma == 2, lambda == 0.058,  alpha == -0.1 ))  ),
        
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058, alpha == -0.1 )) ),
        expression(plain(SV1FJ) ~ (list(sigma == 2, lambda == 0.058, alpha == -0.1 )) ), 
        
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058, alpha == -1.386 )) ),
        expression(plain(SV1FJ) ~ (list(sigma == 1, lambda == 0.058, alpha == -1.386 )) ), 
        
        expression( plain(SV2F) ),
        expression( plain(SV2F) )
      )
    )
  )



results <- left_join(results, labels) %>% mutate(symbol = label)


breakpoints <- c(-Inf, -1.96, -1.64, 0, 1.64, 1.96, Inf)
labels <- c("<-1.96", "t<-1.64", "inconclusive", "inconclusive", "t>1.64", "t>1.96")
color_scale <- scale_color_manual(
  values = c(
    "<-1.96" = "#440154FF", 
    "t<-1.64" = "#31688EFF", 
    "inconclusive" = "grey", 
    "t>1.64" = "#35B779FF", 
    "t>1.96" = "#FDE725FF"
  ), 
  drop = FALSE, 
)

breakpoints <- c(-Inf, -1.96, 0, 1.96, Inf)
labels <- c("qlikeHAR sig. better", "inconclusive", "inconclusive", "mseHAR sig. better")
color_scale <- scale_color_manual(
  values = c(
    #"t<-2.33" = "#440154FF", 
    "qlikeHAR sig. better" = "blue", 
    "inconclusive" = "gray", 
    "mseHAR sig. better" = "coral"
    # "t>2.33" = "#FDE725FF"
  ), 
  drop = FALSE, 
)


infoinconc <- 
  bind_rows(
    results %>% 
      filter(qlike_p_value >.05) %>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="QLIKE",
        qHARbetter=ifelse(qlike_statistic<0, 1, 0)
      ) %>% 
      select(
        symbol,M,lambda, sigma, alpha, 
        win,horizon,model,lossfunction, qHARbetter, tstatgroups
      ),
    results %>% 
      filter(mse_p_value >.05)%>% 
      mutate(
        tstatgroups="inconclusive",
        lossfunction ="MSE", 
        qHARbetter=ifelse(mse_statistic<=0, 1, 0)
      ) %>% 
      select(
        symbol,M,lambda, sigma, alpha, 
        win,horizon,model,lossfunction, qHARbetter, tstatgroups
      )
  ) %>% 
  mutate(
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  )



plotdata <- 
  results %>% 
  pivot_longer(
    cols =c(qlike_statistic,mse_statistic), 
    values_to = "teststatistic", 
    names_to = "lossfunction"
  ) %>% 
  mutate(
    lossfunction = ifelse(lossfunction=="mse_statistic", "MSE", "QLIKE"),
    tstatgroups = cut(
      teststatistic, 
      breaks=breakpoints, 
      labels=labels, 
      include.lowest=TRUE
    )
  ) %>% 
  mutate(
    symbol = factor(
      symbol, 
      levels = c(sort(setdiff(unique(symbol), "panel DMW")), "panel DMW")
    ), 
    horizon = ifelse(
      horizon ==  "1day", 
      "a) one-day-ahead", 
      ifelse(
        horizon ==  "1week", 
        "b) one-week-ahead", 
        "c) one-month-ahead")
    )
  ) %>%
  arrange(symbol) 



plotdata %>% 
  ggplot(aes(y=fct_rev(symbol), x=factor(win), color=tstatgroups))+
  geom_point()+
  #geom_tile()+
  color_scale+
  ggh4x::facet_nested(M+model~horizon+lossfunction,scales = "free")+
  xlab("length of estimation window")+
  ylab("symbol")+
  theme_bw()+
  theme(
    legend.position = "bottom", 
    legend.title  = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  geom_point(
    infoinconc %>% filter(qHARbetter==0),
    mapping = aes(y=(symbol), x=factor(win)),
    shape="+", color="black"
  )+
  geom_hline(yintercept = 1.5, linetype = "solid", color = "black")+
  scale_y_discrete(labels = function(x) parse(text = x)) +
  ylab(element_blank())+
  theme(
    legend.position = "bottom", 
    legend.margin = margin(0, 0, 0, 0),     # Remove margins around the legend
    #legend.box.margin = margin(-20, 0, 0, 0), 
    legend.title = element_blank(),
    legend.key.size = unit(6, "mm"),
    legend.text = element_text(
      colour = "black",
      size = 11
    ),
    legend.spacing.x = unit(1, "mm"),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust=1,
      colour = "black",
      size = 11),
    axis.text.y = element_text(
      colour = "black",
      size = 11
    ),
    strip.text = element_text(
      size = 11#, face = "bold"
    ),
    axis.title=element_text(
      size=11
    ),
    #  strip.text.x = element_text(margin = margin(.5,.5,.5,.5, "mm")), 
    #   strip.text.y = element_text(margin = margin(.5,.5,.5,.5, "mm"))
  )

ggsave("fig_sim_results.pdf", width = 12, height = 10)
ggsave("/Users/mp/Library/CloudStorage/Dropbox/Apps/Overleaf/qlikeHAR/fig/fig_sim_results.pdf", width = 12, height = 10)

