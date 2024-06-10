# Health Research Council of New Zealand trial of randomised funding

R code to run the analysis of a randomised trial of funding. The R files are numbered in running order, with files starting with “0” for data reading, through to files starting with “5” for the analysis and presentation. The Bayesian models were run using the nimble package.

The data cannot be shared publicly as we consented applicants on the understanding that their data would not be shared with other researchers.

The folder `simulation` contains the code to run a simulation that compares two study design: regression dicontinuity and a randomised controlled trial.

### Version information

```
R version 4.3.1 (2023-06-16 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_Australia.utf8  LC_CTYPE=English_Australia.utf8   
[3] LC_MONETARY=English_Australia.utf8 LC_NUMERIC=C                      
[5] LC_TIME=English_Australia.utf8    

time zone: Australia/Brisbane
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] gridExtra_2.3   survminer_0.4.9 ggpubr_0.6.0    coda_0.19-4     ggmcmc_1.5.1.1 
 [6] ggplot2_3.5.1   nimble_1.1.0    mitools_2.4     broom_1.0.6     survival_3.5-5 
[11] flextable_0.9.6 janitor_2.2.0   stringr_1.5.0   tidyr_1.3.0     dplyr_1.1.2    

loaded via a namespace (and not attached):
 [1] DBI_1.1.3               rlang_1.1.1             magrittr_2.0.3         
 [4] snakecase_0.11.0        compiler_4.3.1          systemfonts_1.0.4      
 [7] vctrs_0.6.3             httpcode_0.3.0          pkgconfig_2.0.3        
[10] crayon_1.5.2            fastmap_1.1.1           backports_1.4.1        
[13] ellipsis_0.3.2          labeling_0.4.2          KMsurv_0.1-5           
[16] utf8_1.2.3              promises_1.2.0.1        rmarkdown_2.22         
[19] pracma_2.4.2            nloptr_2.0.3            ragg_1.2.5             
[22] purrr_1.0.1             xfun_0.39               jsonlite_1.8.5         
[25] later_1.3.1             reshape_0.8.9           uuid_1.1-0             
[28] parallel_4.3.1          R6_2.5.1                stringi_1.7.12         
[31] RColorBrewer_1.1-3      rlist_0.4.6.2           GGally_2.1.2           
[34] car_3.1-2               boot_1.3-28.1           lubridate_1.9.2        
[37] numDeriv_2016.8-1.1     Rcpp_1.0.10             knitr_1.43             
[40] zoo_1.8-12              httpuv_1.6.11           Matrix_1.5-4.1         
[43] splines_4.3.1           igraph_1.5.0            timechange_0.2.0       
[46] tidyselect_1.2.0        rstudioapi_0.14         abind_1.4-5            
[49] yaml_2.3.7              codetools_0.2-19        curl_5.0.1             
[52] lattice_0.21-8          tibble_3.2.1            plyr_1.8.8             
[55] shiny_1.7.4             withr_2.5.0             askpass_1.1            
[58] evaluate_0.21           zip_2.3.0               xml2_1.3.4             
[61] survMisc_0.5.6          pillar_1.9.0            TeachingDemos_2.12     
[64] carData_3.0-5           generics_0.1.3          munsell_0.5.0          
[67] scales_1.3.0            minqa_1.2.5             xtable_1.8-4           
[70] glue_1.6.2              gdtools_0.3.7           tools_4.3.1            
[73] gfonts_0.2.0            data.table_1.14.8       lme4_1.1-33            
[76] ggsignif_0.6.4          grid_4.3.1              colorspace_2.1-0       
[79] nlme_3.1-162            cli_3.6.1               km.ci_0.5-6            
[82] textshaping_0.3.6       officer_0.6.5           fontBitstreamVera_0.1.1
[85] fansi_1.0.4             gtable_0.3.3            rstatix_0.7.2          
[88] digest_0.6.31           fontquiver_0.2.1        crul_1.4.0             
[91] farver_2.1.1            htmltools_0.5.5         lifecycle_1.0.3        
[94] httr_1.4.6              season_0.3.15           mime_0.12              
[97] fontLiberation_0.1.0    openssl_2.0.6           MASS_7.3-60 
```
