simulation_beta_sse
================
xichengxie
2023-02-22

``` r
forward_beta_sse<-read.csv("output_analysis_files/sse_forward.csv") %>% 
  select(-1) %>% 
  mutate(methods="forward")

lasso_beta_sse<-read.csv("output_analysis_files/sse_lasso.csv") %>% 
  select(-1) %>%
  mutate(methods="lasso")

data_sse<-rbind(forward_beta_sse,lasso_beta_sse) %>% 
  mutate(cor=factor(cor))
```

\#boxplot and violin plot

``` r
sse_boxplot<-data_sse %>% ggplot(aes(x=cor,y=beta_sse))+
  geom_boxplot(alpha = 0.5, width = 0.7,aes(fill=methods))+
  labs(xlab="correlation value",ylab="SSE",
       title = "SSE boxplot in different correlation values between WBC and strong predictors")
sse_boxplot
```

![](simulation_beta_sse_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
mse_plot<-data_sse %>% group_by(cor,methods) %>% 
  mutate(mse=mean(beta_sse)) %>% 
  ggplot(aes(x=cor,y=mse,color=methods))+
  geom_point()+
  ylim(0,0.03)+
  geom_line(aes(group=methods,color=methods))
mse_plot
```

![](simulation_beta_sse_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->
