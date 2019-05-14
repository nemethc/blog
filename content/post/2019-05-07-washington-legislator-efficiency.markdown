---
title: Analyzing State Legislator Effectiveness, Part One
subtitle: Empirical Bayesian Shrinkage towards a Beta Prior
author: Corban
date: '2019-05-07'
slug: washington-legislator-efficiency
categories:
  - R
tags: []
image:
  caption: ''
  focal_point: ''
---

Washington State's legislative session recently concluded. The Democrats, who for the second year in a row controlled both the House and the Senate, celebrated success with the passage of signature legislation such as a raise in the smoking age, a public option for health insurance, and environmental protections. However, those big-name bills don't tell the whole story. For this analysis, I wanted to dig in to the over 2500 bills introduced this session by members. I'm also working through David Robinson's *Introduction to Empirical Bayes*, which is a fantastic applied reference to my trip down Bayesian Lane this academic quarter. I equated the baseball "batting average" metric used by Robinson throughout the text to a legislative batting average, being the number of bills a member prime sponsored divided by the total number of bills they sponsored. Emperical Bayesian shrinkage was appropriate in this case, as some members introduced relatively bills that had a higher likelihood of success, without giving credence to their overall numerical significance. Let's see how it looks.  


```r
library(tidyverse)
library(here)
library(stats4)
library(VGAM)
library(kableExtra)
library(plotly)
library(hrbrthemes)
library(widgetframe)

bill_data <- read_csv(here("static/data/session_data.csv"))
party_data <- read_csv(here("static/data/leg_rosters.csv"))


bill_data <- bill_data %>%
  separate(Bill, c("prefix", "bill_number"), sep = " ") %>% #seperate e.g. HB 1065 into components
  mutate(billpassed = if_else(Status == "Del to Gov" | startsWith(Status, "C"),"passed", "not_passed")) %>% #filter for bills that passed 
  distinct() %>% #removes house of origin duplicates
  left_join(party_data, by = c("Original Sponsor" = "Member Name")) %>% #join by party affiliation
  select(bill_number, Status, Title,`Original Sponsor`, billpassed, Party) %>% 
  filter(`Original Sponsor` != "People of the State of Washington") %>%  #not including initiatives, sorry Tim Eyman
  mutate(Party = as.factor(Party))

sponsor_record <- bill_data %>% 
  count(`Original Sponsor`, billpassed, Party) %>% 
  spread(billpassed, n) %>% 
  mutate(passed = replace_na(passed, 0),
         not_passed = replace_na(not_passed, 0),
         tot_bills = passed + not_passed,
         pct_passed = passed / (tot_bills))

billsgraph <- ggplot(sponsor_record, aes(x = passed, y = not_passed, color = Party, label = `Original Sponsor`))+
  geom_jitter() +
  labs(title = "Overall Bill Passage",
       y = "Bills not passing",
       x = "Bills passing") +
  theme_modern_rc() +
  scale_color_manual(breaks = c("D", "R"),
                        values=c("blue", "red"))
  

g <- ggplotly(billsgraph)
frameWidget(g)
```

<!--html_preserve--><div id="htmlwidget-581539a7e28dc352716c" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-581539a7e28dc352716c">{"x":{"url":"/post/2019-05-07-washington-legislator-efficiency_files/figure-html//widgets/widget_unnamed-chunk-1.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Intuition generally holds up here. The minority party, in this case Republicans, generally passes fewer bills, with a higher volume of bills not passing. However, ranking members from most to fewest bills passed doesn't give us the true picture of effectiveness, because the total number of bills by a given member varies accross the board. In order to get a more complete picture, we can use a Bayesian prior of the overall distribution, and *shrink* the ratio of passes to failures. 

Here's a quick histogram of the distribution of proportion of bills passed.


```r
#prior estimation
hist(sponsor_record$pct_passed)
```

<img src="/post/2019-05-07-washington-legislator-efficiency_files/figure-html/unnamed-chunk-2-1.png" width="672" />

Based on our distribution, we can develop estimates of alpha and beta based on the technique outlined in *Empirical Bayes*. 


```r
# log-likelihood function
ll <- function(alpha, beta) {
x <- sponsor_record$passed
total <- sponsor_record$tot_bills
-sum(VGAM::dbetabinom.ab(x, total, alpha, beta, log = TRUE))
}

# maximum likelihood estimation
m <- mle(ll, start = list(alpha = 1, beta = 10), method = "L-BFGS-B", lower = c(0.0001, .1))
ab <- coef(m) 
alpha0 <- ab[1]
beta0 <- ab[2]

z <- seq(0,1,length=21)
beta_dist <- data.frame(cbind(z, dbeta(z,alpha0,beta0)))
colnames(beta_dist) <- c("x", "value")

bills_eb <- sponsor_record %>%
mutate(eb_estimate = (passed + alpha0) / (tot_bills + alpha0 + beta0),
       delta = pct_passed-eb_estimate) %>% 
  arrange(eb_estimate) %>% 
  mutate(`Original Sponsor` = as.factor(`Original Sponsor`))


g3 <- ggplot(bills_eb, aes(x = pct_passed, y = eb_estimate, label = `Original Sponsor`, label2 = passed, label3 = not_passed, color = tot_bills, shape = Party)) +
  geom_point() +
  scale_color_viridis_c(name = "Total Bills Introduced") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Overall Bill Passage",
       y = "Bayesian estimate",
       x = "Observed percentage of bills passed") +
  geom_abline(intercept = 0, slope = 1, color = "lightgray", alpha = .5) +
  theme_modern_rc()
  

g1 <- ggplotly(g3)
frameWidget(g1)
```

<!--html_preserve--><div id="htmlwidget-2856de4d248c66fc450d" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-2856de4d248c66fc450d">{"x":{"url":"/post/2019-05-07-washington-legislator-efficiency_files/figure-html//widgets/widget_unnamed-chunk-3.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->




