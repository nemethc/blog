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
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0     ✔ purrr   0.3.0
## ✔ tibble  2.1.1     ✔ dplyr   0.7.8
## ✔ tidyr   0.8.1     ✔ stringr 1.4.0
## ✔ readr   1.1.1     ✔ forcats 0.3.0
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(here)
```

```
## here() starts at /Users/corban_nemeth/GitHub/blog
```

```r
library(stats4)
library(VGAM)
```

```
## Loading required package: splines
```

```
## 
## Attaching package: 'VGAM'
```

```
## The following object is masked from 'package:tidyr':
## 
##     fill
```

```r
library(kableExtra)
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

```r
library(hrbrthemes)
library(widgetframe)
```

```
## Loading required package: htmlwidgets
```

```r
bill_data <- read_csv(here("static/data/session_data.csv"))
```

```
## Parsed with column specification:
## cols(
##   Bill = col_character(),
##   Flags = col_character(),
##   Title = col_character(),
##   Status = col_character(),
##   Veto = col_character(),
##   Date = col_character(),
##   `Original Sponsor` = col_character(),
##   `Committee Sponsor` = col_character(),
##   `Companion Bills` = col_character(),
##   `Last Roll Call` = col_character(),
##   `Latest Committee Recommendation` = col_character(),
##   `Secondary Sponsor` = col_character(),
##   `Sponsor (combined)` = col_character(),
##   `This Committee Recommendation` = col_character()
## )
```

```r
party_data <- read_csv(here("static/data/leg_rosters.csv"))
```

```
## Parsed with column specification:
## cols(
##   `Member Name` = col_character(),
##   Party = col_character(),
##   District = col_integer(),
##   Position = col_integer(),
##   Room = col_character(),
##   Phone = col_character(),
##   Email = col_character(),
##   Chamber = col_character(),
##   `Legislative Assistant` = col_character(),
##   `LA Email` = col_character(),
##   X11 = col_character(),
##   X12 = col_character()
## )
```

```r
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

<!--html_preserve--><div id="htmlwidget-ec0068d6cd00c4f6ec33" style="width:100%;height:480px;" class="widgetframe html-widget"></div>
<script type="application/json" data-for="htmlwidget-ec0068d6cd00c4f6ec33">{"x":{"url":"/post/2019-05-07-washington-legislator-efficiency_files/figure-html//widgets/widget_unnamed-chunk-1.html","options":{"xdomain":"*","allowfullscreen":false,"lazyload":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

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
  

ggplotly(g3)
```

<!--html_preserve--><div id="htmlwidget-13b53e41e3563791cf21" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-13b53e41e3563791cf21">{"x":{"data":[{"x":[0.0416666666666667,0.0645161290322581,0.0434782608695652,0.1,0,0.05,0.0526315789473684,0.119047619047619,0.107142857142857,0.125,0.0714285714285714,0.128205128205128,0.105263157894737,0.0909090909090909,0.137931034482759,0.142857142857143,0.142857142857143,0.125,0.125,0.153846153846154,0.148148148148148,0.15625,0.153846153846154,0.15,0.166666666666667,0.157894736842105,0.175,0.166666666666667,0.176470588235294,0.176470588235294,0.181818181818182,0.181818181818182,0.181818181818182,0.181818181818182,0.2,0.2,0.2,0.2,0.196078431372549,0.222222222222222,0.2,0.214285714285714,0.25,0.222222222222222,0.222222222222222,0.222222222222222,0.227272727272727,0.227272727272727,0.235294117647059,0.25,0.25,0.285714285714286,0.5,0.219512195121951,0.222222222222222,0.25,0.272727272727273,0.272727272727273,0.24,0.266666666666667,0.3,0.263157894736842,0.25,0.277777777777778,0.277777777777778,0.277777777777778,0.277777777777778,0.307692307692308,0.307692307692308,0.375,0.285714285714286,0.3125,0.428571428571429,0.428571428571429,0.4,0.357142857142857,0.333333333333333,0.384615384615385,0.384615384615385,0.5,0.5,0.35,0.4,0.375,0.354838709677419,0.5],"y":[0.128322643378053,0.129846465721108,0.130506215767035,0.136059989762822,0.136761681768058,0.13752679378217,0.140037905441878,0.150056617959735,0.151637250335192,0.15401760448772,0.154107175694161,0.156077564044678,0.158296977660818,0.163992749024436,0.164735878984426,0.16489958067806,0.16731927033302,0.167470548161074,0.167470548161074,0.16945239586218,0.169984978159853,0.172199543586299,0.172737000526255,0.173390107386931,0.176535806007533,0.176556049879758,0.18041421785834,0.181494415087772,0.181596509744574,0.183247924037709,0.185375212853577,0.185375212853577,0.185375212853577,0.185375212853577,0.189425598869723,0.190467056866366,0.190467056866366,0.191321764189311,0.192117286394975,0.193656937788633,0.19400794916652,0.194294209934322,0.198081632906071,0.198438423606339,0.201848705769081,0.201848705769081,0.202008718952984,0.202008718952984,0.202199056447242,0.202429240764119,0.202429240764119,0.202713247590767,0.203072453561128,0.20411683464567,0.204403652940051,0.206104979198293,0.206757676682719,0.206757676682719,0.208492039318622,0.210164777499675,0.211275262337646,0.213074194317639,0.216468685316201,0.217037090222194,0.217037090222194,0.217037090222194,0.217037090222194,0.218783864378897,0.218783864378897,0.220929743425447,0.223183036185087,0.225422194716902,0.226095600698143,0.226095600698143,0.233124925805569,0.234481244174483,0.235635756838049,0.239289410031607,0.239289410031607,0.243777853944823,0.243777853944823,0.245116734596451,0.249560218766292,0.262175111700645,0.264642999869114,0.283373841272731],"text":["pct_passed: 0.04166667<br />eb_estimate: 0.12832264<br />`Original Sponsor`: Appleton<br />passed:  1<br />not_passed: 23<br />tot_bills: 24<br />Party: D","pct_passed: 0.06451613<br />eb_estimate: 0.12984647<br />`Original Sponsor`: Mullet<br />passed:  2<br />not_passed: 29<br />tot_bills: 31<br />Party: D","pct_passed: 0.04347826<br />eb_estimate: 0.13050622<br />`Original Sponsor`: Hudgins<br />passed:  1<br />not_passed: 22<br />tot_bills: 23<br />Party: D","pct_passed: 0.10000000<br />eb_estimate: 0.13605999<br />`Original Sponsor`: Palumbo<br />passed:  5<br />not_passed: 45<br />tot_bills: 50<br />Party: D","pct_passed: 0.00000000<br />eb_estimate: 0.13676168<br />`Original Sponsor`: Sheldon<br />passed:  0<br />not_passed: 13<br />tot_bills: 13<br />Party: D","pct_passed: 0.05000000<br />eb_estimate: 0.13752679<br />`Original Sponsor`: Pollet<br />passed:  1<br />not_passed: 19<br />tot_bills: 20<br />Party: D","pct_passed: 0.05263158<br />eb_estimate: 0.14003791<br />`Original Sponsor`: Carlyle<br />passed:  1<br />not_passed: 18<br />tot_bills: 19<br />Party: D","pct_passed: 0.11904762<br />eb_estimate: 0.15005662<br />`Original Sponsor`: Wellman<br />passed:  5<br />not_passed: 37<br />tot_bills: 42<br />Party: D","pct_passed: 0.10714286<br />eb_estimate: 0.15163725<br />`Original Sponsor`: Dolan<br />passed:  3<br />not_passed: 25<br />tot_bills: 28<br />Party: D","pct_passed: 0.12500000<br />eb_estimate: 0.15401760<br />`Original Sponsor`: Hasegawa<br />passed:  5<br />not_passed: 35<br />tot_bills: 40<br />Party: D","pct_passed: 0.07142857<br />eb_estimate: 0.15410718<br />`Original Sponsor`: Gregerson<br />passed:  1<br />not_passed: 13<br />tot_bills: 14<br />Party: D","pct_passed: 0.12820513<br />eb_estimate: 0.15607756<br />`Original Sponsor`: Kuderer<br />passed:  5<br />not_passed: 34<br />tot_bills: 39<br />Party: D","pct_passed: 0.10526316<br />eb_estimate: 0.15829698<br />`Original Sponsor`: Van De Wege<br />passed:  2<br />not_passed: 17<br />tot_bills: 19<br />Party: D","pct_passed: 0.09090909<br />eb_estimate: 0.16399275<br />`Original Sponsor`: Sells<br />passed:  1<br />not_passed: 10<br />tot_bills: 11<br />Party: D","pct_passed: 0.13793103<br />eb_estimate: 0.16473588<br />`Original Sponsor`: Darneille<br />passed:  4<br />not_passed: 25<br />tot_bills: 29<br />Party: D","pct_passed: 0.14285714<br />eb_estimate: 0.16489958<br />`Original Sponsor`: Blake<br />passed:  5<br />not_passed: 30<br />tot_bills: 35<br />Party: D","pct_passed: 0.14285714<br />eb_estimate: 0.16731927<br />`Original Sponsor`: Reeves<br />passed:  4<br />not_passed: 24<br />tot_bills: 28<br />Party: D","pct_passed: 0.12500000<br />eb_estimate: 0.16747055<br />`Original Sponsor`: Ortiz-Self<br />passed:  2<br />not_passed: 14<br />tot_bills: 16<br />Party: D","pct_passed: 0.12500000<br />eb_estimate: 0.16747055<br />`Original Sponsor`: Tarleton<br />passed:  2<br />not_passed: 14<br />tot_bills: 16<br />Party: D","pct_passed: 0.15384615<br />eb_estimate: 0.16945240<br />`Original Sponsor`: Hunt<br />passed:  6<br />not_passed: 33<br />tot_bills: 39<br />Party: D","pct_passed: 0.14814815<br />eb_estimate: 0.16998498<br />`Original Sponsor`: Macri<br />passed:  4<br />not_passed: 23<br />tot_bills: 27<br />Party: D","pct_passed: 0.15625000<br />eb_estimate: 0.17219954<br />`Original Sponsor`: Randall<br />passed:  5<br />not_passed: 27<br />tot_bills: 32<br />Party: D","pct_passed: 0.15384615<br />eb_estimate: 0.17273700<br />`Original Sponsor`: Hobbs<br />passed:  4<br />not_passed: 22<br />tot_bills: 26<br />Party: D","pct_passed: 0.15000000<br />eb_estimate: 0.17339011<br />`Original Sponsor`: Conway<br />passed:  3<br />not_passed: 17<br />tot_bills: 20<br />Party: D","pct_passed: 0.16666667<br />eb_estimate: 0.17653581<br />`Original Sponsor`: Doglio<br />passed:  6<br />not_passed: 30<br />tot_bills: 36<br />Party: D","pct_passed: 0.15789474<br />eb_estimate: 0.17655605<br />`Original Sponsor`: Ryu<br />passed:  3<br />not_passed: 16<br />tot_bills: 19<br />Party: D","pct_passed: 0.17500000<br />eb_estimate: 0.18041422<br />`Original Sponsor`: Liias<br />passed:  7<br />not_passed: 33<br />tot_bills: 40<br />Party: D","pct_passed: 0.16666667<br />eb_estimate: 0.18149442<br />`Original Sponsor`: Valdez<br />passed:  2<br />not_passed: 10<br />tot_bills: 12<br />Party: D","pct_passed: 0.17647059<br />eb_estimate: 0.18159651<br />`Original Sponsor`: Saldaña<br />passed:  6<br />not_passed: 28<br />tot_bills: 34<br />Party: D","pct_passed: 0.17647059<br />eb_estimate: 0.18324792<br />`Original Sponsor`: Senn<br />passed:  3<br />not_passed: 14<br />tot_bills: 17<br />Party: D","pct_passed: 0.18181818<br />eb_estimate: 0.18537521<br />`Original Sponsor`: Mead<br />passed:  2<br />not_passed:  9<br />tot_bills: 11<br />Party: D","pct_passed: 0.18181818<br />eb_estimate: 0.18537521<br />`Original Sponsor`: Morgan<br />passed:  2<br />not_passed:  9<br />tot_bills: 11<br />Party: D","pct_passed: 0.18181818<br />eb_estimate: 0.18537521<br />`Original Sponsor`: Salomon<br />passed:  2<br />not_passed:  9<br />tot_bills: 11<br />Party: D","pct_passed: 0.18181818<br />eb_estimate: 0.18537521<br />`Original Sponsor`: Wylie<br />passed:  2<br />not_passed:  9<br />tot_bills: 11<br />Party: D","pct_passed: 0.20000000<br />eb_estimate: 0.18942560<br />`Original Sponsor`: Pettigrew<br />passed:  2<br />not_passed:  8<br />tot_bills: 10<br />Party: D","pct_passed: 0.20000000<br />eb_estimate: 0.19046706<br />`Original Sponsor`: Fitzgibbon<br />passed:  3<br />not_passed: 12<br />tot_bills: 15<br />Party: D","pct_passed: 0.20000000<br />eb_estimate: 0.19046706<br />`Original Sponsor`: Kloba<br />passed:  3<br />not_passed: 12<br />tot_bills: 15<br />Party: D","pct_passed: 0.20000000<br />eb_estimate: 0.19132176<br />`Original Sponsor`: Orwall<br />passed:  4<br />not_passed: 16<br />tot_bills: 20<br />Party: D","pct_passed: 0.19607843<br />eb_estimate: 0.19211729<br />`Original Sponsor`: Keiser<br />passed: 10<br />not_passed: 41<br />tot_bills: 51<br />Party: D","pct_passed: 0.22222222<br />eb_estimate: 0.19365694<br />`Original Sponsor`: Bergquist<br />passed:  2<br />not_passed:  7<br />tot_bills:  9<br />Party: D","pct_passed: 0.20000000<br />eb_estimate: 0.19400795<br />`Original Sponsor`: Rolfes<br />passed:  9<br />not_passed: 36<br />tot_bills: 45<br />Party: D","pct_passed: 0.21428571<br />eb_estimate: 0.19429421<br />`Original Sponsor`: Santos<br />passed:  3<br />not_passed: 11<br />tot_bills: 14<br />Party: D","pct_passed: 0.25000000<br />eb_estimate: 0.19808163<br />`Original Sponsor`: Billig<br />passed:  2<br />not_passed:  6<br />tot_bills:  8<br />Party: D","pct_passed: 0.22222222<br />eb_estimate: 0.19843842<br />`Original Sponsor`: Frame<br />passed:  4<br />not_passed: 14<br />tot_bills: 18<br />Party: D","pct_passed: 0.22222222<br />eb_estimate: 0.20184871<br />`Original Sponsor`: Frockt<br />passed:  6<br />not_passed: 21<br />tot_bills: 27<br />Party: D","pct_passed: 0.22222222<br />eb_estimate: 0.20184871<br />`Original Sponsor`: Kilduff<br />passed:  6<br />not_passed: 21<br />tot_bills: 27<br />Party: D","pct_passed: 0.22727273<br />eb_estimate: 0.20200872<br />`Original Sponsor`: Nguyen<br />passed:  5<br />not_passed: 17<br />tot_bills: 22<br />Party: D","pct_passed: 0.22727273<br />eb_estimate: 0.20200872<br />`Original Sponsor`: Wilson, C.<br />passed:  5<br />not_passed: 17<br />tot_bills: 22<br />Party: D","pct_passed: 0.23529412<br />eb_estimate: 0.20219906<br />`Original Sponsor`: Stanford<br />passed:  4<br />not_passed: 13<br />tot_bills: 17<br />Party: D","pct_passed: 0.25000000<br />eb_estimate: 0.20242924<br />`Original Sponsor`: Leavitt<br />passed:  3<br />not_passed:  9<br />tot_bills: 12<br />Party: D","pct_passed: 0.25000000<br />eb_estimate: 0.20242924<br />`Original Sponsor`: Slatter<br />passed:  3<br />not_passed:  9<br />tot_bills: 12<br />Party: D","pct_passed: 0.28571429<br />eb_estimate: 0.20271325<br />`Original Sponsor`: Thai<br />passed:  2<br />not_passed:  5<br />tot_bills:  7<br />Party: D","pct_passed: 0.50000000<br />eb_estimate: 0.20307245<br />`Original Sponsor`: Chopp<br />passed:  1<br />not_passed:  1<br />tot_bills:  2<br />Party: D","pct_passed: 0.21951220<br />eb_estimate: 0.20411683<br />`Original Sponsor`: Cleveland<br />passed:  9<br />not_passed: 32<br />tot_bills: 41<br />Party: D","pct_passed: 0.22222222<br />eb_estimate: 0.20440365<br />`Original Sponsor`: Takko<br />passed:  8<br />not_passed: 28<br />tot_bills: 36<br />Party: D","pct_passed: 0.25000000<br />eb_estimate: 0.20610498<br />`Original Sponsor`: Pellicciotti<br />passed:  4<br />not_passed: 12<br />tot_bills: 16<br />Party: D","pct_passed: 0.27272727<br />eb_estimate: 0.20675768<br />`Original Sponsor`: Hansen<br />passed:  3<br />not_passed:  8<br />tot_bills: 11<br />Party: D","pct_passed: 0.27272727<br />eb_estimate: 0.20675768<br />`Original Sponsor`: Springer<br />passed:  3<br />not_passed:  8<br />tot_bills: 11<br />Party: D","pct_passed: 0.24000000<br />eb_estimate: 0.20849204<br />`Original Sponsor`: McCoy<br />passed:  6<br />not_passed: 19<br />tot_bills: 25<br />Party: D","pct_passed: 0.26666667<br />eb_estimate: 0.21016478<br />`Original Sponsor`: Ormsby<br />passed:  4<br />not_passed: 11<br />tot_bills: 15<br />Party: D","pct_passed: 0.30000000<br />eb_estimate: 0.21127526<br />`Original Sponsor`: Morris<br />passed:  3<br />not_passed:  7<br />tot_bills: 10<br />Party: D","pct_passed: 0.26315789<br />eb_estimate: 0.21307419<br />`Original Sponsor`: Stonier<br />passed:  5<br />not_passed: 14<br />tot_bills: 19<br />Party: D","pct_passed: 0.25000000<br />eb_estimate: 0.21646869<br />`Original Sponsor`: Jinkins<br />passed:  8<br />not_passed: 24<br />tot_bills: 32<br />Party: D","pct_passed: 0.27777778<br />eb_estimate: 0.21703709<br />`Original Sponsor`: Chapman<br />passed:  5<br />not_passed: 13<br />tot_bills: 18<br />Party: D","pct_passed: 0.27777778<br />eb_estimate: 0.21703709<br />`Original Sponsor`: Kirby<br />passed:  5<br />not_passed: 13<br />tot_bills: 18<br />Party: D","pct_passed: 0.27777778<br />eb_estimate: 0.21703709<br />`Original Sponsor`: Lovick<br />passed:  5<br />not_passed: 13<br />tot_bills: 18<br />Party: D","pct_passed: 0.27777778<br />eb_estimate: 0.21703709<br />`Original Sponsor`: Peterson<br />passed:  5<br />not_passed: 13<br />tot_bills: 18<br />Party: D","pct_passed: 0.30769231<br />eb_estimate: 0.21878386<br />`Original Sponsor`: Lekanoff<br />passed:  4<br />not_passed:  9<br />tot_bills: 13<br />Party: D","pct_passed: 0.30769231<br />eb_estimate: 0.21878386<br />`Original Sponsor`: Sullivan<br />passed:  4<br />not_passed:  9<br />tot_bills: 13<br />Party: D","pct_passed: 0.37500000<br />eb_estimate: 0.22092974<br />`Original Sponsor`: Entenman<br />passed:  3<br />not_passed:  5<br />tot_bills:  8<br />Party: D","pct_passed: 0.28571429<br />eb_estimate: 0.22318304<br />`Original Sponsor`: Goodman<br />passed:  6<br />not_passed: 15<br />tot_bills: 21<br />Party: D","pct_passed: 0.31250000<br />eb_estimate: 0.22542219<br />`Original Sponsor`: Tharinger<br />passed:  5<br />not_passed: 11<br />tot_bills: 16<br />Party: D","pct_passed: 0.42857143<br />eb_estimate: 0.22609560<br />`Original Sponsor`: Paul<br />passed:  3<br />not_passed:  4<br />tot_bills:  7<br />Party: D","pct_passed: 0.42857143<br />eb_estimate: 0.22609560<br />`Original Sponsor`: Shewmake<br />passed:  3<br />not_passed:  4<br />tot_bills:  7<br />Party: D","pct_passed: 0.40000000<br />eb_estimate: 0.23312493<br />`Original Sponsor`: Lovelett<br />passed:  4<br />not_passed:  6<br />tot_bills: 10<br />Party: D","pct_passed: 0.35714286<br />eb_estimate: 0.23448124<br />`Original Sponsor`: Davis<br />passed:  5<br />not_passed:  9<br />tot_bills: 14<br />Party: D","pct_passed: 0.33333333<br />eb_estimate: 0.23563576<br />`Original Sponsor`: Cody<br />passed:  6<br />not_passed: 12<br />tot_bills: 18<br />Party: D","pct_passed: 0.38461538<br />eb_estimate: 0.23928941<br />`Original Sponsor`: Das<br />passed:  5<br />not_passed:  8<br />tot_bills: 13<br />Party: D","pct_passed: 0.38461538<br />eb_estimate: 0.23928941<br />`Original Sponsor`: Walen<br />passed:  5<br />not_passed:  8<br />tot_bills: 13<br />Party: D","pct_passed: 0.50000000<br />eb_estimate: 0.24377785<br />`Original Sponsor`: Callan<br />passed:  4<br />not_passed:  4<br />tot_bills:  8<br />Party: D","pct_passed: 0.50000000<br />eb_estimate: 0.24377785<br />`Original Sponsor`: Ramos<br />passed:  4<br />not_passed:  4<br />tot_bills:  8<br />Party: D","pct_passed: 0.35000000<br />eb_estimate: 0.24511673<br />`Original Sponsor`: Riccelli<br />passed:  7<br />not_passed: 13<br />tot_bills: 20<br />Party: D","pct_passed: 0.40000000<br />eb_estimate: 0.24956022<br />`Original Sponsor`: Robinson<br />passed:  6<br />not_passed:  9<br />tot_bills: 15<br />Party: D","pct_passed: 0.37500000<br />eb_estimate: 0.26217511<br />`Original Sponsor`: Fey<br />passed:  9<br />not_passed: 15<br />tot_bills: 24<br />Party: D","pct_passed: 0.35483871<br />eb_estimate: 0.26464300<br />`Original Sponsor`: Dhingra<br />passed: 11<br />not_passed: 20<br />tot_bills: 31<br />Party: D","pct_passed: 0.50000000<br />eb_estimate: 0.28337384<br />`Original Sponsor`: Pedersen<br />passed:  8<br />not_passed:  8<br />tot_bills: 16<br />Party: D"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":["rgba(60,92,138,1)","rgba(46,115,141,1)","rgba(61,88,138,1)","rgba(58,174,125,1)","rgba(68,53,120,1)","rgba(64,78,136,1)","rgba(64,74,136,1)","rgba(42,150,136,1)","rgba(54,105,140,1)","rgba(43,144,137,1)","rgba(68,57,124,1)","rgba(43,141,138,1)","rgba(64,74,136,1)","rgba(69,45,114,1)","rgba(51,109,141,1)","rgba(43,128,141,1)","rgba(54,105,140,1)","rgba(66,64,131,1)","rgba(66,64,131,1)","rgba(43,141,138,1)","rgba(55,102,140,1)","rgba(43,119,142,1)","rgba(57,98,139,1)","rgba(64,78,136,1)","rgba(43,131,140,1)","rgba(64,74,136,1)","rgba(43,144,137,1)","rgba(69,49,117,1)","rgba(43,125,141,1)","rgba(65,67,134,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,42,110,1)","rgba(67,60,127,1)","rgba(67,60,127,1)","rgba(64,78,136,1)","rgba(65,177,122,1)","rgba(70,38,107,1)","rgba(39,160,134,1)","rgba(68,57,124,1)","rgba(70,34,104,1)","rgba(65,71,135,1)","rgba(55,102,140,1)","rgba(55,102,140,1)","rgba(62,85,137,1)","rgba(62,85,137,1)","rgba(65,67,134,1)","rgba(69,49,117,1)","rgba(69,49,117,1)","rgba(70,29,100,1)","rgba(68,1,84,1)","rgba(42,147,137,1)","rgba(43,131,140,1)","rgba(66,64,131,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(59,95,139,1)","rgba(67,60,127,1)","rgba(69,42,110,1)","rgba(64,74,136,1)","rgba(43,119,142,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(68,53,120,1)","rgba(68,53,120,1)","rgba(70,34,104,1)","rgba(63,81,137,1)","rgba(66,64,131,1)","rgba(70,29,100,1)","rgba(70,29,100,1)","rgba(69,42,110,1)","rgba(68,57,124,1)","rgba(65,71,135,1)","rgba(68,53,120,1)","rgba(68,53,120,1)","rgba(70,34,104,1)","rgba(70,34,104,1)","rgba(64,78,136,1)","rgba(67,60,127,1)","rgba(60,92,138,1)","rgba(46,115,141,1)","rgba(66,64,131,1)"],"opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":["rgba(60,92,138,1)","rgba(46,115,141,1)","rgba(61,88,138,1)","rgba(58,174,125,1)","rgba(68,53,120,1)","rgba(64,78,136,1)","rgba(64,74,136,1)","rgba(42,150,136,1)","rgba(54,105,140,1)","rgba(43,144,137,1)","rgba(68,57,124,1)","rgba(43,141,138,1)","rgba(64,74,136,1)","rgba(69,45,114,1)","rgba(51,109,141,1)","rgba(43,128,141,1)","rgba(54,105,140,1)","rgba(66,64,131,1)","rgba(66,64,131,1)","rgba(43,141,138,1)","rgba(55,102,140,1)","rgba(43,119,142,1)","rgba(57,98,139,1)","rgba(64,78,136,1)","rgba(43,131,140,1)","rgba(64,74,136,1)","rgba(43,144,137,1)","rgba(69,49,117,1)","rgba(43,125,141,1)","rgba(65,67,134,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,42,110,1)","rgba(67,60,127,1)","rgba(67,60,127,1)","rgba(64,78,136,1)","rgba(65,177,122,1)","rgba(70,38,107,1)","rgba(39,160,134,1)","rgba(68,57,124,1)","rgba(70,34,104,1)","rgba(65,71,135,1)","rgba(55,102,140,1)","rgba(55,102,140,1)","rgba(62,85,137,1)","rgba(62,85,137,1)","rgba(65,67,134,1)","rgba(69,49,117,1)","rgba(69,49,117,1)","rgba(70,29,100,1)","rgba(68,1,84,1)","rgba(42,147,137,1)","rgba(43,131,140,1)","rgba(66,64,131,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(59,95,139,1)","rgba(67,60,127,1)","rgba(69,42,110,1)","rgba(64,74,136,1)","rgba(43,119,142,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(65,71,135,1)","rgba(68,53,120,1)","rgba(68,53,120,1)","rgba(70,34,104,1)","rgba(63,81,137,1)","rgba(66,64,131,1)","rgba(70,29,100,1)","rgba(70,29,100,1)","rgba(69,42,110,1)","rgba(68,57,124,1)","rgba(65,71,135,1)","rgba(68,53,120,1)","rgba(68,53,120,1)","rgba(70,34,104,1)","rgba(70,34,104,1)","rgba(64,78,136,1)","rgba(67,60,127,1)","rgba(60,92,138,1)","rgba(46,115,141,1)","rgba(66,64,131,1)"]}},"hoveron":"points","name":"D","legendgroup":"D","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.0256410256410256,0,0,0.032258064516129,0,0,0.0740740740740741,0.0769230769230769,0.0588235294117647,0.0588235294117647,0.0714285714285714,0.1,0,0,0.0833333333333333,0,0,0.0909090909090909,0.0909090909090909,0.0909090909090909,0.0909090909090909,0.117647058823529,0.125,0.1,0.153846153846154,0.111111111111111,0,0,0,0,0.15625,0.15,0.125,0.125,0.16,0,0.142857142857143,0.166666666666667,0.181818181818182,0.1875,0.2,0.2,0.25,0.25,0.222222222222222,0.333333333333333,0.333333333333333,0.230769230769231,0.25,0.238095238095238,0.272727272727273,0.4,0.333333333333333,0.307692307692308,0.272727272727273,0.666666666666667,0.304347826086957,0.6,0.444444444444444],"y":[0.0762037745316231,0.0916551493733236,0.09559632907598,0.114869073037997,0.134013658574081,0.136761681768058,0.138121250550625,0.140357405620947,0.145345659218644,0.145345659218644,0.154107175694161,0.155458450584551,0.155948541376014,0.159682289043422,0.160559589411424,0.163599210386715,0.163599210386715,0.163992749024436,0.163992749024436,0.163992749024436,0.163992749024436,0.164296791628176,0.167470548161074,0.167575935401799,0.16945239586218,0.171319202318042,0.172039273545526,0.172039273545526,0.172039273545526,0.172039273545526,0.172199543586299,0.173390107386931,0.175233522386696,0.175233522386696,0.175579598665467,0.176594518293888,0.179330894483391,0.183624466600599,0.184697885982195,0.186787763679683,0.189425598869723,0.191321764189311,0.192859415331479,0.192859415331479,0.193656937788633,0.197834212099512,0.197834212099512,0.198278318726187,0.202429240764119,0.205567259445508,0.206757676682719,0.212658143180791,0.215994673259224,0.218783864378897,0.219319551923773,0.223629150653497,0.232603819034882,0.237187609577829,0.238332408729815],"text":["pct_passed: 0.02564103<br />eb_estimate: 0.07620377<br />`Original Sponsor`: Walsh<br />passed:  2<br />not_passed: 76<br />tot_bills: 78<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.09165515<br />`Original Sponsor`: Young<br />passed:  0<br />not_passed: 37<br />tot_bills: 37<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.09559633<br />`Original Sponsor`: Shea<br />passed:  0<br />not_passed: 34<br />tot_bills: 34<br />Party: R","pct_passed: 0.03225806<br />eb_estimate: 0.11486907<br />`Original Sponsor`: O'Ban<br />passed:  1<br />not_passed: 30<br />tot_bills: 31<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.13401366<br />`Original Sponsor`: Short<br />passed:  0<br />not_passed: 14<br />tot_bills: 14<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.13676168<br />`Original Sponsor`: Kraft<br />passed:  0<br />not_passed: 13<br />tot_bills: 13<br />Party: R","pct_passed: 0.07407407<br />eb_estimate: 0.13812125<br />`Original Sponsor`: Fortunato<br />passed:  2<br />not_passed: 25<br />tot_bills: 27<br />Party: R","pct_passed: 0.07692308<br />eb_estimate: 0.14035741<br />`Original Sponsor`: Klippert<br />passed:  2<br />not_passed: 24<br />tot_bills: 26<br />Party: R","pct_passed: 0.05882353<br />eb_estimate: 0.14534566<br />`Original Sponsor`: Ericksen<br />passed:  1<br />not_passed: 16<br />tot_bills: 17<br />Party: R","pct_passed: 0.05882353<br />eb_estimate: 0.14534566<br />`Original Sponsor`: Stokesbary<br />passed:  1<br />not_passed: 16<br />tot_bills: 17<br />Party: R","pct_passed: 0.07142857<br />eb_estimate: 0.15410718<br />`Original Sponsor`: Honeyford<br />passed:  1<br />not_passed: 13<br />tot_bills: 14<br />Party: R","pct_passed: 0.10000000<br />eb_estimate: 0.15545845<br />`Original Sponsor`: Dent<br />passed:  2<br />not_passed: 18<br />tot_bills: 20<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.15594854<br />`Original Sponsor`: Schoesler<br />passed:  0<br />not_passed:  7<br />tot_bills:  7<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.15968229<br />`Original Sponsor`: DeBolt<br />passed:  0<br />not_passed:  6<br />tot_bills:  6<br />Party: R","pct_passed: 0.08333333<br />eb_estimate: 0.16055959<br />`Original Sponsor`: Van Werven<br />passed:  1<br />not_passed: 11<br />tot_bills: 12<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.16359921<br />`Original Sponsor`: Dye<br />passed:  0<br />not_passed:  5<br />tot_bills:  5<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.16359921<br />`Original Sponsor`: Smith<br />passed:  0<br />not_passed:  5<br />tot_bills:  5<br />Party: R","pct_passed: 0.09090909<br />eb_estimate: 0.16399275<br />`Original Sponsor`: Bailey<br />passed:  1<br />not_passed: 10<br />tot_bills: 11<br />Party: R","pct_passed: 0.09090909<br />eb_estimate: 0.16399275<br />`Original Sponsor`: Corry<br />passed:  1<br />not_passed: 10<br />tot_bills: 11<br />Party: R","pct_passed: 0.09090909<br />eb_estimate: 0.16399275<br />`Original Sponsor`: Griffey<br />passed:  1<br />not_passed: 10<br />tot_bills: 11<br />Party: R","pct_passed: 0.09090909<br />eb_estimate: 0.16399275<br />`Original Sponsor`: Maycumber<br />passed:  1<br />not_passed: 10<br />tot_bills: 11<br />Party: R","pct_passed: 0.11764706<br />eb_estimate: 0.16429679<br />`Original Sponsor`: Becker<br />passed:  2<br />not_passed: 15<br />tot_bills: 17<br />Party: R","pct_passed: 0.12500000<br />eb_estimate: 0.16747055<br />`Original Sponsor`: Wagoner<br />passed:  2<br />not_passed: 14<br />tot_bills: 16<br />Party: R","pct_passed: 0.10000000<br />eb_estimate: 0.16757594<br />`Original Sponsor`: Chambers<br />passed:  1<br />not_passed:  9<br />tot_bills: 10<br />Party: R","pct_passed: 0.15384615<br />eb_estimate: 0.16945240<br />`Original Sponsor`: Zeiger<br />passed:  6<br />not_passed: 33<br />tot_bills: 39<br />Party: R","pct_passed: 0.11111111<br />eb_estimate: 0.17131920<br />`Original Sponsor`: Eslick<br />passed:  1<br />not_passed:  8<br />tot_bills:  9<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.17203927<br />`Original Sponsor`: Gildon<br />passed:  0<br />not_passed:  3<br />tot_bills:  3<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.17203927<br />`Original Sponsor`: McCaslin<br />passed:  0<br />not_passed:  3<br />tot_bills:  3<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.17203927<br />`Original Sponsor`: Rude<br />passed:  0<br />not_passed:  3<br />tot_bills:  3<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.17203927<br />`Original Sponsor`: Sutherland<br />passed:  0<br />not_passed:  3<br />tot_bills:  3<br />Party: R","pct_passed: 0.15625000<br />eb_estimate: 0.17219954<br />`Original Sponsor`: Braun<br />passed:  5<br />not_passed: 27<br />tot_bills: 32<br />Party: R","pct_passed: 0.15000000<br />eb_estimate: 0.17339011<br />`Original Sponsor`: MacEwen<br />passed:  3<br />not_passed: 17<br />tot_bills: 20<br />Party: R","pct_passed: 0.12500000<br />eb_estimate: 0.17523352<br />`Original Sponsor`: Barkis<br />passed:  1<br />not_passed:  7<br />tot_bills:  8<br />Party: R","pct_passed: 0.12500000<br />eb_estimate: 0.17523352<br />`Original Sponsor`: Irwin<br />passed:  1<br />not_passed:  7<br />tot_bills:  8<br />Party: R","pct_passed: 0.16000000<br />eb_estimate: 0.17557960<br />`Original Sponsor`: Rivers<br />passed:  4<br />not_passed: 21<br />tot_bills: 25<br />Party: R","pct_passed: 0.00000000<br />eb_estimate: 0.17659452<br />`Original Sponsor`: Boehnke<br />passed:  0<br />not_passed:  2<br />tot_bills:  2<br />Party: R","pct_passed: 0.14285714<br />eb_estimate: 0.17933089<br />`Original Sponsor`: Chandler<br />passed:  1<br />not_passed:  6<br />tot_bills:  7<br />Party: R","pct_passed: 0.16666667<br />eb_estimate: 0.18362447<br />`Original Sponsor`: Dufault<br />passed:  1<br />not_passed:  5<br />tot_bills:  6<br />Party: R","pct_passed: 0.18181818<br />eb_estimate: 0.18469789<br />`Original Sponsor`: Brown<br />passed:  4<br />not_passed: 18<br />tot_bills: 22<br />Party: R","pct_passed: 0.18750000<br />eb_estimate: 0.18678776<br />`Original Sponsor`: Orcutt<br />passed:  3<br />not_passed: 13<br />tot_bills: 16<br />Party: R","pct_passed: 0.20000000<br />eb_estimate: 0.18942560<br />`Original Sponsor`: Vick<br />passed:  2<br />not_passed:  8<br />tot_bills: 10<br />Party: R","pct_passed: 0.20000000<br />eb_estimate: 0.19132176<br />`Original Sponsor`: Warnick<br />passed:  4<br />not_passed: 16<br />tot_bills: 20<br />Party: R","pct_passed: 0.25000000<br />eb_estimate: 0.19285942<br />`Original Sponsor`: Goehner<br />passed:  1<br />not_passed:  3<br />tot_bills:  4<br />Party: R","pct_passed: 0.25000000<br />eb_estimate: 0.19285942<br />`Original Sponsor`: Hawkins<br />passed:  1<br />not_passed:  3<br />tot_bills:  4<br />Party: R","pct_passed: 0.22222222<br />eb_estimate: 0.19365694<br />`Original Sponsor`: Kretz<br />passed:  2<br />not_passed:  7<br />tot_bills:  9<br />Party: R","pct_passed: 0.33333333<br />eb_estimate: 0.19783421<br />`Original Sponsor`: Hoff<br />passed:  1<br />not_passed:  2<br />tot_bills:  3<br />Party: R","pct_passed: 0.33333333<br />eb_estimate: 0.19783421<br />`Original Sponsor`: Ybarra<br />passed:  1<br />not_passed:  2<br />tot_bills:  3<br />Party: R","pct_passed: 0.23076923<br />eb_estimate: 0.19827832<br />`Original Sponsor`: Schmick<br />passed:  3<br />not_passed: 10<br />tot_bills: 13<br />Party: R","pct_passed: 0.25000000<br />eb_estimate: 0.20242924<br />`Original Sponsor`: Wilson, L.<br />passed:  3<br />not_passed:  9<br />tot_bills: 12<br />Party: R","pct_passed: 0.23809524<br />eb_estimate: 0.20556726<br />`Original Sponsor`: Padden<br />passed:  5<br />not_passed: 16<br />tot_bills: 21<br />Party: R","pct_passed: 0.27272727<br />eb_estimate: 0.20675768<br />`Original Sponsor`: Volz<br />passed:  3<br />not_passed:  8<br />tot_bills: 11<br />Party: R","pct_passed: 0.40000000<br />eb_estimate: 0.21265814<br />`Original Sponsor`: Harris<br />passed:  2<br />not_passed:  3<br />tot_bills:  5<br />Party: R","pct_passed: 0.33333333<br />eb_estimate: 0.21599467<br />`Original Sponsor`: Steele<br />passed:  3<br />not_passed:  6<br />tot_bills:  9<br />Party: R","pct_passed: 0.30769231<br />eb_estimate: 0.21878386<br />`Original Sponsor`: Caldier<br />passed:  4<br />not_passed:  9<br />tot_bills: 13<br />Party: R","pct_passed: 0.27272727<br />eb_estimate: 0.21931955<br />`Original Sponsor`: King<br />passed:  6<br />not_passed: 16<br />tot_bills: 22<br />Party: R","pct_passed: 0.66666667<br />eb_estimate: 0.22362915<br />`Original Sponsor`: Graham<br />passed:  2<br />not_passed:  1<br />tot_bills:  3<br />Party: R","pct_passed: 0.30434783<br />eb_estimate: 0.23260382<br />`Original Sponsor`: Mosbrucker<br />passed:  7<br />not_passed: 16<br />tot_bills: 23<br />Party: R","pct_passed: 0.60000000<br />eb_estimate: 0.23718761<br />`Original Sponsor`: Holy<br />passed:  3<br />not_passed:  2<br />tot_bills:  5<br />Party: R","pct_passed: 0.44444444<br />eb_estimate: 0.23833241<br />`Original Sponsor`: Jenkin<br />passed:  4<br />not_passed:  5<br />tot_bills:  9<br />Party: R"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":["rgba(253,231,37,1)","rgba(44,134,139,1)","rgba(43,125,141,1)","rgba(46,115,141,1)","rgba(68,57,124,1)","rgba(68,53,120,1)","rgba(55,102,140,1)","rgba(57,98,139,1)","rgba(65,67,134,1)","rgba(65,67,134,1)","rgba(68,57,124,1)","rgba(64,78,136,1)","rgba(70,29,100,1)","rgba(69,25,97,1)","rgba(69,49,117,1)","rgba(69,20,94,1)","rgba(69,20,94,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(65,67,134,1)","rgba(66,64,131,1)","rgba(69,42,110,1)","rgba(43,141,138,1)","rgba(70,38,107,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(43,119,142,1)","rgba(64,78,136,1)","rgba(70,34,104,1)","rgba(70,34,104,1)","rgba(59,95,139,1)","rgba(68,1,84,1)","rgba(70,29,100,1)","rgba(69,25,97,1)","rgba(62,85,137,1)","rgba(66,64,131,1)","rgba(69,42,110,1)","rgba(64,78,136,1)","rgba(69,14,90,1)","rgba(69,14,90,1)","rgba(70,38,107,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,53,120,1)","rgba(69,49,117,1)","rgba(63,81,137,1)","rgba(69,45,114,1)","rgba(69,20,94,1)","rgba(70,38,107,1)","rgba(68,53,120,1)","rgba(62,85,137,1)","rgba(68,8,87,1)","rgba(61,88,138,1)","rgba(69,20,94,1)","rgba(70,38,107,1)"],"opacity":1,"size":5.66929133858268,"symbol":"triangle-up","line":{"width":1.88976377952756,"color":["rgba(253,231,37,1)","rgba(44,134,139,1)","rgba(43,125,141,1)","rgba(46,115,141,1)","rgba(68,57,124,1)","rgba(68,53,120,1)","rgba(55,102,140,1)","rgba(57,98,139,1)","rgba(65,67,134,1)","rgba(65,67,134,1)","rgba(68,57,124,1)","rgba(64,78,136,1)","rgba(70,29,100,1)","rgba(69,25,97,1)","rgba(69,49,117,1)","rgba(69,20,94,1)","rgba(69,20,94,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(69,45,114,1)","rgba(65,67,134,1)","rgba(66,64,131,1)","rgba(69,42,110,1)","rgba(43,141,138,1)","rgba(70,38,107,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(43,119,142,1)","rgba(64,78,136,1)","rgba(70,34,104,1)","rgba(70,34,104,1)","rgba(59,95,139,1)","rgba(68,1,84,1)","rgba(70,29,100,1)","rgba(69,25,97,1)","rgba(62,85,137,1)","rgba(66,64,131,1)","rgba(69,42,110,1)","rgba(64,78,136,1)","rgba(69,14,90,1)","rgba(69,14,90,1)","rgba(70,38,107,1)","rgba(68,8,87,1)","rgba(68,8,87,1)","rgba(68,53,120,1)","rgba(69,49,117,1)","rgba(63,81,137,1)","rgba(69,45,114,1)","rgba(69,20,94,1)","rgba(70,38,107,1)","rgba(68,53,120,1)","rgba(62,85,137,1)","rgba(68,8,87,1)","rgba(61,88,138,1)","rgba(69,20,94,1)","rgba(70,38,107,1)"]}},"hoveron":"points","name":"R","legendgroup":"R","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[-0.0333333333333333,0.7],"y":[-0.0333333333333333,0.7],"text":"intercept: 0<br />slope: 1","type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"rgba(211,211,211,0.5)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0],"y":[0.1],"name":"99_3dce385821ca9d24e5f4181dd0f9642f","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#440154"],[0.0526315789473684,"#451961"],[0.105263157894737,"#452A6E"],[0.157894736842105,"#44397C"],[0.210526315789474,"#414787"],[0.263157894736842,"#3E5589"],[0.315789473684211,"#39628B"],[0.368421052631579,"#31708D"],[0.421052631578947,"#2B7D8D"],[0.473684210526316,"#2C898B"],[0.526315789473684,"#2A9688"],[0.578947368421053,"#25A385"],[0.631578947368421,"#3AAE7D"],[0.684210526315789,"#53B971"],[0.736842105263158,"#67C464"],[0.789473684210526,"#77CF54"],[0.842105263157895,"#9AD64A"],[0.894736842105263,"#BDDC41"],[0.947368421052632,"#DEE235"],[1,"#FDE725"]],"colorbar":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"thickness":23.04,"title":"Total Bills Introduced","titlefont":{"color":"rgba(255,255,255,1)","family":"Roboto Condensed","size":15.2760481527605},"tickmode":"array","ticktext":["20","40","60"],"tickvals":[0.236842105263158,0.5,0.763157894736842],"tickfont":{"color":"rgba(224,224,224,1)","family":"Roboto Condensed","size":12.2208385222084},"ticklen":2,"len":0.5,"yanchor":"top","y":1}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":95.7011207970112,"r":39.8505603985056,"b":86.8410128684101,"l":93.8148609381486},"plot_bgcolor":"rgba(30,30,30,1)","paper_bgcolor":"rgba(30,30,30,1)","font":{"color":"rgba(224,224,224,1)","family":"Roboto Condensed","size":15.2760481527605},"title":"<b> Overall Bill Passage <\/b>","titlefont":{"color":"rgba(255,255,255,1)","family":"Roboto Condensed","size":23.9103362391034},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.0333333333333333,0.7],"tickmode":"array","ticktext":["0.0%","20.0%","40.0%","60.0%"],"tickvals":[0,0.2,0.4,0.6],"categoryorder":"array","categoryarray":["0.0%","20.0%","40.0%","60.0%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.81901203819012,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(224,224,224,1)","family":"Roboto Condensed","size":15.2760481527605},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(51,51,51,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"y","title":"Observed percentage of bills passed","titlefont":{"color":"rgba(255,255,255,1)","family":"Roboto Condensed","size":11.9551681195517},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.0658452711945677,0.293732344609786],"tickmode":"array","ticktext":["10.0%","15.0%","20.0%","25.0%"],"tickvals":[0.1,0.15,0.2,0.25],"categoryorder":"array","categoryarray":["10.0%","15.0%","20.0%","25.0%"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":3.81901203819012,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(224,224,224,1)","family":"Roboto Condensed","size":15.2760481527605},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(51,51,51,1)","gridwidth":0.265670402656704,"zeroline":false,"anchor":"x","title":"Bayesian estimate","titlefont":{"color":"rgba(255,255,255,1)","family":"Roboto Condensed","size":11.9551681195517},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(224,224,224,1)","family":"Roboto Condensed","size":12.2208385222084},"y":0.409448818897638,"yanchor":"top"},"annotations":[{"text":"Party","x":1.02,"y":0.5,"showarrow":false,"ax":0,"ay":0,"font":{"color":"rgba(255,255,255,1)","family":"Roboto Condensed","size":15.2760481527605},"xref":"paper","yref":"paper","textangle":-0,"xanchor":"left","yanchor":"bottom","legendTitle":true}],"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"5ea62c6a608":{"x":{},"y":{},"label":{},"label2":{},"label3":{},"colour":{},"shape":{},"type":"scatter"},"5ea63fbe58a4":{"intercept":{},"slope":{}}},"cur_data":"5ea62c6a608","visdat":{"5ea62c6a608":["function (y) ","x"],"5ea63fbe58a4":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script><!--/html_preserve-->




