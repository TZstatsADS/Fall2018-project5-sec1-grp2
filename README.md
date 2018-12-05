# ADS Project 5: How to predict tomorrow's stock price?
![image](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/figs/mainpage-technical-sample.png)

Term: Fall 2018

+ Team #2
+ Projec title: How to predict tomorrow's stock price?
+ Team members
	+ Anke Xu
	+ Chuqiao Rong
	+ Peilin Li
	+ Yiqiao Yin
	+ The designated presenter is Yiqiao Yin.

+ Project summary: What is tomorrow's stock price? Under big data era, what searching technique can we use to grasp the useful information so that we can minimize our prediction error predicting a regression problem? This project studies price actions in capital market as a random walk from limit theorems. Through clear construction, we derive algorithms from a series of theorems to create standardized buy signals given a trader's committed frequency to participate in the market. Using such processed data, we can use influence measure, I-score, to select robust stock clusters to construct portfolio. Simulation result shows under the same risk profile a $1,000 initial investment returns \$5,000 while the same time S\&P500 returns less than $1,500. Empirical evidence show results of on average 97% error reduction.

**Mathematical Model** Lo et al (2002) have introduced a non-parametric statistics that measures the predictivity of a cluster of variables given a data set in discrete framework. After reading dissertation from Huang (2004) and Hsu (2014), we have adopted the extension of their methodology to measure predicitivity in continuous framework. 

The following graph is taken from Hsu (2014) and it presents an illustration to use nearest neighborhood to measure local mean in predictivity score.
![image](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/figs/fig-huang-page-10.PNG)

**Performance and Results** We present a 97% error reduction on average on 30 stocks in Dow Jones 30 Component on held-out test set. Below we present a sample of selected test set resutls for MMM for two comparisons: (1) the first is using time-series ARMA model, and (2) the second is using I-score as feature selection method before we do regression. 
![image](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/figs/MMM-TS.png)
![image](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/figs/MMM-Influence.png)

**Presentation** Yiqiao Yin is the designated presenter for this presentation. He will mainly be using [Presentation Slides](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/doc/Rubust%20Portfolio%20by%20Influence%20Measure%20-%20Presentation%20%5BYiqiao%20Yin%202018%5D.pdf) for the main material. For detailed reference, we invite our audience to read the paper on the [research site](https://yinscapital.com/research/). The paper is also uploaded to [zip folder](https://github.com/TZstatsADS/Fall2018-project5-sec1proj5-grp2/blob/master/doc/Robust%20Portfolio%20by%20Influence%20Measure.zip) in Github folder **doc**.

**Shiny App** We also build a platform using Shiny App and this app should serve as supplement in addition to the paper and presentation. Due to slow speed from Shiny server when executing code to download data lively, we will present limited information from Shiny App. The app can be accessed [here](https://y-yin.shinyapps.io/CENTRAL-INTELLIGENCE-PLATFORM-BETA/).
	
**Contribution statement**:
Group Leader: Yiqiao Yin

Other team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.


**Acknowledgement**: This project provide fair and robust analysis in predicting security prices. However, money management is more of art than science. We have not disclosed related strategy in game planning when it comes to risk management. Hence, this project does not serve as investment advise and nor are we responsible for any monetary losses from investment decisions by any audience. The risk of money management subject to your investment is solely your responsibility. 

**Thank You Note**: We also want to thank Professor Ying Liu and Professor [Tian Zheng](http://www.stat.columbia.edu/~tzheng/) for hosting lectures of Advanced Data Science this semester. It is with transcending gratitude that we announce here what an inspiration both professors have been throughout our experience of building this shiny app. Their knowledge, understanding and genuine “care” for others is illuminated in everything they do! We, Group 8, are in forever debt for their teachings. Moreoever, we also want to thank TA, Chengliang Tang. There is not enough we can say about how much we thank heaven that he is our teaching assistance. His patience and understanding are unsurpassed. We are grateful for being his students.

Special thanks to [Shawhwa Lo](http://statgene.wikischolars.columbia.edu/lo) for his work on non-parametric statistics, I-score. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
