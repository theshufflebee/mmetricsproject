---
title: "Terminally Online: Does Donald Trump impact Financial Markets"
author: "Marcos Constantinou, Ryan Fellarhi & Jonas Bruno"
date: "Last edited: 12.05.2025"
site: bookdown::bookdown_site
documentclass: book
bibliography: [packages.bib, macroeconometrics_citations.bib] #name bib files
# url: your book url like https://bookdown.org/yihui/bookdown
# cover-image: path to the social sharing image like images/cover.jpg -> Do Later
description: |
  This is our website for a Universityproject. We put this together from a basic bookdown example so there might be some stuff in here from that.
link-citations: true           # optional but helpful for clickable citations
csl: apa.csl
github-repo: theshufflebee/mmetricsproject
---

# Readme {.unnumbered}

This is a Website for a class project from the 2025 Class Macroeconometrics at Université de Lausanne.

IMPORTANT:use: bookdown::render_book("final/index.Rmd") to render site

## Usage {.unnumbered}

You can find all sections on the left. There is the Main Report which we hand in. This serves as a complete collection of the whole project to make sure everything is available to readers.

# Abstract {.unnumbered}

This Project assesses to what extent Financial Markets react to information provided by Donald Trump on Twitter and Truth Social. We asses the impact of posts on hourly volatility using ARMA-X. We evaluate multiple time horizons and independent variables, such as if Trump posts anything, specific words such as tariff and sentiments. We then calculate IRFs and show that there are significant impacts on volatility.

<!--chapter:end:index.Rmd-->

---
bibliography: macroeconometrics_citations.bib  #
csl: apa.csl
link-citations: true
editor_options: 
  markdown: 
    wrap: 72
---



# Introduction

Over the past 15 years social media has become an important
communication tool for politicians. One of the pioneers of this novel
approach has been Donald Trump, the 45th and 47th President of the United
States. Since his ban on Twitter after the January 6th riots, his quantity of
social media posts has drastically increased. This is shown in the following
figure. [^1]

[^1]: Includes both Posts and Reposts

The content of his posts can sometimes have announcements or teases of future
political decisions. Note the recent infamous "THIS IS A GREAT TIME TO BUY!!! DJT"
post sent just an hour before lifting his reciprocal tariffs. It is then not 
improbable that agents in financial markets might take this information into 
account in their decision making. This question has been asked before in the 
literature, focusing rather on his first term. 

This brings us to our research question:  Do Donald Trumps Posts impact market Volatility?

    
\@ref(fig:fig1)

![(\#fig:fig1)Terminally Online: Trump's Twitter & Truth Social Posts (EDT)](_main_files/figure-latex/fig1-1.pdf) 



## Literature Review

Information is one of the most valuable assets in the financial market.
Its importance lies at the core of the "Efficient Market Hypothesis", 
which states that the prices of assets fully reflect all
available information, adjusting immediately to any new data
@famaAdjustmentStockPrices2003 , and thereby creating a strong demand
for information flow. In addition, the “Mixture of Distribution
Hypothesis” states that the release of new information is closely linked
to movements in both realized and implied volatility (1)(3)(7).

Consequently, a large part of the literature had focused on the relation
between announcements, news and market activity. For example,
@schumakerTextualAnalysisStock2009 use various linguistic and textual
representations derived from financial news to predict stock market
prices. Similarly, @ederingtonHowMarketsProcess1993 analyze the impact
of macroeconomic news announcements on interest rate and foreign
exchange futures markets, particularly in terms of price changes and
volatility. Both studies, among others, find that prices— such as stock
prices—react primarily within minutes after the release of new
information.

Recently, the world has witnessed the rise of the Internet
which revolutionized the dissemination and accessibility of information.
Social media enable investors, analysts or politicians to instantly
share their information, news or opinions. This led some studies to
focus on the communication dynamics of social platform to predict
changes in the returns of financial assets (6)(8). In this context, the
impact of Trump’s tweets on various financial and macroeconomic
variables has been analysed by several studies, especially during his
first mandate.

Using high-frequency financial data,
@gjerstadPresidentTrumpsTweets2021 found an increase in uncertainty and
trading volume, along with a decline in the U.S. stock market—regardless
of the tweet's content. However, the effect was stronger when Trump used
confrontational words such as "tariff" or "trade war." Some of his
announcements also influenced the U.S. dollar exchange rate (l) and
certain market indices within minutes of the tweet being posted (r)(a).

Other scholars have shown that negative Trump tweets about specific
companies tended to reduce demand for their stocks (b)(g), whereas some
other have shown that they also impact market volatility indices such as
the VIX (w) or the Volfele(v). The effects of his tweets also extended
beyond the U.S.. For example, @nishimuraImpactsDonaldTrumps2025 shows a
positive relationship between volatility in European stock markets and
tweeter activity of Trump, and this effect tends to intensify as public
intention for his tweet grows (z).

Our paper is built as follows: 

<!--chapter:end:01-introduction.Rmd-->

---
bibliography: macroeconometrics_citations.bib  #
csl: apa.csl
link-citations: true
editor_options: 
  markdown: 
    wrap: 72
---





# Data

## Financial Data

For our financial data, we decided to try to find minute-by-minute prices for 
broad market indices. While the actual indices do not update their prices so often,
we had to take proxies under the form of ETF's that track them. Our 3 markets of
analysis are: SPY to track the S&P500, VGK to track the FTSE Developed Europe 
All Cap Index, and finally ASHR to track the CSI 300 China. We accessed this data
through a free stock API, Alpha Vantage. Our timeframe is from the first 
of January 2014 to the 7th of May 2025.


We then had to transform this data to get our main variable of interest, Average
Hourly Volatility (AHV). Note that this is realised market volatility. We did so 
with the following formula:
$$
\begin{aligned}
  v_t = \frac{1}{N}&\sum_{i=1}^N(\Delta p_{t,i})^2 
\end{aligned}
$$
Where $\Delta p_t$ is the difference in price (open - close) and $i$ represents
every minute.

We used a custom function in order to get the AHV for each open market hour. Note 
that the first hour is from 9:30 am to 10:00 am since the 
market open on a half-hour but closes at 4:00 pm. We can plot this data in
<span style="color:red"> *figX* </span>.


![](_main_files/figure-latex/fin plots-1.pdf)<!-- --> ![](_main_files/figure-latex/fin plots-2.pdf)<!-- --> 



## Political Data

We have two sources for Trump's posts. The Tweets are from Kaggle
@DonaldTrumpTweets and go until the 8th of January 2021. Since he
switched his primary Posting platform to Truth Social we use only that
Data from 2021 onwards. All Truth Social Posts were scrapped from
trumpstruth.org, a webpage that aims to conserve all his posts. You can
find the dataset, and webscrapper, and Data cleaning process in the
Appendix.

Since we're using financial data that is constrained by trading hours,
we decided to move posts after 16:00 to the next trading day's opening
hour.

A big problem we had in our analysis was what to do with social media posts
which appeared outside market hours. We first decided to simply ignore them, but 
it turned out to remove a lot of observations. We finally decided to push all the 
social media information outside market hours to the next open hour. This comes 
as an assumption.^[2] 

Since our financial data is hourly, we aggregate the social data by hour. We 
then construct multiple variables from the social media data. These include
a dummy for whether there was a post, the number of posts an hour and counts
for certain words ("tariffs","trade","china"). Further we applied some simple 
sentiment analysis algorithms on the data to see if there are certain sentiments 
in his tweets that move the markets. More information and detailed process are in 
the appendix and online.

<span style="color:red"> *insert wordcloud* </span>.

![](_main_files/figure-latex/social plots-1.pdf)<!-- --> 


## Final Dataframe


```{=latex}
 
  \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
  \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
  \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
  \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}

\begin{table}[ht]
\begin{centerbox}
\begin{threeparttable}
\captionsetup{justification=centering,singlelinecheck=off}
\caption{(\#tab:data) }
 \setlength{\tabcolsep}{0pt}
\begin{tabular}{l l l l}


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{timestamp} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{SPY\_vol} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{VGK\_vol} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{ASHR\_vol} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 10:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0466 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000731 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 5.32e-05 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 11:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0278 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000217 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 3.33e-05 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 12:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0859 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000468 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 3e-05\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 13:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0457 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000457 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 3.26e-05 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 14:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0336 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000224 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2.24e-05 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2025-05-06 15:00:00 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0517 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.000295 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2.03e-05 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}
\end{tabular}
\end{threeparttable}\par\end{centerbox}

\end{table}
 
```

```{=latex}
 
  \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
  \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
  \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
  \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}

\begin{table}[ht]
\begin{centerbox}
\begin{threeparttable}
\captionsetup{justification=centering,singlelinecheck=off}
\caption{(\#tab:data) }
 \setlength{\tabcolsep}{0pt}
\begin{tabular}{l l l l l}


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{dummy} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{N} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{tariff} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{trade} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{china} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 3 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 2 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 5 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 1 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}
\end{tabular}
\end{threeparttable}\par\end{centerbox}

\end{table}
 
```

```{=latex}
 
  \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
  \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
  \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
  \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}

\begin{table}[ht]
\begin{centerbox}
\begin{threeparttable}
\captionsetup{justification=centering,singlelinecheck=off}
\caption{(\#tab:data) }
 \setlength{\tabcolsep}{0pt}
\begin{tabular}{l l l l l}


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_anger} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_anticipation} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_disgust} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_fear} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_joy} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0909 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0455 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0909 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.227 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.087\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.087\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.13\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.13 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}
\end{tabular}
\end{threeparttable}\par\end{centerbox}

\end{table}
 
```

```{=latex}
 
  \providecommand{\huxb}[2]{\arrayrulecolor[RGB]{#1}\global\arrayrulewidth=#2pt}
  \providecommand{\huxvb}[2]{\color[RGB]{#1}\vrule width #2pt}
  \providecommand{\huxtpad}[1]{\rule{0pt}{#1}}
  \providecommand{\huxbpad}[1]{\rule[-#1]{0pt}{#1}}

\begin{table}[ht]
\begin{centerbox}
\begin{threeparttable}
\captionsetup{justification=centering,singlelinecheck=off}
\caption{(\#tab:data) }
 \setlength{\tabcolsep}{0pt}
\begin{tabular}{l l l l l}


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_sadness} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_surprise} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_trust} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_negative} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} \textbf{prop\_positive} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0455 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.136 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.364 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.556 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.444 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.0435 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.087 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.435 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.176 \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0.824 \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\cellcolor[RGB]{242, 242, 242}\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}|>{\huxb{0, 0, 0}{0.4}}|}
\arrayrulecolor{black}

\multicolumn{1}{!{\huxvb{0, 0, 0}{0.4}}r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} &
\multicolumn{1}{r!{\huxvb{0, 0, 0}{0.4}}}{\huxtpad{6pt + 1em}\raggedleft \hspace{6pt} 0\hphantom{0}\hphantom{0}\hphantom{0}\hphantom{0} \hspace{6pt}\huxbpad{6pt}} \tabularnewline[-0.5pt]


\hhline{>{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}->{\huxb{0, 0, 0}{0.4}}-}
\arrayrulecolor{black}
\end{tabular}
\end{threeparttable}\par\end{centerbox}

\end{table}
 
```





¨
^[2]: For instance, if Trump tweets on Good Friday (market holiday), then the 
market will only react to this new information on Monday at 9:30 am. 

<!--chapter:end:02-data.Rmd-->

# ARMA-X

## Methodology

Once we have our final dataframe, we could then finally start on some analysis.
We first thought of a simple ARMA-X type specification, taking the AHV as our
"y variable" and taking any of the social media variables as the exogenous
regressors. The assumption here is that, while the market reacts to Trump posts,
Trump's posts are chaotic, nonsensical, and random enough to be considered 
exogenous. 

We of course first start by checking stationarity of our variables (ADF), where we find
p-values of 0.01 suggesting that the processes are not explosive. Then, we use 
a custom function in order to choose the number of lags based on the AIC criterion.
This however, while often choose a very high number of lags, which could be 
explained by our data being hourly. As such we decided to put a limit of 3 lags,
which sees minimal AIC loss and simplifying our models considerably.

## Results

\@ref(tab:armax1)

### Full Timeframe
We run models with the following exogenous regressors: $TweetDummy$, $TweetCount$,
and the mentions of words $Tariff$, $Trade$, and $China$. We first note on 
<span style="color:red"> *ARMAX Table 1* </span> that all the x-regressors are significant,
apart from trade. Notice also that all the coefficients (apart from $Tariff_{t-3}$)
are positive, in line with our main hypothesis. The effect of $Tariff_{t-1}$ and 
$Tariff_{t-2}$ are especially large, given the usual size of the volatility 
(<span style="color:red"> *ARMAX Table 4* </span>). We in fact predict that an 
extra mention of tariffs one hour ago, leads to a whopping extra 0.02 in volatility
which is just about the average size for the full timeframe. We can see the
impulse response function (IRF) for this shock, in <span style="color:red"> *IRFtarif* </span>.
Notice that there is a large response in the first periods, and then a graduate
decline over time. Something to note is that in our analysises of IRF's, when including
MA terms, the decline shows up gradual while being much sharper when only including
AR terms. 
Note that we ran all these models on the VGK and ASHR ETF's as well, though no
signficant results appear apart from a small but statistically significant effect
of the tariff variable for VGK.

### Split Samples
We then split our sample for the first and second term of the Trump presidancy.
We only run models on tariff, trade and china this time. As seen on table
<span style="color:red"> *ARMAX Table 2* </span>, the first interesting result
is in the coefficients of tariff being significant and very large in the second
term, while being small and not statistically significant in the first. A similar
story goes for the China variable. This may lend some evidence to support the 
claim that investors are much more reactive to Trump's social media presence
now than before. 
Finally, we can check the residuals of all these models to test them somewhat. 
On <span style="color:red"> *ARMAX Table 3* </span>, the pvalues being zero
for the full timeframe and first term indicate that there is autocorrelation in 
the residuals, thus suggesting that these estimations have problems. Note however,
that the pvalues for the second term are quite high, lending support to our
models on the split sample. These results suggest that perhaps ARMA-X models are
not right in this context, as it is not unreasonable to think that Trump does 
in fact react to market movements. With this information, we decided to run
a VAR model to deepen our understanding of these variables.




<<<<<<< HEAD









=======
>>>>>>> 8ef96175d79b5bd3e2aa79f94b0965dea34f62e5

<!--chapter:end:03-armax.Rmd-->

# VAR 


## Methodology


## Results



### Full Timeframe



### Split Samples

<!--chapter:end:04-var.Rmd-->

# Conclusion


<!--chapter:end:05-conclusion.Rmd-->

# Bibliography





<!--chapter:end:06-bibliography.Rmd-->

# Appendix





# Full Timeframe (Jan 2024 to May 2025)


``` r
#load final dataset
source(here("helperfunctions/full_data.R"))

#backup
backup = data

#select timeframe 
data = filter(data,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))

#for interpretation
mean1 = mean(data$SPY_vol)
```


## SPY Models

We choose the specification in the armax_models file. In this file, we will
just run said specifications to produce nice tables and graphs to include in 
our final paper. This is also why there are specification differences in the 
separate timeframes. We always use the best fit we found earlier.



``` r
models <- list()

# ARMA-X(3,3,1) with Tweet Dummy as Exogenous
models[["Model 1"]] <- armax(data$SPY_vol, xreg = data$dummy, latex = F,
                             nb.lags = 1, p = 3, q = 3) 

# ARMA-X(3,3,1) with Tweet Count as Exogenous
models[["Model 2"]] <- armax(data$SPY_vol, xreg = data$N, latex = F,
                             nb.lags = 1, p = 3, q = 3) 

# ARMA-X(3,2,3) with Tariff Mentions as Exogenous
models[["Model 3"]] <- armax(data$SPY_vol, xreg = data$tariff, latex = F,
                             nb.lags = 3, p = 3, q = 2) 

# ARMA-X(3,2,1) with Trade Mentions as Exogenous
models[["Model 4"]] <- armax(data$SPY_vol, xreg = data$trade, latex = F,
                             nb.lags = 1, p = 3, q = 2) 

# ARMA-X(3,2,0) with China Mentions as Exogenous
models[["Model 5"]] <- armax(data$SPY_vol, xreg = data$china, latex = F,
                             nb.lags = 0, p = 3, q = 2) 
```

## SPY Table


``` r
names = list( "ar1" = "AR(1)",
              "ar2" = "AR(2)",
              "ar3" = "AR(3)",
              "ma1" = "MA(1)",
              "ma2" = "MA(2)",
              "ma3" = "MA(3)",
              "(Intercept)" = "Constant",
              "dummy_lag_0" = "$TweetDummy_{t}$",
              "dummy_lag_1" = "$TweetDummy_{t-1}$",
              "N_lag_0" = "$TweetCount_{t}$",
              "N_lag_1" = "$TweetCount_{t-1}$",
              "tariff_lag_0" = "$Tariff_{t}$",
              "tariff_lag_1" = "$Tariff_{t-1}$",
              "tariff_lag_2" = "$Tariff_{t-2}$",
              "tariff_lag_3" = "$Tariff_{t-3}$",
              "trade_lag_0" = "$Trade_{t}$",
              "trade_lag_1" = "$Trade_{t-1}$",
              "china_lag_0" = "$China_{t}$")

table1 = texreg(models,
          custom.model.names = names(models), 
          custom.coef.map = names,
          caption = "ARMAX Models of Average Hourly Volatility",
          caption.above = TRUE,
          label = "tab:armax",
          digits = 4)
table1
```


\begin{table}
\caption{ARMAX Models of Average Hourly Volatility}
\begin{center}
\begin{tabular}{l c c c c c}
\hline
 & Model 1 & Model 2 & Model 3 & Model 4 & Model 5 \\
\hline
AR(1)              & $0.0300$        & $0.0278$        & $0.2200^{***}$  & $2.1903^{***}$  & $0.2209^{***}$  \\
                   & $(0.0510)$      & $(0.0510)$      & $(0.0084)$      & $(0.0096)$      & $(0.0084)$      \\
AR(2)              & $0.7229^{***}$  & $0.7210^{***}$  & $0.9388^{***}$  & $-1.4727^{***}$ & $0.9382^{***}$  \\
                   & $(0.0397)$      & $(0.0399)$      & $(0.0037)$      & $(0.0173)$      & $(0.0037)$      \\
AR(3)              & $0.2110^{***}$  & $0.2148^{***}$  & $-0.1837^{***}$ & $0.2784^{***}$  & $-0.1837^{***}$ \\
                   & $(0.0287)$      & $(0.0284)$      & $(0.0079)$      & $(0.0082)$      & $(0.0079)$      \\
MA(1)              & $0.2751^{***}$  & $0.2779^{***}$  & $0.0870^{***}$  & $-1.8955^{***}$ & $0.0878^{***}$  \\
                   & $(0.0496)$      & $(0.0496)$      & $(0.0042)$      & $(0.0062)$      & $(0.0042)$      \\
MA(2)              & $-0.6445^{***}$ & $-0.6430^{***}$ & $-0.8960^{***}$ & $0.9165^{***}$  & $-0.8950^{***}$ \\
                   & $(0.0284)$      & $(0.0285)$      & $(0.0042)$      & $(0.0063)$      & $(0.0042)$      \\
MA(3)              & $-0.3527^{***}$ & $-0.3563^{***}$ &                 &                 &                 \\
                   & $(0.0256)$      & $(0.0253)$      &                 &                 &                 \\
$TweetDummy_{t}$   & $0.0014^{***}$  &                 &                 &                 &                 \\
                   & $(0.0002)$      &                 &                 &                 &                 \\
$TweetDummy_{t-1}$ & $0.0008^{***}$  &                 &                 &                 &                 \\
                   & $(0.0002)$      &                 &                 &                 &                 \\
$TweetCount_{t}$   &                 & $0.0004^{***}$  &                 &                 &                 \\
                   &                 & $(0.0001)$      &                 &                 &                 \\
$TweetCount_{t-1}$ &                 & $0.0002^{**}$   &                 &                 &                 \\
                   &                 & $(0.0001)$      &                 &                 &                 \\
$Tariff_{t}$       &                 &                 & $0.0035^{*}$    &                 &                 \\
                   &                 &                 & $(0.0014)$      &                 &                 \\
$Tariff_{t-1}$     &                 &                 & $0.0191^{***}$  &                 &                 \\
                   &                 &                 & $(0.0015)$      &                 &                 \\
$Tariff_{t-2}$     &                 &                 & $0.0103^{***}$  &                 &                 \\
                   &                 &                 & $(0.0015)$      &                 &                 \\
$Tariff_{t-3}$     &                 &                 & $-0.0045^{**}$  &                 &                 \\
                   &                 &                 & $(0.0014)$      &                 &                 \\
$Trade_{t}$        &                 &                 &                 & $0.0032$        &                 \\
                   &                 &                 &                 & $(0.0018)$      &                 \\
$Trade_{t-1}$      &                 &                 &                 & $0.0016$        &                 \\
                   &                 &                 &                 & $(0.0018)$      &                 \\
$China_{t}$        &                 &                 &                 &                 & $0.0026^{*}$    \\
                   &                 &                 &                 &                 & $(0.0012)$      \\
\hline
AIC                & $-45761.2161$   & $-45737.6695$   & $-46020.9547$   & $-45816.1540$   & $-45840.5349$   \\
AICc               & $-45761.2051$   & $-45737.6585$   & $-46020.9415$   & $-45816.1449$   & $-45840.5277$   \\
BIC                & $-45682.1963$   & $-45658.6497$   & $-45934.0340$   & $-45745.0361$   & $-45777.3186$   \\
Log Likelihood     & $22890.6081$    & $22878.8348$    & $23021.4773$    & $22917.0770$    & $22928.2675$    \\
Num. obs.          & $19970$         & $19970$         & $19968$         & $19970$         & $19971$         \\
\hline
\multicolumn{6}{l}{\scriptsize{$^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$}}
\end{tabular}
\label{tab:armax}
\end{center}
\end{table}


## SPY IRFs


``` r
#we want to plot the IRFs of these models
nb.periods = 7 * 15

#irf.plot(models[["Model 1"]],nb.periods,title="Tweet Dummy Shock")
#irf.plot(models[["Model 2"]],nb.periods,title="Tweet Count Shock")
plot1 = irf.plot(models[["Model 3"]],nb.periods,
                 title="Tariff Mention Shock - Full Timeframe")
plot1
```

![](_main_files/figure-latex/SPYirf-1.pdf)<!-- --> 

``` r
#irf.plot(models[["Model 4"]],nb.periods,title="Trade Mention Shock")
#irf.plot(models[["Model 5"]],nb.periods,title="China Mention Shock")

ggsave("armax_plot1.png",plot=plot1,bg="white")
```


## SPY Residuals

``` r
res1 = checkresiduals(models[["Model 1"]], plot = FALSE)
res2 = checkresiduals(models[["Model 2"]], plot = FALSE)
res3 = checkresiduals(models[["Model 3"]], plot = FALSE)
res4 = checkresiduals(models[["Model 4"]], plot = FALSE)
res5 = checkresiduals(models[["Model 5"]], plot = FALSE)
```


``` r
resnames = c("Twitter Dummy", "Twitter Count", "Tariff", "Trade", "China")

#extract p-values directly from checkresiduals results
pvals <- data.frame(Model = resnames,
                    `Ljung-Box p-value` = c(
                      res1$p.value,
                      res2$p.value,
                      res3$p.value,
                      res4$p.value,
                      res5$p.value))

#table
knitr::kable(pvals, digits = 100, caption = "Full Timeframe Ljung-Box Test p-values")
```

\begin{table}

\caption{(\#tab:SPYresiduals table)Full Timeframe Ljung-Box Test p-values}
\centering
\begin{tabular}[t]{l|r}
\hline
Model & Ljung.Box.p.value\\
\hline
Twitter Dummy & 0\\
\hline
Twitter Count & 0\\
\hline
Tariff & 0\\
\hline
Trade & 0\\
\hline
China & 0\\
\hline
\end{tabular}
\end{table}



# First Term


``` r
#load final dataset
data = backup

#first term
data = filter(data,between(timestamp, as.Date('2017-01-20'), as.Date('2021-01-20')))

#for interpretation
mean2 = mean(data$SPY_vol)
```

## SPY Models


``` r
models <- list()

# ARMA-X(3,3,0) with Tariff Mentions as Exogenous
models[["First Term (1)"]] <- armax(data$SPY_vol, xreg = data$tariff, latex = F,
                             nb.lags = 0, p = 3, q = 3) 

# ARMA-X(3,3,0) with Trade Mentions as Exogenous
models[["First Term (2)"]] <- armax(data$SPY_vol, xreg = data$trade, latex = F,
                             nb.lags = 0, p = 3, q = 3)

# ARMA-X(3,3,0) with Trade Mentions as Exogenous
models[["First Term (3)"]] <- armax(data$SPY_vol, xreg = data$china, latex = F,
                             nb.lags = 0, p = 3, q = 3) 
```


## SPY Residuals

``` r
res6 = checkresiduals(models[["First Term (1)"]], plot = FALSE)
res7 = checkresiduals(models[["First Term (2)"]], plot = FALSE)
res8 = checkresiduals(models[["First Term (3)"]], plot = FALSE)

pvals_new1 <- data.frame(
  Model = c("First Term Tariffs", "First Term Trade", "First Term China"),
  `Ljung-Box p-value` = c(
    res6$p.value,
    res7$p.value,
    res8$p.value))
```



# Second Term


``` r
#load final dataset
data = backup

#second term
data = filter(data,between(timestamp, as.Date('2025-01-20'), as.Date('2025-05-07')))

#for interpretation
mean3 = mean(data$SPY_vol)
```

## SPY Models


``` r
# ARMA-X(3,2,3) with Tariff Mentions as Exogenous
models[["Second Term (1)"]] <- armax(data$SPY_vol, xreg = data$tariff, latex = F,
                             nb.lags = 2, p = 1, q = 2) 

# ARMA-X(3,2,1) with Trade Mentions as Exogenous
models[["Second Term (2)"]] <- armax(data$SPY_vol, xreg = data$trade, latex = F,
                             nb.lags = 0, p = 1, q = 2) 

# ARMA-X(3,2,0) with China Mentions as Exogenous
models[["Second Term (3)"]] <- armax(data$SPY_vol, xreg = data$china, latex = F,
                             nb.lags = 2, p = 1, q = 2) 
```


## SPY IRFs

``` r
#we want to plot the IRFs of these models
nb.periods = 7 * 15

plot2 = irf.plot(models[["Second Term (1)"]],nb.periods,
                 title="Tariff Mention Shock - Second Term")
plot2
```

![](_main_files/figure-latex/2nd SPYirf-1.pdf)<!-- --> 

``` r
ggsave("armax_plot2.png",plot=plot2,bg="white")

plot3 = irf.plot(models[["Second Term (3)"]],nb.periods,
                 title="China Mention Shock - Second Term")
plot3
```

![](_main_files/figure-latex/2nd SPYirf-2.pdf)<!-- --> 


## SPY Residuals

``` r
res9 = checkresiduals(models[["Second Term (1)"]], plot = FALSE)
res10 = checkresiduals(models[["Second Term (2)"]], plot = FALSE)
res11 = checkresiduals(models[["Second Term (3)"]], plot = FALSE)

pvals_new2 <- data.frame(
  Model = c("Second Term Tariffs", "Second Term Trade", "Second Term China"),
  `Ljung-Box p-value` = c(
    res9$p.value,
    res10$p.value,
    res11$p.value))

#combine with other term
pvals_combined <- rbind(pvals_new1, pvals_new2)
```


## SPY Table (both terms)

\centering
Table: (\#tab:armax1) Split-Term ARMAX Models of Average Hourly Volatility


``` r
library(modelsummary)

xnames <- c(
  "ar1" = "AR(1)",
  "ar2" = "AR(2)",
  "ar3" = "AR(3)",
  "ma1" = "MA(1)",
  "ma2" = "MA(2)",
  "ma3" = "MA(3)",
  "(Intercept)" = "Constant",
  "tariff_lag_0" = "$Tariff_{t}$",
  "tariff_lag_1" = "$Tariff_{t-1}$",
  "tariff_lag_2" = "$Tariff_{t-2}$",
  "trade_lag_0" = "$Trade_{t}$",
  "china_lag_0" = "$China_{t}$",
  "china_lag_1" = "$China_{t-1}$",
  "china_lag_2" = "$China_{t-2}$"
)

msummary(models,
         coef_map = xnames,
         stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
         output = "markdown",   # or "latex" for PDF, or "html" for HTML output
         title = "Split-Term ARMAX Models of Average Hourly Volatility",
         fmt = 4,
         estimate = "{estimate}{stars}",
         gof_omit = "IC|Log|Num. obs."
)
```

\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Split-Term ARMAX Models of Average Hourly Volatility},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]Q[]Q[]Q[]Q[]Q[]},
column{2,3,4,5,6,7}={}{halign=c,},
column{1}={}{halign=l,},
hline{28}={1,2,3,4,5,6,7}{solid, black, 0.05em},
}                     %% tabularray inner close
\toprule
& First Term (1) & First Term (2) & First Term (3) & Second Term (1) & Second Term (2) & Second Term (3) \\ \midrule %% TinyTableHeader
AR(1) & 0.2953*** & 0.2943*** & 0.2927*** & 0.9686*** & 0.9683*** & 0.9693*** \\
& (0.0225) & (0.0224) & (0.0224) & (0.0163) & (0.0163) & (0.0161) \\
AR(2) & 0.1434*** & 0.1439*** & 0.1438*** &  &  &  \\
& (0.0220) & (0.0220) & (0.0219) &  &  &  \\
AR(3) & 0.5456*** & 0.5462*** & 0.5480*** &  &  &  \\
& (0.0223) & (0.0222) & (0.0222) &  &  &  \\
MA(1) & 0.1854*** & 0.1863*** & 0.1866*** & -0.6965*** & -0.6905*** & -0.7207*** \\
& (0.0180) & (0.0179) & (0.0179) & (0.0469) & (0.0469) & (0.0467) \\
MA(2) & -0.1707*** & -0.1706*** & -0.1695*** & -0.1732*** & -0.1755*** & -0.1609*** \\
& (0.0169) & (0.0169) & (0.0168) & (0.0437) & (0.0438) & (0.0434) \\
MA(3) & -0.6557*** & -0.6564*** & -0.6575*** &  &  &  \\
& (0.0162) & (0.0161) & (0.0161) &  &  &  \\
$Tariff_{t}$ & 0.0011 &  &  & 0.0048 &  &  \\
& (0.0010) &  &  & (0.0099) &  &  \\
$Tariff_{t-1}$ &  &  &  & 0.0278** &  &  \\
&  &  &  & (0.0102) &  &  \\
$Tariff_{t-2}$ &  &  &  & 0.0168 &  &  \\
&  &  &  & (0.0099) &  &  \\
$Trade_{t}$ &  & 0.0023** &  &  & -0.0074 &  \\
&  & (0.0009) &  &  & (0.0297) &  \\
$China_{t}$ &  &  & 0.0018** &  &  & 0.0173 \\
&  &  & (0.0006) &  &  & (0.0319) \\
$China_{t-1}$ &  &  &  &  &  & 0.1515*** \\
&  &  &  &  &  & (0.0324) \\
$China_{t-2}$ &  &  &  &  &  & 0.1309*** \\
&  &  &  &  &  & (0.0319) \\
Num.Obs. & 7042 & 7042 & 7042 & 516 & 518 & 516 \\
RMSE & 0.03 & 0.03 & 0.03 & 0.44 & 0.44 & 0.43 \\
\bottomrule
\end{talltblr}
\end{table}


# Descriptive Stats


``` r
means <- data.frame(
  Model = c("Full Time Mean", "First Term Mean", "Second Term Mean"),
  `SPY Volatility Mean` = c(
    mean1,
    mean2,
    mean3))

table4 = knitr::kable(means, digits = 6, format="latex",
             caption = "Summary Statistics of SPY Volatility")

table4
```

\begin{table}

\caption{(\#tab:means)Summary Statistics of SPY Volatility}
\centering
\begin{tabular}[t]{l|r}
\hline
Model & SPY.Volatility.Mean\\
\hline
Full Time Mean & 0.022621\\
\hline
First Term Mean & 0.017486\\
\hline
Second Term Mean & 0.144248\\
\hline
\end{tabular}
\end{table}


<!--chapter:end:07-appendix.Rmd-->

# Cross-references {#cross}

Cross-references make it easier for your readers to find and link to elements in your book.

## Chapters and sub-chapters

There are two steps to cross-reference any heading:

1. Label the heading: `# Hello world {#nice-label}`. 
    - Leave the label off if you like the automated heading generated based on your heading title: for example, `# Hello world` = `# Hello world {#hello-world}`.
    - To label an un-numbered heading, use: `# Hello world {-#nice-label}` or `{# Hello world .unnumbered}`.

1. Next, reference the labeled heading anywhere in the text using `\@ref(nice-label)`; for example, please see Chapter \@ref(cross). 
    - If you prefer text as the link instead of a numbered reference use: [any text you want can go here](#cross).

## Captioned figures and tables

Figures and tables *with captions* can also be cross-referenced from elsewhere in your book using `\@ref(fig:chunk-label)` and `\@ref(tab:chunk-label)`, respectively.

See Figure \@ref(fig:nice-fig).


``` r
par(mar = c(4, 4, .1, .1))
plot(pressure, type = 'b', pch = 19)
```

\begin{figure}

{\centering \includegraphics[width=0.8\linewidth,alt={Plot with connected points showing that vapor pressure of mercury increases exponentially as temperature increases.}]{_main_files/figure-latex/nice-fig-1} 

}

\caption{Here is a nice figure!}(\#fig:nice-fig)
\end{figure}

Don't miss Table \@ref(tab:nice-tab).


``` r
knitr::kable(
  head(pressure, 10), caption = 'Here is a nice table!',
  booktabs = TRUE
)
```

\begin{table}

\caption{(\#tab:nice-tab)Here is a nice table!}
\centering
\begin{tabular}[t]{rr}
\toprule
temperature & pressure\\
\midrule
0 & 0.0002\\
20 & 0.0012\\
40 & 0.0060\\
60 & 0.0300\\
80 & 0.0900\\
\addlinespace
100 & 0.2700\\
120 & 0.7500\\
140 & 1.8500\\
160 & 4.2000\\
180 & 8.8000\\
\bottomrule
\end{tabular}
\end{table}

<!--chapter:end:08-test.Rmd-->

