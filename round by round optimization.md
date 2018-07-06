---
title: "Optimization for Fantasy Football Snake Drafts (Part 2) - Please Don't Do Zero RB"
output:  
  html_document:
    toc: true
    fig_width: 8.5
    fig_height: 4
  md_document:
    toc: true
    fig_width: 8.5
    fig_height: 4
    variant: markdown_github
---

---

## Background



[In my last post](https://brianlefevre.com/optimization-for-fantasy-football-snake-drafts/) I tested different draft strategies for optimizing draft picks for Yahoo default leagues, evaluating the strategies based on their mean-simulated starting lineup given the players picked. This accounted for player variance and waiver wire adds. In this post I will also account for opponent draft uncertainty. If a player has an ADP of 4.1, my original system would assume they will be there at 3.11, but in reality there is a good chance they won't be there, and so I will account for that in this post. If you only care about the results just skip to the conclusion.

---

## Draft Methodology



My system is very similar to my previous post. Like last time, I start off with a dataframe of players with their ADPs:


```r
head(adp[, c("Player", "Pos", "ADP_est", "ADPSD_est", "ADP_Rank", "HALF", "STD", "PPR")], 25)
```

```
##                  Player Pos ADP_est ADPSD_est ADP_Rank     HALF      STD      PPR
## 649         Todd Gurley  RB    1.85      0.90        1 286.9787 278.7288 295.3511
## 440         Leveon Bell  RB    2.25      1.00        2 285.5994 276.2004 294.9930
## 258     Ezekiel Elliott  RB    3.40      1.35        3 245.7330 240.7392 250.8822
## 188       David Johnson  RB    3.90      1.10        4 237.7158 232.9482 242.4851
## 39        Antonio Brown  WR    5.10      1.40        5 244.1744 220.2120 268.1369
## 27         Alvin Kamara  RB    6.00      1.25        6 245.4832 235.7970 255.2076
## 599      Saquon Barkley  RB    6.80      1.80        7 221.9244 216.4002 227.4485
## 193     Deandre Hopkins  WR    8.80      1.90        8 217.1364 196.1761 238.0967
## 400         Kareem Hunt  RB    9.30      1.75        9 229.6042 222.5463 236.7176
## 437   Leonard Fournette  RB   10.00      1.80       10 213.3087 207.6375 219.0514
## 494       Melvin Gordon  RB   10.45      2.05       11 212.0997 205.3586 218.8665
## 533       Odell Beckham  WR   11.50      2.05       12 196.8351 178.9319 214.7382
## 167         Dalvin Cook  RB   12.90      2.00       13 204.0279 199.3044 208.7746
## 386         Julio Jones  WR   14.10      2.15       14 224.4879 202.2329 246.7428
## 507      Michael Thomas  WR   15.90      2.30       15 208.1782 188.1379 228.2185
## 223     Devonta Freeman  RB   17.55      2.50       16 202.7999 196.5961 209.0049
## 186       Davante Adams  WR   18.10      2.40       17 176.9410 161.1901 192.6918
## 403        Keenan Allen  WR   18.40      2.50       18 193.9841 176.8953 211.0729
## 439        Lesean Mccoy  RB   18.85      2.90       19 195.3838 189.0302 201.7331
## 133 Christian Mccaffrey  RB   20.15      3.10       20 213.4223 203.1830 223.5142
## 10             Aj Green  WR   20.55      2.25       21 185.0449 168.3994 201.6903
## 341     Jerick Mckinnon  RB   20.60      3.50       22 194.1646 187.8265 200.4310
## 359       Jordan Howard  RB   22.10      3.05       23 170.0072 166.0453 174.0318
## 500       Michael Evans  WR   23.55      2.75       24 182.9814 164.8576 201.1052
## 571   Robert Gronkowski  TE   23.70      3.65       25 176.8560 160.0528 193.6592
```

</br>In the first post, I just optimized all my picks to maximize total points. This time it's similar, but because actual draft-position will be decided probabilistically, I will be storing my own picks one by one, and just re-running the optimization several times. Prior to each of my picks, I simulate the draft using the ADP_est (mean ADP) and ADPSD_est (sd ADP).  When it's my pick, I re-rank the players based on who is available. Then, I run the base case optimization the same as before to plan the rest of my picks. However, I only store the first player from this. I then repeat everything--I simulate more picks, then run the optimization for my remaining picks, returning only the first player, etc., until I've selected all of my players. Below is an example draft with the specified number of players to take:



```r
simDraft(slot="Slot4",scoring="HALF",numRB=4, numWR=6, numTE=1, numQB=2,numK=1,numDST=1,  numFLEX=0, out=c())  
```

```
##                 Player ADP_est ADPSD_est Pos ADP_Rank     HALF      STD      PPR    ADP_sim Slot
## 1         Alvin Kamara    6.00      1.25  RB      6.0 245.4832 235.7970 255.2076   4.974415    4
## 2  Christian Mccaffrey   20.15      3.10  RB     20.0 213.4223 203.1830 223.5142  21.991094   21
## 3          Tyreek Hill   29.65      3.40  WR     29.0 184.3552 165.8817 202.8286  28.024290   28
## 4          Mark Ingram   48.15      5.65  RB     50.0 154.7205 148.2986 161.1322  53.128259   45
## 5     Demaryius Thomas   46.00      4.50  WR     47.5 166.1272 150.4034 181.8510  49.458398   52
## 6         Carson Wentz   67.30      7.60  QB     68.0 287.7429 287.7384 287.7473  78.438217   69
## 7       Delanie Walker   79.15      8.00  TE     80.0 132.7886 120.0308 145.5463  74.433832   76
## 8          Cooper Kupp  100.70      8.40  WR    100.0 149.4343 133.5061 165.3624  96.723435   93
## 9       Robby Anderson  104.65      8.20  WR    107.0 145.7251 129.9623 161.4879 110.527001  100
## 10    Matthew Stafford  104.50     10.40  QB    106.0 268.6257 268.6231 268.6284 122.879788  117
## 11    Sterling Shepard  119.10      9.10  WR    125.0 139.7021 124.8505 154.5536 118.188264  124
## 12    Rishard Matthews  142.75     11.85  WR    146.0 133.4577 119.3678 147.5476 133.852795  141
## 13        Bilal Powell  148.95     13.15  RB    153.5 107.4629 102.5156 112.3073 144.767401  148
## 14                 Pit  163.35     10.80 DST    186.0 122.0000 122.0000 122.0000 161.635450  165
## 15     Harrison Butker      NA        NA   K    500.0 145.3158 145.3158 145.3158 500.000000  172
```

</br>You can see above that even though McCaffrey has the 20th ranked ADP, I am still able to get him with my round 2 pick (pick 21) in this draft simulation, something I wouldn't have been able to account for in my original post. 
I repeat the draft several times to get several potential drafts given my specified parameters.


```r
drafts<-replicate(250,  simDraft(slot="Slot4", scoring="HALF"), simplify = F) 
```

[Becker & Sun (2013)](https://www.degruyter.com/view/j/jqas.2016.12.issue-1/jqas-2013-0009/jqas-2013-0009.xml?format=INT)[^1] also did a draft algorithm with probabilistic picks, which had some useful ideas. One thing they did was have a parameter called alpha which determined how conservative or risky to plan their future picks. Similar to that, I will use the shift parameter from my last post, which just shifted everyone's ADP up or down. Having this parameter will allow me test if a more conservative approach to planning future  picks is beneficial.

After repeating the draft several times, I can apply the simulations to each of my optimal drafts to get mean simulated top lineups for my specified draft parameters. In this case, I'd have 250 drafts x 100 sims = 25,000 total simulated lineups.


```r
simScores<-lapply(drafts, function(x)replicate(100,simSeason(x, scoring="HALF")))
```

---

## Parameter Tuning (2 WR Leagues)


Finally, I am ready to test out different drafting parameters (ex: numWR, numRB, alpha, zeroRB, zeroQB,alpha, etc.). Below are plots of the mean-simulated starting lineups for different drafting parameters. My sample size is 250 simulated drafts x 100 simulated seasons = 25,000 simulations for each specified parameter. I chose this number because it was sufficient for the mean-simulated lineup results to stabilize.

















