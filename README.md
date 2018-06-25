Optimization for Fantasy Football Snake Drafts
---------
-   [Base Case](#base-case)
-   [More Complex Case](#more-complex-case)
    -   [Error Analysis](#error-analysis)
    -   [Simulation](#simulation)
    -   [Optimizing Parameters](#optimizing-parameters)
-   [Conclusion](#conclusion)

------------------------------------------------------------------------

Base Case
---------

Optimizing your strategy for a fantasy football draft is an interesting problem. Should you should go RB early? Wait on RB's? Should you do something productive with your free time instead of fantasy football? (probably). Optimizing your draft picks can be viewed as an optimization where you try to maximize the projected points of your selected players. In this post I will go through my optimization methodology. To start, I have a dataframe of players and their projected points and their ADPs (Average draft positions).

``` r
head(adp, 20)
```

    ##                  Player ADP_est ADPSD_est Pos ADP_Rank     HALF      STD      PPR
    ## 648         Todd Gurley    1.65      0.85  RB        1 282.7787 253.1955 312.4844
    ## 439         Leveon Bell    2.65      1.15  RB        2 279.5328 241.1337 317.9263
    ## 188       David Johnson    4.00      1.20  RB        3 231.1158 200.3482 261.8851
    ## 258     Ezekiel Elliott    4.30      1.85  RB        4 243.0664 225.0725 261.2155
    ## 39        Antonio Brown    5.15      1.45  WR        5 249.7020 196.4496 303.0005
    ## 27         Alvin Kamara    6.05      1.70  RB        6 243.6166 209.5970 277.6742
    ## 598      Saquon Barkley    6.55      1.70  RB        7 208.1787 183.8469 232.5106
    ## 193     Deandre Hopkins    9.05      1.85  WR        8 201.0354 158.6881 243.4242
    ## 399         Kareem Hunt    9.20      1.95  RB        9 220.6709 196.2797 245.1176
    ## 436   Leonard Fournette    9.85      1.90  RB       10 215.2420 197.5708 232.9848
    ## 493       Melvin Gordon   10.40      1.85  RB       11 210.6997 186.6252 234.7998
    ## 532       Odell Beckham   11.30      1.95  WR       12 205.2866 163.3321 247.2449
    ## 167         Dalvin Cook   13.15      2.05  RB       13 202.0279 181.9711 222.1079
    ## 385         Julio Jones   14.05      2.20  WR       14 221.9109 175.9947 267.9012
    ## 506      Michael Thomas   16.80      2.50  WR       15 205.0079 160.0206 250.1222
    ## 186       Davante Adams   17.70      2.60  WR       16 164.2036 129.6687 198.7782
    ## 223     Devonta Freeman   18.25      2.40  RB       17 200.8666 179.3294 222.4049
    ## 402        Keenan Allen   18.80      2.50  WR       18 167.9322 129.4230 206.3104
    ## 438        Lesean Mccoy   18.85      2.85  RB       19 212.4505 186.0969 238.7998
    ## 133 Christian Mccaffrey   19.35      3.00  RB       20 208.6223 172.7164 244.3809

<br />For league settings, I am using Yahoo's defaults[1]. I can then easily set up an optimization where I say to maximize the sum of the projected points of the 15 players taken. Given the slot I am picking at, which for this example I will say is slot 4, I just constrain it to take 15 players with ADP&gt;=4, 14 players with ADP&gt;=21, etc. The end result is a function which takes different parameters for the optimization and returns the optimal draft picks.

``` r
getPicks(slot="Slot4", numRB=4, numWR = 6,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')
```

    ##                      Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27             Alvin Kamara    6.05      6.0  RB 243.6166    4
    ## 340         Jerick Mckinnon   22.25     24.0  RB 188.5646   21
    ## 680             Tyreek Hill   30.20     30.0  WR 186.7585   28
    ## 71            Brandin Cooks   48.30     51.0  WR 183.8862   45
    ## 428            Lamar Miller   48.45     52.0  RB 160.6619   52
    ## 367            Jordy Nelson   75.80     78.0  WR 168.1870   69
    ## 384          Julian Edelman   76.25     79.0  WR 152.4598   76
    ## 315         Jamison Crowder  101.25    100.0  WR 150.6055   93
    ## 406         Kelvin Benjamin  113.80    116.0  WR 146.6154  100
    ## 59  Benjamin Roethlisberger  120.25    125.0  QB 268.8837  117
    ## 295              Jack Doyle  130.50    135.0  TE 121.0421  124
    ## 313             James White  147.80    151.0  RB 137.4044  141
    ## 22          Alexander Smith  151.25    160.0  QB 266.7094  148
    ## 547                     Pit  165.45    190.5 DST 120.0000  165
    ## 282         Harrison Butker  167.65    193.0   K 145.3158  172

<br />The parameters of getPicks() specify number of players at each position to take. I also added the shift parameter which can shift everyone's ADP by a given fraction i.e. shift=.1 would add 10% to everyone's ADP. Rotoviz [already has an app](http://rotoviz.com/2017/08/using-the-rotoviz-draft-optimizer-to-dominate-your-ppr-draft/) which does a similar optimization. The results do seem to suggest certain things like how you should often take RB's early. Looking at the optimal first two picks for each draft slot, you can see how RB's are usually suggested for the early picks:

``` r
sapply(paste0("Slot", 1:12), function(x) getPicks(slot=x, numRB=4, numWR = 6,numTE=1,numK=1,numQB=2, numDST=1,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')[1:2,],simplify = FALSE,USE.NAMES = TRUE)
```

    ## $Slot1
    ##              Player ADP_est ADP_Rank Pos     HALF Slot
    ## 648     Todd Gurley    1.65        1  RB 282.7787    1
    ## 340 Jerick Mckinnon   22.25       24  RB 188.5646   24
    ## 
    ## $Slot2
    ##              Player ADP_est ADP_Rank Pos     HALF Slot
    ## 439     Leveon Bell    2.65        2  RB 279.5328    2
    ## 340 Jerick Mckinnon   22.25       24  RB 188.5646   23
    ## 
    ## $Slot3
    ##              Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27     Alvin Kamara    6.05        6  RB 243.6166    3
    ## 340 Jerick Mckinnon   22.25       24  RB 188.5646   22
    ## 
    ## $Slot4
    ##              Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27     Alvin Kamara    6.05        6  RB 243.6166    4
    ## 340 Jerick Mckinnon   22.25       24  RB 188.5646   21
    ## 
    ## $Slot5
    ##                  Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27         Alvin Kamara    6.05        6  RB 243.6166    5
    ## 133 Christian Mccaffrey   19.35       20  RB 208.6223   20
    ## 
    ## $Slot6
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27  Alvin Kamara    6.05        6  RB 243.6166    6
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   19
    ## 
    ## $Slot7
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 399  Kareem Hunt    9.20        9  RB 220.6709    7
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   18
    ## 
    ## $Slot8
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 399  Kareem Hunt    9.20        9  RB 220.6709    8
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   17
    ## 
    ## $Slot9
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 399  Kareem Hunt    9.20        9  RB 220.6709    9
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   16
    ## 
    ## $Slot10
    ##                Player ADP_est ADP_Rank Pos     HALF Slot
    ## 436 Leonard Fournette    9.85       10  RB 215.2420   10
    ## 438      Lesean Mccoy   18.85       19  RB 212.4505   15
    ## 
    ## $Slot11
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 385  Julio Jones   14.05       14  WR 221.9109   11
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   14
    ## 
    ## $Slot12
    ##           Player ADP_est ADP_Rank Pos     HALF Slot
    ## 385  Julio Jones   14.05       14  WR 221.9109   12
    ## 438 Lesean Mccoy   18.85       19  RB 212.4505   13

<br />There are some shortcomings with using this basic optimization to inform your strategy. First of all, the thing you want to optimize is not all of your picks' points--**a more appropriate objective would be to draft in a way that will give you the eventual best starting lineup**. This would ideally take into account the uncertainty of the projections, the fact that you need to start different number of positions, and the possibility of getting waiver wire adds. I'll show how I account for this below.

------------------------------------------------------------------------

More Complex Case
-----------------

For the more complicated case, my methodology will be to get an optimal lineup, but I will evaluate its performance not by the sum of the projected points, but rather by how strong it's mean-simulated top starting lineup is. I will explain this in depth later. To do this method though, I will first need estimates of the errors of the projections. It is widely assumed that RBs have errors with high variance while something like a TE has low variance, and so this should be accounted for when simulating the actual values from the projections.

### Error Analysis

I summarize below the projection error (actual-projected) for 2012-2017, grouped by projection range and position.

    ##    fantPts_bin Pos         mean      median       sd   n
    ## 12    (50,100] DST  16.50746269  11.0000000 29.04822  67
    ## 22   (100,150] DST  17.42622951  15.0000000 30.41297  61
    ## 23   (100,150]   K  -1.92049545   2.8375140 32.26448 166
    ## 6     (-50,50]  QB  12.24430746  -2.3085314 47.34714 164
    ## 15    (50,100]  QB  12.96164604  -6.4223763 68.02034  35
    ## 31   (150,200]  QB  -3.71707735 -32.1527604 87.13484  24
    ## 35   (200,250]  QB -11.83994287  -2.3260519 70.04558  58
    ## 39   (250,400]  QB  -9.22522616   3.9422639 70.68462  93
    ## 7     (-50,50]  RB   6.58141184  -0.4866269 31.77453 338
    ## 16    (50,100]  RB   2.79001375  -6.7112516 53.26364 194
    ## 26   (100,150]  RB -22.09952819 -23.1663667 62.26261 138
    ## 32   (150,200]  RB -19.30728640 -12.5154457 71.29497  86
    ## 36   (200,250]  RB -35.07334805 -34.6574039 93.87923  42
    ## 8     (-50,50]  TE  -0.01226137  -4.7333753 24.48580 322
    ## 17    (50,100]  TE  -4.80893126  -6.4069032 39.81488 149
    ## 27   (100,150]  TE -11.41815408  -4.4928840 46.09589  78
    ## 9     (-50,50]  WR   4.24743389  -4.2280372 34.12923 379
    ## 18    (50,100]  WR  -0.05253393 -10.4939524 48.37551 279
    ## 28   (100,150]  WR -14.58023486 -17.6677955 52.90124 190
    ## 34   (150,200]  WR -12.30101110 -13.3453151 55.85909 125
    ## 38   (200,250]  WR -20.18237638 -12.7166185 67.41880  44

Plotting the above standard deviations of the errors by position:

<img src="base_case_optimization_files/figure-markdown_github/fig1-1.png" style="display: block; margin: auto;" />

I can also look at an example plot of one of these subgroups ex: WRs with projections between 150-200:

<img src="base_case_optimization_files/figure-markdown_github/fig2-1.png" style="display: block; margin: auto;" />

Looking at the above data and plots, I see how error variance is a function of position and projection. Looking at the example histogram above, the errors do appear pretty normally distributed. I should mention that in the table I do see that there appears to be bias in my projections for certain positions (negative mean/median value implies projections that are too high), I will revisit this at another time and for now assume unbiased errors. Below is an overview of the assumptions I am making when I simulate the seasons.

**Assumption 1.** For simplicity I am assuming that errors are normally distributed with mean 0 and a standard deviation based on the SD for their position and projection.<br /> **Assumption 2.** I also assume that you will be able to pick up undrafted players. I assume you will be able to get the third highest performing undrafted player at each position. This may be aggressive but it's likely that if you only need DST and TE mid-season, you will be able to get a strong one at both.

### Simulation

Finally, I am ready to simulate a season from my optimal lineup.

First I get the optimal picks at Slot=4/12, same as in base case:

    ##                      Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27             Alvin Kamara    6.05      6.0  RB 243.6166    4
    ## 340         Jerick Mckinnon   22.25     24.0  RB 188.5646   21
    ## 680             Tyreek Hill   30.20     30.0  WR 186.7585   28
    ## 71            Brandin Cooks   48.30     51.0  WR 183.8862   45
    ## 428            Lamar Miller   48.45     52.0  RB 160.6619   52
    ## 367            Jordy Nelson   75.80     78.0  WR 168.1870   69
    ## 384          Julian Edelman   76.25     79.0  WR 152.4598   76
    ## 315         Jamison Crowder  101.25    100.0  WR 150.6055   93
    ## 406         Kelvin Benjamin  113.80    116.0  WR 146.6154  100
    ## 59  Benjamin Roethlisberger  120.25    125.0  QB 268.8837  117
    ## 295              Jack Doyle  130.50    135.0  TE 121.0421  124
    ## 313             James White  147.80    151.0  RB 137.4044  141
    ## 22          Alexander Smith  151.25    160.0  QB 266.7094  148
    ## 547                     Pit  165.45    190.5 DST 120.0000  165
    ## 282         Harrison Butker  167.65    193.0   K 145.3158  172

Then I can get the top starting lineup from 1 simulation, Projected Points=HALF. Simulated Points=Sim:

    ##             Player ADP_est ADP_Rank Pos      HALF Slot     Score  ScoreSD      Sim
    ## 2  Jerick Mckinnon   22.25       24  RB 188.56459   21 188.56459 76.95688 185.2032
    ## 3      Tyreek Hill   30.20       30  WR 186.75852   28 186.75852 60.51378 225.5177
    ## 4    Brandin Cooks   48.30       51  WR 183.88622   45 183.88622 60.08293 295.7044
    ## 12     James White  147.80      151  RB 137.40439  141 137.40439 63.31245 238.5547
    ## 13 Alexander Smith  151.25      160  QB 266.70944  148 266.70944 70.00000 379.6933
    ## 18   Quincy Enunwa      NA      500  WR 107.16926   NA 107.16926 48.57539 178.3922
    ## 19     Jesse James      NA      500  TE  56.72271   NA  56.72271 36.50841 117.2310
    ## 20             Oak      NA      500 DST  89.00000   NA  89.00000 30.00000 152.5625
    ## 21     Dan Carlson      NA      500   K 111.60000   NA 111.60000 30.00000 163.4856

Finally, I can repeat this a large number of times to get the mean-simulated optimal lineup from a set of picks.

### Optimizing Parameters

The last step of the system is to test different parameters. I can specify things like number of players to take at each position or whether I should lock in a certain player. I repeat the above simulation many times and I want to find the parameters that result in the best mean-simulated optimal lineup. Below I plot the simulation results for different parameter combinations. <br /> <br />

![](Base%20Case%20Parameter%20Plot.jpeg)

<br /> In the plot you can see the effect of different actions. For example, it suggests you should definitely take 2 QBs, as the 1 QB test (case 5) performs very poorly. Taking Antonio Brown instead of Kamara in round 1 slightly decreases the median-simulated starting lineup, despite Antonio Brown's raw projection actually being higher than Kamara's. The planned draft from the optimal parameter combo (case 4) is shown below.

``` r
getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=2,numFLEX = 0,shift=0,  out=c(), fix=c(), scoring='HALF')
```

    ##                      Player ADP_est ADP_Rank Pos     HALF Slot
    ## 27             Alvin Kamara    6.05      6.0  RB 243.6166    4
    ## 340         Jerick Mckinnon   22.25     24.0  RB 188.5646   21
    ## 680             Tyreek Hill   30.20     30.0  WR 186.7585   28
    ## 71            Brandin Cooks   48.30     51.0  WR 183.8862   45
    ## 428            Lamar Miller   48.45     52.0  RB 160.6619   52
    ## 367            Jordy Nelson   75.80     78.0  WR 168.1870   69
    ## 201          Delanie Walker   78.55     81.0  TE 133.6074   76
    ## 315         Jamison Crowder  101.25    100.0  WR 150.6055   93
    ## 489        Matthew Stafford  104.05    106.0  QB 276.5724  100
    ## 59  Benjamin Roethlisberger  120.25    125.0  QB 268.8837  117
    ## 295              Jack Doyle  130.50    135.0  TE 121.0421  124
    ## 55                      Bal  142.45    144.0 DST 122.0000  141
    ## 313             James White  147.80    151.0  RB 137.4044  148
    ## 547                     Pit  165.45    190.5 DST 120.0000  165
    ## 282         Harrison Butker  167.65    193.0   K 145.3158  172

<br /> For comparison, below is the one with Antonio Brown:

``` r
getPicks(slot="Slot4", numRB=4, numWR = 4,numTE=2,numK=1,numQB=2, numDST=1,numFLEX = 1,shift=0,  out=c(), fix="Antonio Brown", scoring='HALF')
```

    ##               Player ADP_est ADP_Rank Pos     HALF Slot
    ## 39     Antonio Brown    5.15      5.0  WR 249.7020    4
    ## 340  Jerick Mckinnon   22.25     24.0  RB 188.5646   21
    ## 680      Tyreek Hill   30.20     30.0  WR 186.7585   28
    ## 71     Brandin Cooks   48.30     51.0  WR 183.8862   45
    ## 428     Lamar Miller   48.45     52.0  RB 160.6619   52
    ## 367     Jordy Nelson   75.80     78.0  WR 168.1870   69
    ## 201   Delanie Walker   78.55     81.0  TE 133.6074   76
    ## 315  Jamison Crowder  101.25    100.0  WR 150.6055   93
    ## 489 Matthew Stafford  104.05    106.0  QB 276.5724  100
    ## 224  Devontae Booker  119.75    124.0  RB 119.8159  117
    ## 295       Jack Doyle  130.50    135.0  TE 121.0421  124
    ## 313      James White  147.80    151.0  RB 137.4044  141
    ## 22   Alexander Smith  151.25    160.0  QB 266.7094  148
    ## 547              Pit  165.45    190.5 DST 120.0000  165
    ## 282  Harrison Butker  167.65    193.0   K 145.3158  172

------------------------------------------------------------------------

Conclusion
----------

In conclusion, I created a system that optimizes to get the best eventual starting lineup for fantasy football. In testing different strategies, it seems you should definitely draft 2 QBs, and probably draft RB first, though that is less clear. One main flaw still remaining in all of this is the uncertainty in opponent picks. If an optimal strategy depends on getting a high value QB in round 10 for example, it should factor in what happens if someone else takes the QB. In addition, I should look into the projection bias because if Kamara's projection is biased more than Antonio Brown's that will make a big difference. I will look into these things in a future post. <br /><br />

[1] 12-team league with 15 picks per team. Positions=1 QB, 2 WR, 2 RB, 1 TE, 1 FLEX, 1 DST, 1 K. Scoring = .5 PPR
