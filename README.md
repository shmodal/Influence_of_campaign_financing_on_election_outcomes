# Influence of campaign financing on election outcomes

In this project, we aim to use campaign financing data - specifically, direct contribution amounts from Political Action Committees (PACs) to 
candidates - to forecast election outcomes for congressional candidates. A PAC is an organization that pools 
campaign contributions from members and donates those funds to campaigns for or against candidates, ballot initiatives, or legislation.


We obtained our data from the 
[Center for Responsive Politics](http://www.opensecrets.org),
which is a non-profit, non-partisan research group based in Washington, D.C. 
It tracks the effects of money and lobbying on elections and public policy. We used data from the past three cycles: 2018, 2016, and 2014.


We first performed some exploratory data analysis to better understand our dataset ; in particular the distributions of
political parties and amount of contributions, according to the status of the candidate (incumbent or not), the outcome of the election, 
or the nature of PAC industries which contributed to the campaign. 
We then built several logistic regression models to predict the outcome of the elections from relevant variables.

After observing the strong apparent relationship between incumbency and amount of money raised, we decided to predict the status of the candidate by
the PAC contributions. We also used matching in order to measure the treatment effect of incumbency over success in election, accounting for the differences
in campaign contributions.