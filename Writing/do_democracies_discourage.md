---
title: Do democracies discourage NGO cooperation?
author:
- name: Andrew Heiss
  affiliation: Duke University
  email: andrew.heiss@duke.edu
date: April 18, 2014
published: Incomplete Draft. Please do not cite without permission.
abstract: Nope.
...


Anecdotes of cooperating NGOs

Find good/bad governance democracy/non-democracy in ME, Asia
Case of NGOs cooperating in each of the 4 types
Can be the introduction, good section of the paper

Lots of cooperation in nondemocratic China; very little in Japan and South Korea
Little in Saudi Arabia, Syria; lots in Israel, Turkey


# Opening the black box of NGO behavior

Prior to Bob, nobody looked at NGOs as organizations - treated them as altruistic actors...

New work on explaining determinants of INGO effectiveness - Wong, Stroup, other Murdie, Carpenter, etc.
Little work has been done on the domestic contexts - where the INGOs work
Murdie:2013 does that - looks at quality of governance and how that impacts inter-NGO cooperation

Cooperation as proxy for effectiveness - type of strategy for building networks

QOG has a significant impact
Puzzlingly, democracy weakens that cooperation


Murdie finds that the quality of governance in a nation is a strong predictor of inter-NGO cooperation—as nations …

However, Murdie's statistical analysis also uncover an empirical puzzle. While the probability of inter-NGO cooperation increases with better governance and higher trust, cooperation is less likely in democratic regimes than in non-democracies. Murdie posits that because her sample is limited to non-Western regimes, democracies outside North America and Western Europe have newer NGO sectors with less trust between organization, perhaps because these organizations struggle for legitimacy and autonomy from their nascent democratic states. 




# Institutional proxies for civil society sector strength

Measuring civil society strength systematically is difficult - Johns Hopkins vs. Civicus thing
No data on age of sector, level of trust - WVS, Gallup, but not a lot of data
Same with NGO restrictions - government regulations on NGO funding and activities - that one paper + Bloodgood, Ron, and Prakash at ISA


Trust typically measured by WVS question on trust or Gallup question on wallets, but they don't go back that far. Murdie used quality of governance as a proxy for trust

Instead we can look at democratic institutions themselves, under the assumption that civil society is generally a part of democratic rule (citation needed). We can look at other proxy variables - inequality, wealth, age of democracy

## Hypotheses here

She posits that young NGO sectors are driving the democracy effect. There is no reliable way to measure NGO sector age, though. Indirectly use Knack, who finds income inequality and wealth are strong predictors of trust?

Younger NGO sectors less likely to cooperate

Democracies with less trust are less likely to cooperate

H1: newer democracies less likely to cooperate

H2: lower levels of democracy less likely to cooperate

H3: The location of cooperation has an effect on the likelihood of cooperation - regional effects


# Modeling institutional effects on NGO cooperation

## Dependent variable

In order to generate results that are comparable to Murdie's findings, I have maintained many of the original variables from her models. All three of my hypotheses rely on a measure of inter-NGO cooperation. As discussed previously, measuring civil society *strength* is difficult due to a disappointing dearth of data. Measuring civil society *activity* is equally challenging, as there is no standard metric for NGO actions undertaken. 

To remedy this, Murdie used event data methods—long popular in the conflict forecasting literature—to compile a new measure of inter-INGO cooperation. Event data uses natural language text processing to determine the main actors and actions in a reported news story, essentially determining "who did what to whom." To calculate the "who," Murdie compiled a list of the 33,524 INGOs listed in the 2001/2002 *Yearbook of International Organizations* and worked with Virtual Research Associates (VRA) to find all events where a listed INGO was mentioned in the Reuters Global News Service archives from 1990–2009, previously prepared and coded for event data analysis by the Integrated Data for Event Analysis Project (IDEA). The sample was then limited to events where NGOs were both the source ("who") and the target ("to whom"). Finally, events were filtered further by limiting the action ("did what") to cooperative terms (i.e. events where an NGO "criticized" another NGO were ignored, while events where an NGO "collaborated" or "advised" another NGO were preserved), and collapsed to a count of inter-NGO cooperation events for each country and year. 

Because this measure of cooperation is wholly reliant on a single source of event data, it is subject to some degree of selection bias. For a cooperative event to count, Reuters wire reporters must have taken some interest in the event and reported on it using the names of both organizations. As such, countries with reduced Reuters coverage (such as North Korea, which did permit country offices for foreign media organization during the time period under study) and countries that are naturally underreported (such as Central Asia) will underrepresent NGO activities, while countries or regions that are more salient (such as the Middle East during the sanctions in Iraq in the 1990s and the American war in Iraq in 2003) may overrepresent NGO action. In reality, the asymmetry in reporting may reflect an actual asymmetry in inter-NGO cooperation, since countries facing more salient conditions may indeed attract more concerted NGO responses. Murdie controls for this bias in part by including the overall number of NGO events (including single-NGO events and noncooperative inter-NGO events) as an independent variable in her models. Alternatively, it may be more accurate to control for the total number of reported events per country-year, which would normalize and rescale many of the countries that are under- or over-reported.^[This is the standard approach when using GDELT (see [http://gdeltproject.org/data.html](http://gdeltproject.org/data.html))] However, due to contract restrictions, the original raw event data is unavailable, so controlling for NGO events must suffice.

Given these biases and the fact that there is only one archival news source for events, there are no NGO-related events in 91% of the included country-years. While it is possible that NGOs do not cooperate in many nations, it is likely that reliance on Reuters data has led to gross underreporting of actual inter-INGO cooperation. Recent and forthcoming developments in event data methods and sources can potentially increase this model's robustness. For example, the GDELT project uses "tens of thousands of broadcast, print and online news sources from nearly every corner of the globe"^[http://gdeltproject.org/about.html#datasources] and thus clearly has better (and potentially less biased coverage). In spite of the vastness of its sources and coverage, however, GDELT was not designed to work with events surrounding NGOs (there are a host of actor codes for protest and conflict actors, but few for non-state or non-governmental actors), and ongoing legal issues have led to turnovers in ownership and have left the project's reliability in question.^[http://asecondmouse.wordpress.com/2014/02/14/the-legal-status-of-event-data/] Fortunately international relations scholars are currently developing a new open source (and legally reliable) framework for massive event data collection. In the future these newer sources can be used to replicate this and other event data-based research by Murdie and others and yield even more accurate results. However, as this more comprehensive event data is either suspect or not ready yet, the existing Reuters data must again suffice.

## Independent variables

While measures of civil society are severely lacking, there is fortunately a relatively established body of literature and data on measuring democratic institutions. My first two hypotheses deal directly with a country's overall institutionalization of democracy. For the first hypothesis, I measure democracy age by counting the number of consecutive years since each country scored a 6 or higher on the Polity IV scale.^[Rather than provide a dichotomous measure of democracy, the Policy IV project assigns democracy scores ranging from -10 to 10. Regime types labels are then assigned based on this score: autocracies (-10–-6), anocracies (-5–5), and democracies (6–10).] As seen in Figure X, most of the countries included in the dataset have had democratic regimes for fewer than 20 years. A handful of country-years exceeding 100 years of democracy were excluded from the figure, but included in the model.

![Years of consecutive democratic rule](../Figures/demdur.pdf)

Murdie's original article included a binary control variable to indicate whether a country's political regime was a democracy or an autocracy. However, it has been shown that while dichotimization is convenient, much of the nuance and complexity in assigning a label of regime type is lost when boiling a country's institutionalization into one of two categories.[@EpsteinEtAl:2006] Additionally, while categories of institutionalization are useful at large magnitudes (i.e. there is a clear difference between a country that scores a -8 on the Polity IV scale and one that scores a 6), marginal changes in democratization scores are often meaningless (i.e. a change from a level 5 anocracy to a level 6 democracy is rather imperceptible and more susceptible to rater subjectivity). Though Polity IV and the numerous other competing democracy scales tend to result in similar findings in spite of their reliance on differing methods and multiple raters, there is little consensus about which scale is the more reliable or "best." Additionally, each of these scales fails to account for uncertainty in their estimates of levels of democratic governance. The Unified Democracy Score (UDS) scale was created in 2010 to combat these deficiencies of democratic measurement.[@PemsteinMeserveMelton:2010] This innovative scale uses Bayesian estimation and simulation to generate aggregate democracy scores based on 10 other standard measurement scales (such as Polity IV). Instead of assigning each country a single score, the UDS provides every country-year with a posterior score distribution, including a mean and a median score, a standard deviation, and 95% confidence intervals. UDS scores range from -2–2, with more democratic nations receiving higher scores (see Figure X for the distribution of mean, non-simulated UDS scores).

![Mean UDS scores](../Figures/uds_dist.pdf)

To test my third hypotheses I include regional-level fixed effects,^[Western Europe, North America, Australia, and New Zealand excluded.] which Murdie omitted from her original models. Fixed effects are generally simply controlled for and ignored (and often not reported in regression tables). However, to determine whether regional differences in institutional settings and problems facing NGOs play a determining role on NGO behavior, I treat these regional fixed effects as actual reported coefficients. 

For the sake of comparability I also include many of Murdie's independent and control variables, each of which are described and cited in more detail in the original paper: (1) the Political Risk Services' quality of governance measure, which ranges from 0 (minimum governance) to 1 (high quality governance), (2) foreign aid per capita, (3) country population, (4) GDP per capita, and (5) the number of INGOs with members or volunteers in the country. Additionally, I include indicator variables for whether the country (6) underwent a humanitarian military intervention that year, or suffered a (7) natural disaster or (8) civil war in the past five years. As with Murdie's original models, all independent variables are lagged by one year.


# Results

In her original paper, Murdie used two forms of her inter-NGO cooperation variable: (1) a binary measure indicating whether cooperation occurred, and (2) a count measure indicating the number of cooperative events in a given country-year. Each of these forms were then used in models with different functional forms: (1) regular logistic and rare-event logistic for the binary measure, and (2) negative binomial and zero-inflated negative binomial for the count data. For the sake of simplicity, and because no country-year experienced more than four NGO events, I only use logistic regression models. 

Table 1 provides the results for Murdie's replicated logistic and rare-event models, followed by extensions of the model that test the hypotheses laid out previously. Columns 1 and 2 highlight the negative effect democracy has on NGO cooperation—all else equal, NGOs are about 50% less likely to collaborate when working in democratic countries. Figure X demonstrates this effect graphically, showing the predicted probabilities of NGO cooperation along the full range of possible quality of governance scores (with all other model variables held at their means). While democracies do indeed have lower predicted probabilities, the prediction line falls within the 95% confidence interval for dictatorships, indicating that the variable's significance is likely mathematic and not substantive.

![Predicted probabilities of inter-NGO cooperation for democracies and dictatorships](../Figures/pred_demo.pdf)

\begin{landscape}
\input{../Tables/all_models.tex}
\end{landscape}

The findings from my expanded model are included in columns 3 and 4. To test the hypotheses of whether the age and level or quality of a democracy explain some variation of inter-NGO cooperation, I have replaced the binary "dictatorship/democracy" measure with the mean UDS score and the number of years of consecutive democracy. Despite the theoretical supposition that younger democracies face barriers to high quality governance such as increased corruption, weakened rule of law, and a weaker (and less trusting) civil society sector, the duration of democratic rule does not have a significant impact on the probability of cooperation (z = -1.81, p = 0.071). If we use a 90% confidence threshold, democratic age is significant, but not substantively so—each year of consecutive democratic rule decreases the probability of cooperation by 2%. Even if we use this lower threshold, the changes in probability are not substantive—Figure X shows the predicted probabilities of cooperation for varying durations of democracy (with all other values held at the mean), and as in Figure X, there are no significant differences in prediction. There is therefore insubstantial evidence that younger democracies are more likely to see INGO cooperation. 

![Predicted probabilities of inter-NGO cooperation for varying years of consecutive democracy](../Figures/pred_demdur.pdf)

The models provide a similar conclusion for the hypothesis that higher levels of democracy see less NGO cooperation. The estimated coefficient for mean UDS scores is marginally smaller than the original binary indicator (countries are 41% less likely to see inter-NGO cooperation for each one-point increase in UDS scores), but this finding is no longer significant (z = -1.94, p = 0.053). One advantage to using the UDS scores, though, is that the scores are actually posterior probability distributions, which permits simulation and resampling. Figure X shows the estimated coefficients for the UDS and quality of governance where the logistic regression model was run 100 times using simulated UDS scores, generated using the mean and standard deviation of the UDS's posterior distribution. The violin plots (density plots mirrored horizontally)[@HintzeNelson:1998] demonstrate the distribution of key model coefficients, while the dark grey lines show the simulated 95% confidence intervals. As seen in the plot, simulated UDS scores can result in marginally different coefficient estimates that consistently remain negative, ranging from -0.16 to -0.82 (log odds), which shows that countries are 15%–56% less likely to experience inter-NGO cooperation as they increase in UDS scores. However, the UDS coefficient in most simulations remains insignificant at a 95% confidence level. 

![Simulated model coefficients after 100 random draws of UDS scores](../Figures/simulated_uds.pdf)

The figure does show that quality of governance—Murdie's primary variable of interest—maintains its substantive significance regardless of simulated UDS. In fact, the governance coefficient increases significantly when controlling for UDS and democratic duration, further bolstering Murdie's claim that high quality governance provides a more amenable environment for inter-NGO cooperation. However, given the lack of evidence otherwise, I find little evidence for Hypotheses 1 and 2—that younger and weaker democracies will see less cooperation. Democratic institutions in general do not appear have a substantively significant[@EsareyDanneman:2014] impact on the probability of NGO cooperation, despite Murdie's finding of significance in her original models.

Although actual democratic institutions may not explain variation in collaboration, unmeasured region- or country-specific conditions are more successful in predicting inter-NGO cooperation. In her original models, Murdie only included fixed effects variables to control for time, capturing effects that may be specific to certain years—perhaps some years were inherently more amenable to cooperation than others. To test my third hypothesis—that regional differences affect the probability of cooperation—I included regional fixed effects in columns 5 and 6 of Table 1. Filtering out region-specific characteristics increased the quality of governance effect more, but had no significant influence on the potential impact of UDS scores or consecutive years of democracy. Rather than keep these fixed effects hidden, however, a richer story of regional effects can be told by looking at the effects of the model in each region. 

To see these individual regional effects, I estimated separate regression models for a subset of data for each region. Running multiple models rather than simply including the interaction terms for each region in the original model essentially allows for every possible interaction between region and other variables included in the model—instead of simply shifting the intercept or slope for each region, individual regional models ostensibly reflect the full impact of the region on the aggregate model as a whole. However, running separate models presents a mathematical challenge, as subsetting the data into smaller groups decreases the number of observations available to model and increases the number of observations dropped due to missing data. To maintain the separate models' statistical power, I was forced to both ignore some regions and remove some control variables in the model. I dropped regions where there were fewer than 80 observations; removed indicators for humanitarian interventions, natural disasters, and civil conflicts; and dropped variables measuring foreign aid per capita and the number of INGO members in the country. Some regions in the sample did not suffer from disaster or conflict, and others received little (or unreported) foreign aid, thus weakening their region-specific models too much to return useful results.

With these important caveats, results from the individual regional models are presented in Table 2. Region-specific differences appear to play a crucial role in the reliability of the previously specified models. For example, quality of governance plays an extremely important role in predicting inter-INGO cooperation in Latin America, but not as strongly (if at all) in other regions. Similarly, democracy (as measured by UDS scores) has a significant negative impact in Latin America alone—all other regions balance out the aggregate coefficients reported in Table 1. However, even with these changes in significant coefficients, the substantive significance of the regional models remains suspect. The differences in regions are perhaps best understood using predicted probabilities. As seen in Figure X, the individual models for Eastern Europe and Latin America fail to make reliable predictions. Cooperation is actually more likely in the Middle East and North Africa as democracy increases, yet less likely in Southeast Asia and Sub-Saharan Africa—averaging the disparate regional effects eventually yields the original negative democratic effect.

![Predicted probabilities of inter-NGO cooperation for varying levels of democracy (UDS) by region](../Figures/pred_region_uds.pdf)

\begin{landscape}
\input{../Tables/regional_models.tex}
\end{landscape}


# Conclusion

Nope. Democracies don't inherently discourage cooperation. Other factors—regional specific ones, quality of governance, etc.—do, but regime type doesn't.


The counterintuitive democracy effect originally found by Murdie may be attributable to regional and country-level differences. Turkey and Israel—both in the Middle East—are democracies with relatively high UDS scores, and both saw more cooperative events than democracies in other regions. 
China, not democratic but a hotbed for collaboration over human rights - blah blah

Except maybe in specific regions?


# References
\setlength{\parindent}{-0.2in}
\setlength{\leftskip}{0.2in}
\setlength{\parskip}{8pt}
\vspace*{-0.2in}
\noindent