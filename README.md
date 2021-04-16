# Rising Temperatures and Falling Corn Crops: Armageddon or Opportunity? 
 In this repo, Natalie Williams and Kimberly Li present their code for their final project in STAT5206 with Professor Wayne Lee
 
### Background 
* Climate change is expected to shift the human climate niche and by extension crop output northward 
* The human climate niche is the most hospitable region for humans to live and grow food. Currently the heart of the US's niche is south eastern states. 
* However, studies published in the *National Academy of Sciences* and *Science* stimulate potential movements of the human climate niche under potential emmissions scenarios 
* Sources: [Xu et al.](https://www.pnas.org/content/117/21/11350#sec-1) & [Hsiang et al. 2017](http://www.impactlab.org/research/estimating-economic-damage-from-climate-change-in-the-united-states/) 
* These studies show that in the next 50-70 years, the american Midwest will see significant changes in crop yields, particularly changes in North Dakota and Oklahoma and Arkansas. Northern states are predicted to see increases in crop yields, due to the human climate niche shifting upwards, conversly southern states will see losses. 
* Climate change will have a dramatic impact on the global and US economy. 
* Every increase of 1 ℃ could incur losses around 1.2% of Gross Domestic Product (GDP) for the US
  * However these losses will not be evenly dissipated throughout the country.
  * Southern counties are predicted to feel the bulk of this effect, with some counties potentially losing 20% of their income

### Research Question 
With this background in mind, we sought to find out if the trends predicted by these papers could already be seen.  
How have corn crop yields changed in northern and southern American states and are these potential trends correlated to changes in state economy GDP's. 

### Data Sources 
To accomplish this, we used data from the [USDA NASS](https://quickstats.nass.usda.gov/) and [Bureau of Economic Analysis](https://www.bea.gov/data/gdp/gdp-state) and [USDA ERS](https://www.ers.usda.gov/data-products/agricultural-productivity-in-the-us/). We then cleaned these data source and grouped data by state and year (See ./lib/).

### Methodology 
*Why Corn?* 
* Corn is grown on a massive scale in the US with [90 million acres used for its production](https://www.ers.usda.gov/topics/crops/corn-and-other-feedgrains/feedgrains-sector-at-a-glance/). Corn is used to feed people, livestock and even for biofuels. Corn was determined to be a good proxy for overall crop output by comparing [USDA NASS](https://quickstats.nass.usda.gov/) and [USDA ERS](https://www.ers.usda.gov/data-products/agricultural-productivity-in-the-us/) data. The ERS source has total state crop output measured relative to Alabama 1996. This would've been a superior data source then the NASS corn surverys **if** the ERS source was more clear on the metrics determining their calculations, and if it extended past 2004. 

*Our choice of regions*
Instead of focusing on the corn belt as a whole, we chose to analyse southern states and northern states. Northern states consist of  Minnesota, North Dakota, South Dakota, and Wisconsin. Southern states consist of Arkansas, Oklahoma, Tennessee and Texas. Note that the strongest players in the corn belt, i.e. Iowa, were not investigated because our focus is on states predicted to see the greatest differences in crop yields in the next 50 years, and these states are concentrated in the very north, and in the very south. [Choice of regions](figs/final/regions_map.png)

*Models Used*
Two models were used to fit trends, the linear “r::lm” model and the generalized additive model “r::gam”. In cases where a linear fit was not suitable (see Figure 2), the GAM was used. This was an appropriate fit because the data used was not very large, and a 95% confidence interval could be produced. 


### Results and Conclusions

Climatologists agree that the next 30 to 50 years will see a shift in the human climate niche if emissions are not corralled. With this in mind, the vulnerability of southern states economies to a decrease in crop yield was tested. Findings indicate that potential effects of shifting climates are already being seen in corn crop yields.
* [Figure 1](figs/final/totals_v_years_with_lines.png) 
* [Figure 2](figs/final/totals_v_years_with_lines.png) 
* [Figure 3](figs/final/totals_v_years_with_lines.png) 
* [Figure 4](figs/final/totals_v_years_with_lines.png)
* [Figure 5](figs/final/totals_v_years_with_lines.png)  
* [Figure 6](figs/final/totals_v_years_with_lines.png) 

More specifically, northern states have seen consistent gains in productivity while some southern states have started seeing dramatic decreases (Figure 1). Since no single cause could be identified for this downturn,  it is also possible that climate change has nothing to do with Texas’s and Oklahoma’s decreasing corn yields. However, the result at least shows that northern states are only becoming increasingly better competitors in the corn market. If northern states continue to increase productivity faster than southern states, it may no longer be economically viable for southern states to manufacture corn due to specialization.
Moreover, southern states are not only producing corn at a less competitive rate, but have slower increases in total crop output, too (Figure 2). Similarly, no claim can be made that climate change is the particular driver of this trend, but for the billion dollar industry that is agriculture, the effects of crop loss would have repercussions across the whole US economy.

Back to the top, Xu et al. had also identified two states that are likely to see the most change in the next 50 years. Using these two as proxies for their regions, it’s possible to compare what the most dramatic trends might look like in the northern and southern states. Figures 5 and 6 pit these two against each other and the result is North Dakota out-competing Oklahoma and seeing the economic benefits of greater corn yields. If this can be representative of their respective regions, the south is losing competitiveness and should consider beginning transition to crops better suited to their new climate and economic needs.

### Recommendations

In sum, southern states are becoming less competitive players in the corn industry and could stand to lose billions of dollars if corn yields fall. These results are supported by the PNAS paper; as the human niche shifts northward, southern farmers will require more resources such as water to irrigate their crops.

Consequently, it is recommended that southern states should prepare themselves for incoming climate change by switching to more heat resistant crops and/or investing in climate change resistant economies. Furthermore, federal and state governments should seek to support farmers that may see losses in livelihood by subsidizing programs to help farmers switch to tougher crops. Then leaning even more into specialization by helping to move corn production entirely to the north could help alleviate potential crop loss strain.


### Limitations
This analysis has **many** limitations. 
1. Regions are by no means monoliths and there’s variance not only between states the way they were grouped in this discussion, but also within states themselves
2. There was also an awkward discontinuity in the GDP data, which made a time series analysis across our entire time period of interest difficult; there was no easy way to bridge the gap between the different industry definitions and measurements and they were thus treated separately, making trends less obvious
3. Corn's direct contribution to the economy was not evaluated, only compared to agricultural GDP trends
4. Finally, there are a considerable amount of confounding variables and general variability in this analysis. Weather events like El Niño generally result in warmer temperatures across the country--an effect that has greater potential to negatively impact the warmer southern states. 
5. Projected crop prices, weather, climate change, availability of resources and government policy are also some factors that affect corn and soybean production and these factors likely interact, thus determining causal relationships is beyond the scope of this paper.

- - - -
*Written on April 16, 2021*
Author Contact Information: 
Natalie Williams: naw2127@columbia.edu 
Kimberly Li: kl3081@barnard.edu
