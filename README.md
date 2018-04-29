## Observations

### Nature of the data
1. The dataset appears like a sample of the traffic in March rather than a population, because neither weekly nor hourly fluctuation is pronounced (see `./images/cmp_pub_pvs_by_hour.png`). 
2. In addition, `visit_hour` ranges only from 8 to 19. 
3. As the sampling method is unknown and some traffic data are trimmed by hour, some `url_id`s may not have a full record of visits. 


### Possible predictors of pageviews
1. Assume every `url_id` has a full record and the sample well represents the population, choose `hourly_pageviews` as the outcome and test its relations with `age`, `section`, `visit_weekday` and `visit_hour`. `age` is measured as the time difference between `publication_date` and `visit_date`. 
2. All the included predictors are significant as the sample is large. P-value adjustments can be applied to address this issue, such as Bonferroni Correction, but not run here. 
3. The statistical method adopted here is `negative binomial regression`, because it handles over-dispersion better than Poisson regression, which is the case here. A test is run to confirm this choice. 
4. The coefficients and confidence intervals are included in the code and two charts are created to show `hourly_pageviews` is associated with `age` and `visit_weekday`. As there is no interaction term is introduced to the analysis, such as `section`x`weekday`, weekly pageviews fluctuate proportionally across sections (see `./images/predicted_pvs_by_section_weekday.png`). 
5. Although `politics` gets the most pageviews in general, the `magazine` section may gain even more when it's first published (see `images/predicted_pvs_by_section_age.png`), which suggests further analyses with interaction terms, such as `section`x`age`. 
6. The homepage '/' is excluded for the chart about `age`, as it's constantly updated and its age doesn't make mucn sense.


### Evergreen content
1. If the sample is representative, we can see 40% of the stories are a week old or younger while 50% are 1 to 3 weeks old, which suggests a strong presence of evergreen content (see `./images/age_hist.png` and `./images/age_ecdf.png`). 
2. Some sections may have a longer shelflife than the others. The `health` section, for instance, still attracted traffic over a month old (see `./images/cmp_pvs_section_age.png`). 

