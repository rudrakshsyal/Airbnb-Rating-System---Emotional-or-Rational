# Airbnb-Rating-System---Emotional-or-Rational

As a customer or a potential investor who lists properties on Airbnb, one would always be interested in determining the quality of the listing. The rating score is one of the indicators which every stakeholder looks forward to, in order to gauge this metric. It is often observed that this is not an accurate indicator. As an example, the average rating for all listings across Airbnb stand at 4.7 with 94% of all the listings being rated above 4.5. This is much higher than ratings on other hotel sites like TripAdvisor.

## Hypothesis

There exists a better rating or scoring system that could be derived from the same amount of information obtained from the underlying emotions of reviewers and discounting their tendency to inflate ratings.  In other words, there is a way to get past the ‘noise’ in the reviews and get the actual rating value for a particular listing.

## The Approach

The data for Airbnb historical listings as well as customer reviews for each listing was collected. Data was cleaned and processed for further analysis. The next step involved using the Google NLP API which uses a sophisticated algorithm to perform Natural Language Processing (NLP) on each review and generates sentiment scores and magnitude of the emotional content of the reviews. The API supports various languages, but for those which are not supported, we used the Google Translate API to convert the text to English and then generate sentiment scores. Using these sentiment scores, we generate a new sentiment factor which is used to weigh the reviews provided to each listing.

## Observations

After performing the analysis, it was found that the new rating system was actually a better determinant of number of bookings keeping other factors of a listing fixed. This meant that the new rating system explained more variation in the booking rate of a listing as compared to the old rating values.

## Recommendations

For investors, the study recommends them to use the new rating system which accounts for user reviews and sentiments before investing money into any kind of property for high profitability.
For Airbnb, the study suggests updating the conventional rating system which consists of some underlying ‘noise’ and use the updated rating system which accounts for the emotional sentiments of users. Airbnb could approach this issue by displaying two ratings, the average rating as well as the average sentiment rating.

## Limitations

The study was conducted by rolling up historical data which was captured monthly, which meant that the booking metric was highly skewed. Hence lack of granular historical data caused limited confidence in one of the metrics. Had there been comprehensive and elemental data, the model would have higher accuracy in explaining the booking trends.

