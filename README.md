# TwitterShinyApp

Interactive Shiny App developed by Carlo R. M. A. Santagiustina for downloading and processing data from Twitter's Search API using R and the following R libraries (`shiny`,`Matrix`,`rjson`,`jsonlite`,`httr`,`rvest`,`tidyverse`,`shinyjs`,`DT`,`openxlsx`,`quanteda`,`rtweet`,`stringr`,`tibble`,`rtweet`,`shinythemes`,`ggplot2`,`quanteda`).
You can use the `TwitterShinyApp` to download up to 18000 tweets every 15min from the Twitter Search API (`get-search-tweets` endpoint), by `keyword` and/or by `geocode` (for more info on the endpoint see: <https://developer.twitter.com/en/docs/tweets/search/api-reference/get-search-tweets.html>).
The `TwitterShinyApp` also allows you to analyse and process through a user friendly interface downloaded tweets. Implemented functionalities include:

- `Summary statistics` by variable (column);
- `Ngrams` and `term/pattern` context analysis;
- `Time series` of tweets counts for multiple time frequencies (`weeks`, `days`, `hours`, `mins`, `secs`);
- Textual data cleaning (`text` and user `description`);
- Extraction of `URLs`;
- Extraction of `emoticones`;
- Extraction of hashtags (`#`), usertags (`@`) and fintags (`$`); 
- Hashtags and usertags frequency analysis and `wordclouds` for most frequent hashtags/usertags;
- Hashtag and usertag `cooccurrence network` (construction + visualization) for most frequent hashtags/usertags;


To run the app locally: 
1. Download the `app - nocredentials.R` file;
2. Install R version 3.5 (or later) and RStudio;
3. Open the `app - nocredentials.R` file in RStudio;
4. Install automatically the suggested packages in RStudio ( `install.packages("shiny","Matrix","rjson","jsonlite","httr","rvest","tidyverse","shinyjs","DT","openxlsx","quanteda","rtweet","stringr","tibble","rtweet","shinythemes","ggplot2","quanteda")` );
5. Insert your Twitter Developer App credentials (see your Twitter developer account credentials at: <https://developer.twitter.com/en/apps/> ) in the corresponding fields (see script lines 262, 264, 266, 268,270)
6. Run the Shiny app using the `Run App` button (scripting window top-right corner);
7. Enjoy!

Reminder: Twitter Search API limits allow up to 180 queries (18 000 tweets downloaded) every 15min. 

Apache Licence 2.0

contact: carlo.santagiustina@unive.it

