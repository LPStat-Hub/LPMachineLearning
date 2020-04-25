\name{onlineNews}
\alias{onlineNews}
\docType{data}
\title{
Online news popularity data.
}
\description{
Popularity study of online articles.
}
\usage{data(onlineNews)}
\format{
  A data frame with 39644 observations on the following 60 variables.
  \describe{
    \item{\code{x.timedelta}}{Days between the article publication and the dataset acquisition.}
    \item{\code{x.n_tokens_title}}{Number of words in the title.}
    \item{\code{x.n_tokens_content}}{Number of words in the content.}
    \item{\code{x.n_unique_tokens}}{Rate of unique words in the content.}
    \item{\code{x.n_non_stop_words}}{Rate of non-stop words in the content.}
    \item{\code{x.n_non_stop_unique_tokens}}{Rate of unique non-stop words in the content.}
    \item{\code{x.num_hrefs}}{Number of links.}
    \item{\code{x.num_self_hrefs}}{Number of links to other articles published by Mashable.}
    \item{\code{x.num_imgs}}{Number of images.}
    \item{\code{x.num_videos}}{Number of videos.}
    \item{\code{x.average_token_length}}{Average length of the words in the content.}
    \item{\code{x.num_keywords}}{Number of keywords in the metadata.}
    \item{\code{x.data_channel_is_lifestyle}}{Is data channel 'Lifestyle'?}
    \item{\code{x.data_channel_is_entertainment}}{Is data channel 'Entertainment'?}
    \item{\code{x.data_channel_is_bus}}{Is data channel 'Business'?}
    \item{\code{x.data_channel_is_socmed}}{Is data channel 'Social Media'?}
    \item{\code{x.data_channel_is_tech}}{Is data channel 'Tech'?}
    \item{\code{x.data_channel_is_world}}{Is data channel 'World'?}
    \item{\code{x.kw_min_min}}{Worst keyword (min. shares).}
    \item{\code{x.kw_max_min}}{Worst keyword (max. shares).}
    \item{\code{x.kw_avg_min}}{Worst keyword (avg. shares).}
    \item{\code{x.kw_min_max}}{Best keyword (min. shares).}
    \item{\code{x.kw_max_max}}{Best keyword (max. shares).}
    \item{\code{x.kw_avg_max}}{Best keyword (avg. shares).}
    \item{\code{x.kw_min_avg}}{Avg. keyword (min. shares).}
    \item{\code{x.kw_max_avg}}{Avg. keyword (max. shares).}
    \item{\code{x.kw_avg_avg}}{Avg. keyword (avg. shares).}
    \item{\code{x.self_reference_min_shares}}{Min. shares of referenced articles in Mashable.}
    \item{\code{x.self_reference_max_shares}}{Max. shares of referenced articles in Mashable.}
    \item{\code{x.self_reference_avg_sharess}}{Avg. shares of referenced articles in Mashable.}
    \item{\code{x.weekday_is_monday}}{Was the article published on a Monday?}
    \item{\code{x.weekday_is_tuesday}}{Was the article published on a Tuesday?}
    \item{\code{x.weekday_is_wednesday}}{Was the article published on a Wednesday?}
    \item{\code{x.weekday_is_thursday}}{Was the article published on a Thursday?}
    \item{\code{x.weekday_is_friday}}{Was the article published on a Friday?}
    \item{\code{x.weekday_is_saturday}}{Was the article published on a Saturday?}
    \item{\code{x.weekday_is_sunday}}{Was the article published on a Sunday?}
    \item{\code{x.is_weekend}}{Was the article published on the weekend?}
    \item{\code{x.LDA_00}}{Closeness to LDA topic 0.}
    \item{\code{x.LDA_01}}{Closeness to LDA topic 1.}
    \item{\code{x.LDA_02}}{Closeness to LDA topic 2.}
    \item{\code{x.LDA_03}}{Closeness to LDA topic 3.}
    \item{\code{x.LDA_04}}{Closeness to LDA topic 4.}
    \item{\code{x.global_subjectivity}}{Text subjectivity.}
    \item{\code{x.global_sentiment_polarity}}{Text sentiment polarity.}
    \item{\code{x.global_rate_positive_words}}{Rate of positive words in the content.}
    \item{\code{x.global_rate_negative_words}}{Rate of negative words in the content.}
    \item{\code{x.rate_positive_words}}{Rate of positive words among non-neutral tokens.}
    \item{\code{x.rate_negative_words}}{Rate of negative words among non-neutral tokens.}
    \item{\code{x.avg_positive_polarity}}{Avg. polarity of positive words.}
    \item{\code{x.min_positive_polarity}}{Min. polarity of positive words.}
    \item{\code{x.max_positive_polarity}}{Max. polarity of positive words.}
    \item{\code{x.avg_negative_polarity}}{Avg. polarity of negative words.}
    \item{\code{x.min_negative_polarity}}{Min. polarity of negative words.}
    \item{\code{x.max_negative_polarity}}{Max. polarity of negative words.}
    \item{\code{x.title_subjectivity}}{Title subjectivity.}
    \item{\code{x.title_sentiment_polarity}}{Title polarity.}
    \item{\code{x.abs_title_subjectivity}}{Absolute subjectivity level.}
    \item{\code{x.abs_title_sentiment_polarity}}{Absolute polarity level.}
    \item{\code{y}}{Response variable, log of number of shares (base 10).}
  }
}
\references{
Fernandes, K., P. Vinagre, and P. Cortez (2015) 
"A proactive intelligent decision supportsystem for predicting the popularity of online news." 
Portuguese Conference on Artificial Intelligence, pp. 535-546. Springer.
}
\keyword{datasets}
