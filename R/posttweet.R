#' Publicar un tweet
#'
#' Publica un tweet
#' @param tweet text que publicar
#' @param image fitxer de la imatge que es vol incloure
#' @examples
#' \dontrun{
#' post_tweet("Hello")
#' post_tweet("Hello", image = "image.png")
#' }
#' @import twitteR
#' @export

post_tweet <- function(tweet, image = NULL) {


  #Connect to twitter

  twitteR::setup_twitter_oauth(Sys.getenv("consumerKey"), Sys.getenv("consumerSecret"), Sys.getenv("accessToken"), Sys.getenv("accessTokenSecret"))

  #Post Tweet
  twitteR::updateStatus(tweet, mediaPath=image, bypassCharLimit=TRUE)

}
