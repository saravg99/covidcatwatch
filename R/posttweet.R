#' Publicar un tweet
#'
#' Publica un tweet
#' @param tweet text que publicar
#' @param keysfile Fitxer JSON que contingui les variables: 
#' consumerKey, consumerSecret, accessToken, accessTokenSecret. 
#' El nom del fitxer per defecte es "accesskeys.json"
#' @param image fitxer de la imatge que es vol incloure
#' @examples
#' post_tweet("Hello")
#' post_tweet("Hello", image = "image.png")
#' post_tweet("Hello", keysfile = "keys.json")
#' @importFrom jsonlite fromJSON
#' @import twitteR
#' @export

post_tweet <- function(tweet,  keysfile = "accesskeys.json", image = NULL) {

	##App values

keys <- jsonlite::fromJSON(keysfile)


#Connect to twitter
twitteR::setup_twitter_oauth(keys$consumerKey,keys$consumerSecret,keys$accessToken,keys$accessTokenSecret)

#Post Tweet
twitteR::updateStatus(tweet, mediaPath=image, bypassCharLimit=TRUE)

}