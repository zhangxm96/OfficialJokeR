#' Retrieves random official joke from API
#' The Final Project for QMSS MDS 2019 Fall
#' Option B: API project
#' @author Ximing Zhang
#' @return List with all parameters of the joke
#' @examples
#' \dontrun{
#'
#' # Retrieve random joke
#' rand_joke1 <- get_random_joke()
#'
#' }
#' @import httr jsonlite
#' @export
get_random_joke <- function(){
  # -------------------
  # Send a GET request
  resp <- httr::GET("https://official-joke-api.appspot.com/jokes/random")

  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "anapioficeandfire API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # -------------------
  # Parse JSON object
  parsed <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = FALSE)

  return(parsed)
}

#' Retrieves jokes from API
#'
#'
#' @param type The endpoint type, one of general, knock-knock or programming.
#'             Default to general if nothing is provided.
#' @param choice The choice type, one of random or ten.
#'             Default to random if nothing is provided.
#' @param return_type The return_type, one of list or dataframe.
#'            Default to list if nothing is provided.
#' @author  Ximing Zhang
#' @return List with all parameters of that joke
#' @examples
#' \dontrun{
#'
#' # Retrieve random general joke,return list
#' joke1 <- get_joke(type = "general",choice="random",return_type="list")
#'
#' # Retrieve random programming joke,return dataframe
#' joke2 <- get_joke(type = "programming",choice="random",return_type="dataframe")
#'
#' # Retrieve ten knock-knock jokes,return list
#' joke3 <- get_joke(type = "knock-knock",choice="ten",return_type="list")
#'
#' # Retrieve ten general jokes,return dataframe
#' joke4 <- get_joke(type = "general",choice="ten",return_type="dataframe")
#'
#' # Retrieve data with default value
#' joke5 <- get_joke()
#' }
#' @export
get_joke <- function(type = c("general", "knock-knock", "programming"),
                     choice=c("random","ten"),return_type=c("list","dataframe")){
  # Check if type is null,set default value
  type <- match.arg(type)
  # Check if choice is null,set default value
  choice <- match.arg(choice)
  # Check if return_type is null,set default value
  return_type <- match.arg(return_type)

  # modify url from input type and choice
  url <- paste0("https://official-joke-api.appspot.com/jokes/",type,"/",choice)

  # -------------------
  # Send a GET request
  resp <- httr::GET(url)

  # -------------------
  # Turn API errors into R errors
  if (httr::http_error(resp)) {
    stop(
      sprintf(
        "anapioficeandfire API request failed [%s]\n%s\n<%s>",
        httr::status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  resp.text<-httr::content(resp, "text")
  if(return_type=="list"){
    # -------------------
    # Parse JSON object
    parsed <- jsonlite::fromJSON(resp.text, simplifyVector = FALSE)
  }else if(return_type == "dataframe"){
    # -------------------
    # Parse JSON object
    parsed <- jsonlite::fromJSON(resp.text)
  }

  return(parsed)
}

get_joke("general","random","dataframe")
get_joke(type = "general",choice="ten",return_type="dataframe")
get_random_joke()
get_joke()
