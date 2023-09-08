#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door.
#'
#' @description
#'   Simulates a contestant selecting a door from three available choices.
#'
#' @details
#'   The `select_door()` function simulates the contestant's initial choice 
#'   in the Monty Hall Problem. It randomly selects one of the three available doors, 
#'   and the outcome represents the door number chosen by the contestant.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The chosen door number (1, 2, or 3).
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a Goat Door
#'
#' @description
#'   Simulates the host opening a door to reveal a goat, based on the contestant's choice.
#'
#' @details
#'   The `open_goat_door()` function emulates the host's behavior 
#'   in the Monty Hall Problem. When the contestant selects a door 
#'   containing a car, the host randomly reveals one of the two 
#'   unchosen doors with goats by opening it. Conversely, if the 
#'   contestant initially chooses a door with a goat, the host 
#'   exposes the other unselected door containing a goat.
#'
#' @param game A character vector representing the game setup.
#' @param a.pick The contestant's chosen door number.
#'
#' @return 
#'   The door number that was opened, revealing a goat.
#'
#' @examples
#'   open_goat_door(game = create_game(), a.pick = 1)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Change the Chosen Door
#'
#' @description
#'   Simulates the contestant either staying with their original choice or switching to another unopened door.
#'
#' @details
#'   The `change_door()` function simulates the decision-making process of the contestant in the Monty Hall Problem. 
#'   When the `stay` parameter is set to `TRUE`, the contestant retains their original choice. 
#'   However, if `stay` is set to `FALSE`, the contestant opts to switch to the remaining unopened door. 
#'   This function provides the final selected door number as its output.
#'
#' @param stay Logical, indicating whether the contestant chooses to stay with their original choice.
#' @param opened.door The door number that was opened to reveal a goat.
#' @param a.pick The contestant's original choice of door number.
#'
#' @return
#'   The final chosen door number (1, 2, or 3).
#'
#' @examples
#'   change_door(stay = TRUE, opened.door = 2, a.pick = 1)
#'   change_door(stay = FALSE, opened.door = 2, a.pick = 1)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine the Game Outcome
#'
#' @description
#'   Determines if the contestant wins or loses the Monty Hall Problem game based on their final choice.
#'
#' @details
#'   The `determine_winner()` function evaluates the game's result in the context of the Monty Hall Problem. 
#'   It verifies whether the contestant's ultimate selection matches the car's position within the game's configuration. 
#'   If the contestant wins, it returns "WIN"; otherwise, it returns "LOSE."
#'
#' @param final.pick The contestant's final chosen door number.
#' @param game A character vector representing the game setup.
#'
#' @return 
#'   "WIN" if the contestant wins, "LOSE" if they lose.
#'
#' @examples
#'   determine_winner(final.pick = 2, game = create_game())
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play a Monty Hall Problem Game
#'
#' @description
#'   Simulates the entire Monty Hall Problem game, including the contestant's choice, the host's action, and the game outcome.
#'
#' @details
#'   The `play_game()` function offers a comprehensive simulation of the Monty Hall Problem game. 
#'   It encompasses the contestant's initial door selection, the host's action of revealing a goat door, 
#'   and the ultimate game outcome, determined by the contestant's decision to either stay or switch.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'   "WIN" if the contestant wins, "LOSE" if they lose.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play Multiple Monty Hall Problem Games
#'
#' @description
#'   Simulates multiple Monty Hall Problem games and records the outcomes.
#'
#' @details
#'   The `play_n_games()` function enables you to simulate numerous Monty Hall Problem game instances. 
#'   It iterates the `play_game()` function 'n' times, records the results (WIN or LOSE) for each game, 
#'   and presents them in a data frame. The 'n' parameter allows you to specify the number of games to simulate.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'   A data frame with the outcomes of each simulated game.
#'
#' @examples
#'   # Simulate 50 Monty Hall Problem games
#'   play_n_games(50)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
