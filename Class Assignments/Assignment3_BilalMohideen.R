#Assignment 3 - Hangman
#Bilal Mohideen - BTC1855H
#R Version 2024.04.1+748 (2024.04.1+748)

#ensure that word bank (random_foods.txt)
#is saved in working directory
#use function to set to the appropriate working directory
setwd("/Users/bilalmohideen/Desktop/MBiotech - UofT/BTC1855H/BM_BTC1855H/Class Assignments")
  
#reading word list (food themed in this case)
#contains 53 random food items, created list on my own
food_bank <- readLines("random_foods.txt")

#choosing a random element from the list
#this serves as the hidden word for this round
hidden_word <- sample(food_bank, 1)

#determining the length of the word
#displayed to user throughout the game
word_length <- nchar(hidden_word)

#setting the maximum amount of attempts to 5
#to start, user has 5 "lives" left
max_attempts <- 5

#creating a variable to track the number of attempts
#set to 0 to start, will go up with incorrect guesses
#will not change for invalid entries or correct guesses
attempts <- 0

#set to false before starting game
#defined within the while (max_attempts > attempts) loop
winner <- FALSE

#creating a variable to store all incorrect guesses
#will update and be displayed to user throughout the game
incorrect <- character()

#creating a variable to store all correct guesses
#will update and be displayed to user throughout the game
correct <- character()

#creating a variable for the user input (guess)
#will prompt user to input guess
guess <- character()

#creating function for verifying that
#input has no special characters or numbers
#invalid returns TRUE when special characters
#or numbers present
invalid <- function(guess) {
  grepl("[0-9]", guess) || grepl("[^a-zA-Z0-9]", guess)
}

#creating function for input of only 1 letter
#returns TRUE when input is 1 character
single_letter <- function(guess) {
  nchar(guess) == 1
}

#creating function for whole word input
#returns TRUE when input is more than 1 character
whole_word <- function(guess) {
  nchar(guess) > 1
}
#creating object to store individual letters of hidden_word 
word_letters <- (strsplit(hidden_word, NULL)[[1]])

#adding a welcome message with instructions for the game
#user is informed on the length of the hidden word, and will
#continue to be informed throughout the game
cat("     Welcome to Hungry Hangman!
     Your objective is to correctly guess a predetermined word
     All words will be food-themed in Hungry Hangman
     You will be provided with a series of blank spaces
     Each blank space represents a letter in the word
     You may guess one letter at a time, or attempt to guess the entire word
     You win if you can guess the word before the Hangman figure is completed
     A failed attempt to guess the entire word is game over
     Users are allowed 5 incorrect guesses (lives) \n
     The word is", word_length,"letters long")

#while loop that will run as long as
#there are more max attempts remaining than
#user attempts
#once max_attempts = attempts, game over
while (max_attempts > attempts) {
  
  #creating object to indicate if the word has been
  #guessed through individual letters
  #setequal checks if sets contain same elements
  #order/duplicates are ignored
  #correct contains all letter guesses
  #word_letters: hidden word split up into letters
  winner <- setequal(correct, word_letters)
  
  #if correct and word_letters contain same elements
  #(ignoring duplicates and order)
  #game over and user has guessed correctly using letter guessing
  if (winner == TRUE) {
    cat("\nGreat job! You have guessed the whole word. Game over :) The word was", hidden_word)
    break
  }
    #user input for guessing either a letter or the entire word
    #converts to lowercase to match with words in food_bank
    guess <- tolower(readline(prompt = "Please enter a letter or word as your guess: "))
    
    #if the guess contains special characters
    #no need to add an attempt (user is not penalized for invalid entry)
    #checks if invalid function is TRUE
    if (invalid(guess) == TRUE) {
      cat("Invalid entry.\nYou have used", attempts,"out of 5 lives.
            \nCorrect guesses:", correct,"\nIncorrect guesses:", incorrect,
          "\nThe word is", word_length,"letters long.")
    }
    #if the user attempts to guess a single letter
    #checks if single_letter function is TRUE
    else if (single_letter(guess) == TRUE) {
      cat("You have entered a letter.")
      #checks for duplicate letter guesses that have previously been made
      #does not add an attempt (user is not penalized)
      if (guess %in% incorrect || guess %in% correct) {
        cat("\nOops! You've already guessed that letter.\nYou have used", attempts,"out of 5 lives.
            \nCorrect guesses:", correct,"\nIncorrect guesses:", incorrect,
            "\nThe word is", word_length,"letters long.")
      }
      #checks if the guessed letter is in the word
      #we have already checked that this has not been guessed before
      #does not add an attempt (user is not penalized)
      #adds to variable that stores user guesses under correct
      else if (guess %in% word_letters) {
        correct <- c(correct, guess)
        cat("\nCongrats! This letter is in the word.\nYou used", attempts,"out of 5 lives.
            \nCorrect guesses:", correct,"\nIncorrect guesses:", incorrect,
            "\nThe word is", word_length,"letters long.")
        #this is for guesses that are not in the word
        #adds an attempt (user is penalized)
        #adds to variable that stores user guesses under incorrect
      } else {
        attempts <- attempts + 1
        incorrect <- c(incorrect, guess)
        cat("\nSorry, that letter is not in the word.\nYou have used", attempts,"out of 5 lives.
            \nCorrect guesses:", correct,"\nIncorrect guesses:", incorrect,
            "\nThe word is", word_length,"letters long.")
      }
    }
    #if the user attempts to guess the entire word
    #break is used to break the loop
    #if user's guess and hidden_word are identical, then the user wins
    #initial input for guess already converted to lowercase
    else if (whole_word(guess) == TRUE) {
      cat("You have entered a word. ")
      if (identical(hidden_word, guess) == TRUE) {
        cat("\nCongrats! You have guessed the entire word. ")
        break
      }
      #if there's an incorrect guess of the word, automatically game over
      else {
        cat("\nSorry, that guess is not the word. \nGame over :( The word was", hidden_word)
        break
    }
    }
}
#used if the number of attempts exceeds the maximum
#outside of the while loop
if (attempts >= max_attempts) {
  cat("\nNo lives remaining. \nGame over :( The word was", hidden_word)
}
#resets all parameters of the game (back to baseline)
reset_game <- tolower(readline(prompt = "Please enter 'r' if you would like to reset: "))

if (reset_game == "r") {
  correct <- character()
  incorrect <- character()
  hidden_word <- sample(food_bank, 1)
  winner <- FALSE
  attempts <- 0
} else {
  print("Thank you for playing!")
}

