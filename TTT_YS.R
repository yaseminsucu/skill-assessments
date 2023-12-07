#Tic-Tac-Toe
#3x3 grid board
#make user to pick "X" or "O" by asking which column or row to insert the X or O
#always stars with X 
#the players will take turns 
#the player who places three of X or O in horizontal, vertical or diagonal row is a winner


# Printing the Tic-Tac-Toe board
TicTacToe_board <- function(board) {
  cat("\n")
  for (row in 1:3) {
    for (col in 1:3) {
      # Use different symbols for "X" and "O"
      symbol <- switch(board[row, col], "X" = "❌", "O" = "⭕", " " = " ")
      cat(symbol, sep = "")
      if (col < 3) cat(" | ")  
    }
    cat("\n")
    if (row < 3) cat("----------\n")  
  }
  cat("\n")
}

# Checking if the current player won
checking_winner <- function(board, symbol) {
  # Checking rows and columns
  for (i in 1:3) {
    if (all(board[i,] == symbol) || all(board[,i] == symbol)) {
      return(TRUE)
    }
  }
  # Check diagonaly
  if (all(diag(board) == symbol) || all(diag(board[,3:1]) == symbol)) {
    return(TRUE)
  }
  return(FALSE)
}

# Checking if the board is full, if it's a tie
checking_tie <- function(board) {
  return(all(board != " "))
}

# Getting the user to move 
user_move <- function() {
  repeat {
    cat("Enter your move (row and column, e.g., 1 2): ")
    move <- scan(n = 2, quiet = TRUE)
    if (length(move) == 2 && all(move %in% 1:3)) {
      return(move)
    } else {
      cat("Invalid move. Please try again.\n")
    }
  }
}

# Getting the computer move
computer_move <- function(board) {
  empty_cells <- which(board == " ", arr.ind = TRUE)
  return(empty_cells[sample(1:nrow(empty_cells), 1), ])
}

# Function to play Tic-Tac-Toe
play_tic_tac_toe <- function() {
  # Initialize the board
  board <- matrix(" ", nrow = 3, ncol = 3)
  
  # Choose X or O
  cat("Choose X or O: ")
  symbol <- readLines(con = stdin(), n = 1)
  if (!(symbol %in% c("X", "O"))) {
    cat("Not acceptible! Defaulting to X.\n")
    symbol <- "X"
  }
  
  # Main loop for the game
  while (TRUE) {
    TicTacToe_board(board)
    
    if (symbol == "X") {
      # User's move
      move <- user_move()
      if (board[move[1], move[2]] == " ") {
        board[move[1], move[2]] <- "X"
      } else {
        cat("Try somewhere else, the cell is full.\n")
        next
      }
    } else {
      # Computer's move
      move <- computer_move(board)
      board[move[1], move[2]] <- "O"
    }
    
    # Check for a winner
    if (checking_winner(board, symbol)) {
      TicTacToe_board(board)
      cat(symbol, "Woohoo!! You win!\n")
      break
    }
    
    # Check for a tie
    if (checking_tie(board)) {
      print_board(board)
      cat("Ahh, it's a tie!\n")
      break
    }
    
    # Switch player
    symbol <- ifelse(symbol == "X", "O", "X")
  }
}

# Run the game
play_tic_tac_toe()
