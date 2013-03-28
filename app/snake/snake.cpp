/***********************************************
 * File: snake.cpp
 * Author: Keith Schwarz (htiek@cs.stanford.edu)
 *
 * An implementation of the game "Snake" with a
 * rudimentary computer AI player.  The computer
 * controls a snake that tries to eat as much food
 * as possible.  Every time it eats a food pellet,
 * its length increases by one.  The snake loses if
 * it hits a wall or crashes into itself.
 *
 * The AI in this program is extremely simple.  The
 * snake moves forward, turning with probability 1/5
 * at each step.  If the snake's next step will
 * certainly hit a wall, it tries to turn.
 *
 * In order to run this program, you will need to use
 * a level file.  A set of sample files are included
 * in this directory.
 */

#include <iostream>
#include <string>
#include <deque>
#include <vector>
#include <fstream>
#include <cstdlib>

#include <ctime>
using namespace std;

/* Probability of turning at each step. */
const double kTurnRate = 0.2;

/* Time to wait, in seconds, between frames. */
const double kWaitTime = 0.1;

/* Number of food pellets that must be eaten to win. */
const size_t kMaxFood = 20;

/* Constants for the different tile types.  Each level file is
 * encoded using these characters.
 */
const char kEmptyTile = ' '; // Empty space
const char kWallTile  = '#'; // Wall
const char kFoodTile  = '$'; // Food
const char kSnakeTile = '*'; // Snake start location/current position

/* The string used to clear the display before printing the game board.
 * Windows systems should use CLS, Mac OS X or Linux users should use
 * clear.
 */
#ifdef _MSC_VER

  const string kClearCommand = "CLS";

#else

  const string kClearCommand = "clear";

#endif

/* A struct encoding a point in a two-dimensional grid. */
struct pointT {
  size_t row, col;

  /* Utility constructor. */
  pointT(size_t row, size_t col) {
    this->row = row;
    this->col = col;
  }

  /* Default constructor sets everything to 0. */
  pointT() {
    row = col = 0;
  }
};

/* A struct containing relevant game information. */
struct gameT {
  vector<string> world; // The playing field.  Each string is
                        // composed of the characters encoding
                        // tiles and represents one row of the
                        // playing field.
  
  size_t numRows, numCols; // Size of the playing field
  
  deque<pointT> snake;  // The snake body.  The head is in the front
                        // of the deque and the tail in the back, so
                        // we can easily advance the snake a step
                        // forward
  
  int dx, dy;           // The snake direction
  
  size_t numEaten;      // How much food we've eaten.
};

/* Reads a line of text from the user. */
string GetLine() {
  string result;
  getline(cin, result);
  return result;
}

/* Returns true with probability probability.  This works by scaling
 * down rand() by RAND_MAX so that we have a value in [0, 1) and returning
 * whether the value is less than the set probability.
 */
bool RandomChance(double probability) {
  return static_cast<double>(rand()) / RAND_MAX < probability;
}

/* Places a piece of food randomly on the board.  This assumes that there
 * is some free space remaining.
 */
void PlaceFood(gameT& game) {
  while(true) {
    size_t row = rand() % game.numRows;
    size_t col = rand() % game.numCols;
    
    /* If there isn't anything at the specified position, place the food there. */
    if(game.world[row][col] == kEmptyTile) {
      game.world[row][col] = kFoodTile;
      return;
    }
  }
}

/* Clears the display and prints the game board. */
void PrintWorld(gameT& game) {
  /* Use a system call to clear the display. */
  system(kClearCommand.c_str());
  
  /* Print each row. */
  for(size_t i = 0; i < game.world.size(); ++i)
    cout << game.world[i] << endl;
  
  cout << "Food eaten: " << game.numEaten << endl;
}

/* Given an ifstream to a file containing CORRECTLY-FORMATTED world data,
 * loads in the world.
 *
 * The format used is as follows:
 * Line 1: numRows numCols
 * Line 2: dx dy
 * Rest:   World data
 *
 * We assume that the world is correctly-sized and that there is a single
 * '*' character in the world that's the starting point for the snake.
 */
void LoadWorld(gameT& game, ifstream& input) {
  /* Read in the number of rows and columns. */
  input >> game.numRows >> game.numCols;
  game.world.resize(game.numRows);

  /* Read in the starting location. */
  input >> game.dx >> game.dy;

  /* Because we're going to be using getline() to read in the world
   * data, we need to make sure that we consume the newline character
   * at the end of the line containing the input data.  We'll use
   * getline() to handle this.  This scans characters until it finds
   * a newline, then consumes it.
   */
  string dummy;
  getline(input, dummy);

  /* Read in the rows. */
  for(size_t row = 0; row < game.numRows; ++row) {
    getline(input, game.world[row]);
    
    /* Check to see if the * character (snake start position)
     * is in this line.  If so, make the snake.
     */
    size_t col = game.world[row].find('*');
    if(col != string::npos)
      game.snake.push_back(pointT(row, col));
  }
  
  /* Set numEaten to zero - this needs to get done somewhere! */
  game.numEaten = 0;
}

/* Helper function which returns whether a point is contained in the game
 * grid.
 */
bool InWorld(pointT& pt, gameT& game) {
  return pt.col < game.numCols &&
         pt.row < game.numRows;
}

/* Returns whether, if the snake head is at position head, the snake
 * has crashed.
 */
bool Crashed(pointT head, gameT& game) {
  /* We crashed if the head is out of bounds, on a wall, or on another
   * snake piece.
   */
  return !InWorld(head, game) ||
         game.world[head.row][head.col] == kSnakeTile ||
         game.world[head.row][head.col] == kWallTile;
}

/* Returns the next position occupied by the head if the snake is moving
 * in the direction dx, dy.
 */
pointT GetNextPosition(gameT& game, int dx, int dy) {
  /* Get the head. */
  pointT nextSpot = game.snake.front();
  
  /* Update it. */
  nextSpot.col += dx;
  nextSpot.row += dy;
  
  return nextSpot;
}

/* Performs AI logic to control the snake.  The behavior is as follows:
 * 1. If we aren't going to crash and we don't roll a 1 on a fair die,
 *    keep going straight.
 * 2. Otherwise, check to see if we can turn left or right.  Then randomly
 *    pick one safe choice.
 */
void PerformAI(gameT& game) {
  /* Look where we're going to be next step. */
  pointT nextSpot = GetNextPosition(game, game.dx, game.dy);
  
  /* If this crashes us or we just feel like turning, turn. */
  if(Crashed(nextSpot, game) || RandomChance(kTurnRate)) {
    /* Compute what direction we'd be facing if we turned left or
     * right.  From linear algebra we have the following:
     *
     * For a left turn:
     * |x'|   |0 -1||x| --> x' = -y
     * |y'| = |1  0||y| --> y' =  x
     *
     * For a right turn:
     * |x'|   |0  1||x| --> x' =  y
     * |y'| = |-1 0||y| --> y' = -x
     */
    int leftDx = -game.dy;
    int leftDy =  game.dx;
    
    int rightDx =  game.dy;
    int rightDy = -game.dx;
    
    /* Check if turning left or right will cause us to crash. */
    bool canLeft = !Crashed(GetNextPosition(game, leftDx, leftDy), game);
    bool canRight = !Crashed(GetNextPosition(game, rightDx, rightDy), game);
    
    /* Now determine which direction to turn based on what direction
     * we're facing.  If we can choose either direction, pick one
     * randomly.  If we can't turn, don't.
     */
    bool willTurnLeft;
    if(!canLeft && !canRight)
      return;
    else if(canLeft && !canRight)
      willTurnLeft = true;
    else if(!canLeft && canRight)
      willTurnLeft = false;
    else
      willTurnLeft = RandomChance(0.5);
    
    /* Otherwise, based on the direction, turn appropriately. */
    game.dx = willTurnLeft? leftDx : rightDx;
    game.dy = willTurnLeft? leftDy : rightDy;
  }
}

/* Moves the snake one step in its current direction and handles collisions
 * and eating food.  Returns true if we didn't crash, false if we did.
 */
bool MoveSnake(gameT& game) {
  /* Compute new head. */
  pointT nextHead = GetNextPosition(game, game.dx, game.dy);
  
  /* Check for dead. */
  if(Crashed(nextHead, game))
    return false;

  /* Remember whether we just ate food. */
  bool isFood = (game.world[nextHead.row][nextHead.col] == kFoodTile);

  /* Update the display. */
  game.world[nextHead.row][nextHead.col] = kSnakeTile;
  
  /* Push new head to the front of the deque. */
  game.snake.push_front(nextHead);
  
  /* If we got food, pick a new spot and don't remove the tail.  This
   * causes us to extend by one spot. */
  if(isFood) {
    PlaceFood(game);
    ++game.numEaten;
  }
  else {
    /* Clear the tail and remove it from the snake. */
    game.world[game.snake.back().row][game.snake.back().col] = kEmptyTile;
    game.snake.pop_back();
  }
  
  return true;
}

/* Pauses for a few milliseconds so we can see what's happening.  This is
 * implemented using a busy loop, which is less-than-optimal but doesn't
 * require platform-specific features.
 */
void Pause() {
  clock_t start = clock();
  
  while(static_cast<double>(clock() - start) / CLOCKS_PER_SEC < kWaitTime);
}

/* Prompts the user for a filename, then loads the specified file. */
void InitializeGame(gameT& game) {
  /* Seed the randomizer. */
  srand(static_cast<int>(time(NULL)));
  
  ifstream input;
  while(true) {
    cout << "Enter level file: ";
    input.open(GetLine().c_str());
    
    if(!input.fail()) break;
    
    cout << "Sorry, I can't open that file." << endl;
    input.clear();
  }
  
  LoadWorld(game, input);
}

/* Displays the result of the game. */
void DisplayResult(gameT& game) {
  PrintWorld(game);
  if(game.numEaten == kMaxFood)
    cout << "Yay!  The snake won!" << endl;
  else
    cout << "Oh no!  The snake crashed!" << endl;
}

/* Runs the simulation and displays the result. */
void RunSimulation(gameT& game) {
  /* Keep looping while we haven't eaten too much. */
  while(game.numEaten < kMaxFood) {
    PrintWorld(game);
    PerformAI(game);
    
    /* Move the snake and abort if we crashed. */
    if(!MoveSnake(game))
      break;
    Pause();
  }
  DisplayResult(game);
}

/* The main program.  Initializes the world, then runs the simulation. */
int main() {
  gameT game;
  InitializeGame(game);
  RunSimulation(game);
  return 0;
}
