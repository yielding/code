#include <deque>
#include <string>

#include <iostream>

using namespace std;


deque<string> deck_of_cards; 
deque<string> current_hand;

void initialize_cards(deque<string>& cards)
{
  cards.push_front("aceofspades");
  cards.push_front("kingofspades");
  cards.push_front("queenofspades");
  cards.push_front("jackofspades");
  cards.push_front("tenofspades");
  //
  // etc.
  //
}

template <class It, class It2> 
void print_current_hand(It start, It2 end) 
{
  while (start < end) 
    cout << *start++ << endl;
}


template <class It, class It2>
void deal_cards(It, It2 end)
{
  for (int i=0;i<5;i++)
  {
    current_hand.insert(current_hand.begin(),*end);
    deck_of_cards.erase(end++);
  }
}

void play_poker()
{
  initialize_cards(deck_of_cards);
  deal_cards(current_hand.begin(),deck_of_cards.begin()); 
}

int main () 
{
  play_poker();
  print_current_hand(current_hand.begin(),current_hand.end());
  return 0;
}
