# Jim and Joe start a game with 5 tokens, 3 for Jim and 2 for Joe.
# A biased coin is tossed and if the outcome is heads,
# Jim gives Joe a token, else Jim gets a token from Joe.
# The probability that the coin comes out head is 0.30.
# The game ends when Jim or Joe has all the tokens.
# At this point, there is 40% chance that Jim and Joe
# will continue to play the game, again starting with
# 3 tokens for Jim and 2 for Joe. (Winston, 2004)
# 
# Question 1: Represent the game as Markov Chain.
# Question 2: Determine the probability that Joe will win in 10 coin tosses.
# Question 3: Determine the fraction of the time that Jim ends up with 0 tokens.
# Question 4: Determine the average number of coin tosses needed before Jim wins.

#For this project we will need the following libraries:
library(matrixcalc)
library(markovchain)

# X(t): The number of Tokens Jim has. (You can define the decision variable according to Joe too.)*
# States: (0, 1, 2, 3, 4, 5) (Jim cannot have nonpositive or more than 5 tokens since we are given a total of 5 tokens in the problem.)* 


#Question 1:

#We know that our matrix will be a square matrix of size 6x6. Hence let's create a matrix full of zeros.

#Creating a 6x6 full of zeros
MarkovChain = matrix(0, 6, 6)
print(MarkovChain)

#Adding state names to the rows and columns.
states = seq(0, 5)
colnames(MarkovChain) = states
rownames(MarkovChain) = states
print(MarkovChain)

#If game ends, the game restarts with the same tokens with a probability of 0.4.
#If Jim loses the game.
MarkovChain[1,1] = 1 - 0.4
MarkovChain[1,4] = 0.4

#If Jim wins the game.
MarkovChain[6,6] = 1 - 0.4
MarkovChain[6,4] = 0.4

#If he gets a heads, he gives Joe a token. If tails, he gets a token from Joe.
i = 2
k = 3
j = 1

while(i < (length(MarkovChain[1,]))){
  MarkovChain[i, j] = 0.3
  MarkovChain[i, k] = 1 - 0.3
  i = i + 1
  j = j + 1
  k = k + 1
}
print(MarkovChain)


#Question 2:
#Computing the nth power of the matrix.
Toesses = matrix.power(MarkovChain, 10)

#Get the probability of Jim starting with 3 tokens and ending up with 0.
paste0("The probability that Joe wins in 10 coin tosses is ", round(Toesses[4,1], 3))


#Question 3:
#Compute the eigen values and vectors:
e = eigen(t(MarkovChain))

#Get the first column of the vectors:
first = Re(e$vectors[ ,1])

#Convert it to probabilites:
mu = first / sum(first)

#Get only the probability of Pi0.
paste0("The long term probability that Jim ends up with 0 tokens is ", round(mu[1], 3))


#Question 4:
#Markov Chain library requires from us to convert the matrix into its own Markov Chain type.
mc = new("markovchain", states = as.character(states), transitionMatrix = MarkovChain)

print(mc)

#Compute Mean Passage Times:
mfp = meanFirstPassageTime(mc)
print(paste0("On the average ",mfp[4,6], " are needed for Jim to win the game."))

# A generalized version of this problem can be found here:
# https://avsdashboard.shinyapps.io/MarkovChainsProject/

