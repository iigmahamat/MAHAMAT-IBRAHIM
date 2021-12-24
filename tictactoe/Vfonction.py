
"""
Created on Thu Dec  2 14:33:25 2021

@author: hp
"""
               ## CREATION DE LA TABLE-Q : dans cette partie nous allons utilisons l'algo Q-learning 
               
              
from collections import defaultdict

class Q :
    def __init__(self, alpha=0.5, discount=0.5):
        self.alpha = alpha
        self.discount = discount
        self.values = defaultdict(lambda: defaultdict(lambda: 0.0))

    def update(self, state, action, next_state, reward):  #la mise à jour de la valeur de Q en utilisant l'équation de Bellman
        value = self.values[state][action]
        v = list(self.values[next_state].values())
        next_q = max(v) if v else 0
        value = value + self.alpha * (reward + self.discount * next_q - value)
        self.values[state][action] = value

    def get_best_action(self, state):                # elle choisi la meilleur action              
        keys = list(self.values[state].keys())
        if not keys:
            return None
        return max(keys, key=lambda x: self.values[state][x])