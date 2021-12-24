# -*- coding: utf-8 -*-
"""
Created on Sun Dec 12 17:04:17 2021

@author: hp
"""

from agent import Agent
from tictactoe import TicTacToe


def play(agent):                # creation d'une fonction play 
    game = TicTacToe()       #  creation du jeu 
    while True:
        action = agent.qlearner.get_best_action(game.get_state())
        winner = game.play(*action)
        if winner:
            print("**** you lost ****")
            return
        if game.is_ended():
            print("**** égalité ****")
            return
        x, y = input("input x and y: ").split()
        winner = game.play(int(x), int(y))
        if winner:
            print("**** you won ****")
            return
        if game.is_ended():
            print("**** égalité ****")
            return


q_agent = Agent()
print("learning...")
q_agent.learn()
print("done")


print("\nlet's play\n")
play(q_agent)
    