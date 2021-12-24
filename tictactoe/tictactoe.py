
# creation du logique du jeu 

class TicTacToe:                      # render =True pour imprimer le plateau du jeu sur le terminal à chaque coup 
    def __init__(self, render=True):  # affecter des valeurs a nos objets 
                                      # render est defini comme argument car Nous ne voulons donc pas qu'il imprime le tableau après chaque mouvement.                                  
    
        self.board = [[0, 0, 0] for _ in range(3)] # initier le board 
        self.player = 1
        self.repr = {0: ".", 1: "x", -1: "o"} # affectation des valeurs aux joueur 
        self.render = render

    def _get_winner(self):  ##  detecter le gagnant 
       
        # verification sur l'horizontale
        for i in range(3):
            if abs(sum(self.board[i])) == 3:
                return self.board[i][0]

        # verification sur le vertical
        for i in range(3):
            if abs(sum(self.board[j][i] for j in range(3))) == 3:
                return self.board[0][i]

        # verification sur le diagonale 
        if abs(sum(self.board[i][i] for i in range(3))) == 3:
            return self.board[0][0]
        if abs(sum(self.board[i][2 - i] for i in range(3))) == 3:
            return self.board[0][2]

        return None

    def get_state(self):    ## pour afficher l'etat du jeu 
        return str(self.board)  

    def get_valid_actions(self):  ## cette fonction renverra toutes les coordonnées des cellules vides
        actions = []              ## cette fonction est utile pour former notre agent IA  
        for i in range(3):
            for j in range(3):
                if self.board[i][j] == 0:
                    actions.append((i, j))
        return actions

    def is_ended(self): ## fonction vérifie s'il reste des cellules vides dans le plateau 
        for i in range(3):   ## et renvoie si le jeu est terminé ou non
            for j in range(3):
                if self.board[i][j] == 0:
                    return False
        return True

    def _print(self):          ## fonction d'affichage 
        for row in self.board:
            for item in row:
                print(self.repr[item], end="\t")
            print("\n")
        print("-----------------\n")

    def play(self, x, y):          #Place le mouvement du joueur actuel à ces coordonnées sur le plateau 3x3 du jeu
        if self.board[x][y] != 0:  # et change le tour de l'autre joueur s'il y a un gagnant elle retournera le gagnant
            return None

        self.board[x][y] = self.player
        if self.render:
            self._print()
        winner = self._get_winner()
        if winner:
            return winner
        self.player *= -1     # est equi a player = player * -1
        return None