import os
import pgn

def cleanup(file):
    pgn_text = open(os. () + '\\Games\\' + file).read()
    games = pgn.loads(pgn_text)
    return games


#file = "test.pgn"
#games = cleanup(file)
#for game in games:
#    print(game.moves)