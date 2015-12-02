import os
import pgn

def cleanup(file):
    print(file)
    pgn_text_file = open(os.getcwd() + '\\Games\\' + file, encoding="Latin-1")
    pgn_text = pgn_text_file.read()
    #print(pgn_text[0:100])
    games = pgn.loads(pgn_text)
    return games


#file = "rvkopen15r1.pgn"
file = "rvkopen14r5.pgn"
games = cleanup(file)
for game in games:
    print(game.moves)