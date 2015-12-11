import pgnParser
import convert

id_to_piece = {1:'bR', 2:'bN', 3:'bB', 4:'bQ', 5:'bK', 6:'bB', 7:'bN', 8:'bR', 9:'bP', 10:'bP', 11:'bP', 12:'bP', 13:'bP', 14:'bP', 15:'bP', 16:'bP', 17:'wP', 18:'wP', 19:'wP', 20:'wP', 21:'wP', 22:'wP', 23:'wP', 24:'wP', 25:'wR', 26:'wN', 27:'wB', 28:'wQ', 29:'wK', 30:'wB', 31:'wN', 32:'wR'}
piece_to_id = {'bR':0, 'bN':1, 'bB':2, 'bQ':3, 'bK':4, 'bP':5, 'wR':6, 'wN':7, 'wB':8, 'wQ':9, 'wK':10, 'wP':11}
all_pieces = ['R', 'N', 'B', 'Q', 'K']

piece_history = []
piece_id = 8
chess_length = [[(0,0) for r in range(10)] for s in range(7)]
elo_difference = [(0,0) for r in range(10)]
elo_results = []
elo_file = open("Elo\\elo_results.csv","w")
elo_file.write("elo1, elo2, res\n")
total_games = 0
games_with_elos = 0

result_by_difference = [(0,0,0) for r in range(40)]
length_by_difference = [(0,0) for r in range(40)]
checkmate = (0,0)

total_captured =[[0 for x in range(8)] for x in range(8)]
total_visited =[[0 for x in range(8)] for x in range(8)]

piece_capture = [[0 for x in range(6)] for x in range(6)]
piece_lifetime = [(0,0) for x in range(6)]
relative_lifetime = [(0,0) for x in range(6)]

for y in range(9,16):
    thisRange = range(1,11)
    if y < 13:
        thisRange = range(1,10)
    for x in thisRange:
        elo_round_file = open("Elo\\round" + str(x) + ".csv", "a")
        games = pgnParser.cleanup('rvkopen' + str(y) + 'r' + str(x) + '.pgn')
        b = 0
        for game in games:
            b += 1
            #print(b)
            #print(game)
            moves = game.moves
            length = (1 + len(moves)) // 2

            total_games += 1
            thing = False
            if hasattr(game, 'whiteelo'):
                w_elo = int(getattr(game,'whiteelo'))
                #print(w_elo, " ",end="")
                thing = True
            if hasattr(game, 'blackelo'):
                b_elo = int(getattr(game,'blackelo'))
                #print(b_elo,end="")
                if thing:
                    if w_elo < 1 or b_elo < 1:
                        break
                    games_with_elos += 1
                    t = elo_difference[x - 1]
                    newAverage = (t[0] * t[1] + abs(b_elo - w_elo)) / (t[1] + 1)
                    elo_difference[x - 1] = (newAverage, t[1] + 1)

                    result = getattr(game,'result')
                    if result is not "*":
                        text = str(str(w_elo) + "," + str(b_elo) + "," + result + "\n")
                        elo_file.write(text)
                        elo_round_file.write(text)
                        if (w_elo >= b_elo):
                            group = (w_elo - b_elo) // 50
                            old = result_by_difference[group]
                            if result == '1-0':
                                result_by_difference[group] = (old[0] + 1, old[1], old[2])
                            elif result == '0-1':
                                result_by_difference[group] = (old[0], old[1], old[2] + 1)
                            else:
                                result_by_difference[group] = (old[0], old[1] + 1, old[2])

                            t = length_by_difference[group]
                            newAverage = (t[0] * t[1] + length) / (t[1]+ 1)
                            length_by_difference[group] = (newAverage, t[1] + 1)
                        else:
                            group = (b_elo - w_elo) // 50
                            old = result_by_difference[group]
                            #print(result)
                            if result == '1-0':
                                result_by_difference[group] = (old[0], old[1], old[2] + 1)
                            elif result == '0-1':
                                result_by_difference[group] = (old[0] + 1, old[1], old[2])
                            else:
                                result_by_difference[group] = (old[0], old[1] + 1, old[2])
                            t = length_by_difference[group]
                            newAverage = (t[0] * t[1] + length) / (t[1]+ 1)
                            length_by_difference[group] = (newAverage, t[1] + 1)



            # length of chess
            t = chess_length[y - 9][x - 1]
            newAverage = (t[0] * t[1] + length) / (t[1] + 1)
            chess_length[y - 9][x - 1] = (newAverage, t[1] + 1)

            # setting up board
            board = [[0 for x in range(8)] for x in range(8)]
            for i in range(8):
                board[0][i] = i + 1
                board[1][i] = 8 + i + 1
                board[6][i] = 16 + i + 1
                board[7][i] = 24 +  i + 1

            # keeping track of data
            occupy = [[0 for x in range(8)] for x in range(8)]
            visited = [[0 for x in range(8)] for x in range(8)]
            captured =[[0 for x in range(8)] for x in range(8)]
            history = [[[] for x in range(length)] for x in range(12)]

            turn = False
            skip = False

            # chess
            for k in range(len(moves) - 1):
                m = moves[k]
                #print("move",k//2,m)

                if '{' in m or '(' in m:
                    skip = True
                    continue
                if '}' in m or ')' in m:
                    skip = False
                    continue
                if skip == True:
                    continue

                # capture statistics
                if 'x' in m:
                    #print("capture", m)
                    piece = m[0]
                    spot = m[(m.find('x')+1):(m.find('x')+3)]
                    if piece in all_pieces:
                        capturing_piece = piece
                    else:
                        capturing_piece = "P"
                    row = 8 - int(spot[1])
                    col = ord(spot[0]) - ord('a')
                    #print(capturing_piece,"captures",end=" ")
                    if board[row][col] < 1:
                        captured_piece = "P"
                    else:
                        captured_piece = id_to_piece[board[row][col]][1]

                        round = k // 2 + 1
                        t = piece_lifetime[piece_to_id["b" + captured_piece]]
                        newAverage = (t[0] * t[1] + round) / (t[1] + 1)
                        piece_lifetime[piece_to_id["b" + captured_piece]] = (newAverage, t[1] + 1)

                        t = relative_lifetime[piece_to_id["b" + captured_piece]]
                        newAverage = (t[0] * t[1] + round / length) / (t[1] + 1)
                        relative_lifetime[piece_to_id["b" + captured_piece]] = (newAverage, t[1] + 1)

                    piece_capture[piece_to_id["b" + capturing_piece]][piece_to_id["b" + captured_piece]] += 1

                for i in range(8):
                    for j in range(8):
                        if board[i][j] > 0:
                            occupy[i][j] = occupy[i][j] + 1
                            piece = id_to_piece[board[i][j]]
                            if ((turn is True and piece[0] is 'b') or (turn is False and piece[0] is 'w')):
                                history[piece_to_id[piece]][k//2].append((i,j))

                r = convert.convert(m, board, turn)
                space = (-1,-1)

                if (len(r) == 2):
                    #print(m, r)
                    piece = board[r[0][0]][r[0][1]]
                    board[r[1][0]][r[1][1]] = piece
                    board[r[0][0]][r[0][1]] = 0

                    visited[r[1][0]][r[1][1]] = visited[r[1][0]][r[1][1]] + 1
                    if "x" in m:
                        captured[r[1][0]][r[1][1]] = captured[r[1][0]][r[1][1]] + 1
                elif (len(r) == 3 and r[0] == "C"):
                    r_1 = r[1]
                    r_2 = r[2]
                    piece = board[r_1[0][0]][r_1[0][1]]
                    board[r_1[1][0]][r_1[1][1]] = piece
                    board[r_1[0][0]][r_1[0][1]] = 0
                    piece = board[r_2[0][0]][r_2[0][1]]
                    board[r_2[1][0]][r_2[1][1]] = piece
                    board[r_2[0][0]][r_2[0][1]] = 0

                    visited[r_1[1][0]][r_1[1][1]] = visited[r_1[1][0]][r_1[1][1]] + 1
                    visited [r_2[1][0]][r_2[1][1]] = visited[r_2[1][0]][r_2[1][1]] + 1

                elif (len(r) == 3 and r[0] == "prom"):
                    #print(m,r)
                    board[r[1][0][0]][r[1][0][1]] = 0
                    piece = 0
                    if turn == 0:
                        if r[2] == "Q":
                            piece = 28
                        elif r[2] == "R":
                            piece = 25
                        elif r[2] == "N":
                            piece = 26
                        else:
                            piece = 27
                    else:
                        if r[2] == "R":
                            piece = 1
                        elif r[2] == "N":
                            piece = 2
                        elif r[2] == "B":
                            piece = 3
                        elif r[2] == "Q":
                            piece = 4
                    board[r[1][1][0]][r[1][1][1]] = piece

                    visited[r[1][1][0]][r[1][1][1]] = visited[r[1][1][0]][r[1][1][1]] + 1
                    if "x" in m:
                        captured[r[1][1][0]][r[1][1][1]] = captured[r[1][1][0]][r[1][1][1]] + 1
                else:
                    print("ERROR:" + m + " returns " + r + " WHICH IS NOT A VALID MOVE !!!" + str(x))
                    exit()

                turn = not turn
                sum = 0
                for i in range(8):
                    for j in range(8):
                        sum += visited[i][j]


            piece_history.append(history[piece_id])
            for n in range(8):
                for m in range(8):
                    total_captured[n][m] += captured[n][m]
                    total_visited[n][m] += visited[n][m]
        elo_round_file.close()


print(piece_history)
total_length = 0
for l in piece_history:
    if len(l) > total_length:
        total_length = len(l)
print(total_length)

data = [[[0, 0, 0, 0, 0, 0, 0, 0] for x in range(8)] for x in range(total_length)]

for s in piece_history:
    for i in range(len(s)):
        # working with data[i]
        for t in s[i]:
            # print(t)
            data[i][t[0]][t[1]] += 1
sum = 0
for s in data[0]:
    for r in s:
        sum += r
print(sum)
for i in range(65):
    #print("Step", i)

    file = open("Results\\distribution_p" + str(piece_id) + "_r" + str(i) + ".csv", "w")
    file.write("X,a,b,c,d,e,f,g,h\n")
    for s in range(len(data[i])):
        r = data[i][s]
        file.write(str(8 - s) + ",")
        first = True
        for s in r:
            if first:
                file.write(str(s))
                first = False
            else:
                file.write("," + str(s))
            print(s,'',end='')
        file.write("\n")
        print("")
    print("")
    file.close()

print("Chess lengths")
for c in chess_length:
    print(c)

print("games with elos:", games_with_elos,"out of", total_games)
for e in elo_difference:
    print(e)


elo_file.close()
print("results by ELO difference")
for r in result_by_difference:
    print(r[0], r[1], r[2])

print("Most captured spots")
file = open("captured_spots.csv","w")
file.write("a,b,c,d,e,f,g,h\n")
for r in total_captured:
    print(r)
    file.write(str(r).strip('[]'))
    file.write("\n")
file.close()
print("Most visited spots")
for r in total_visited:
    print(r)

print("Captures")
file = open("captures.csv","w")
file.write("R,N,B,Q,K,P\n")
for r in piece_capture:
    print(r)
    for s in range(len(r)):
        file.write(str(r[s]))
        if s < 5:
            file.write(", ")
    file.write("\n")
file.close()
print("Lifetime")
for r in piece_lifetime:
    print(r)
print("Relative lifetime")
for r in relative_lifetime:
    print(r)
print("Length by difference")
for r in length_by_difference:
    print(r)
print("Total games:", total_games)
