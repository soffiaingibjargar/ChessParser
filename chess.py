import pgnParser
import convert

piece_history = []
piece_id = 5
for y in range(14,16):
    if y == 15:
        thisRange = range(1,11)
    else:
        thisRange = range(1,11)
    for x in thisRange:
        games = pgnParser.cleanup('rvkopen' + str(y) + 'r' + str(x) + '.pgn')
        b = 0
        for game in games:
            #print(b)
            b += 1
            moves = game.moves
            length = (1 + len(moves)) // 2

        #for i in range(len(moves) // 3):
        #    moves.pop(2*i)
        #print(moves)

        # rows 0 and 1 are black pieces
        # rows 6 and 7 are white pieces
        board = [[0 for x in range(8)] for x in range(8)]
        for i in range(8):
            board[0][i] = i + 1
            board[1][i] = 8 + i + 1
            board[6][i] = 16 + i + 1
            board[7][i] = 24 +  i + 1
        for i in range(8):
            print(board[i])

        # keeping track of data
        occupy = [[0 for x in range(8)] for x in range(8)]
        visited = [[0 for x in range(8)] for x in range(8)]
        captured =[[0 for x in range(8)] for x in range(8)]
        history = [[[] for x in range(length)] for x in range(12)]

        turn = False
        skip = False
        #for m in moves[0:-1]:
        for k in range(len(moves) - 1):
            m = moves[k]

            if '{' in m:
                skip = True
                continue
            if '}' in m:
                skip = False
                continue
            if skip == True:
                continue

            for i in range(8):
                for j in range(8):
                    if board[i][j] > 0:
                        occupy[i][j] = occupy[i][j] + 1
                        if (turn == True):
                            if board[i][j] == 1 or board[i][j] == 8:
                                history[0][k//2].append((i,j))
                            elif board[i][j] == 2 or board[i][j] == 7:
                                history[1][k//2].append((i,j))
                            elif board[i][j] == 3 or board[i][j] == 6:
                                history[2][k//2].append((i,j))
                            elif board[i][j] == 4:
                                history[3][k//2].append((i,j))
                            elif board[i][j] == 5:
                                history[4][k//2].append((i,j))
                            elif board[i][j] >= 9 and board[i][j] <= 16:
                                history[5][k//2].append((i,j))
                        else:
                            if board[i][j] >= 17 and board[i][j] <= 24:
                                history[6][k//2].append((i,j))
                            elif board[i][j] == 25 or board[i][j] == 32:
                                history[7][k//2].append((i,j))
                            elif board[i][j] == 26 or board[i][j] == 31:
                                history[8][k//2].append((i,j))
                            elif board[i][j] == 27 or board[i][j] == 30:
                                history[9][k//2].append((i,j))
                            elif board[i][j] == 28:
                                history[10][k//2].append((i,j))
                            elif board[i][j] == 29:
                                history[11][k//2].append((i,j))



            r = convert.convert(m, board, turn)
            space = (-1,-1)
            #print (m, "\t", r)
            if (len(r) == 2):
                print(m, r)
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
                print(m,r)
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
                #print("")
                #for i in range(8):
                #    for j in range(8):
                #        if board[i][j] == 0:
                #            print(". " ,"",end="")
                #        elif board[i][j] < 10:
                #            print(board[i][j],"","",end="")
                #        else:
                #            print(board[i][j],"",end="")
                #    print("")
                #print("")
                turn = not turn
                sum = 0
                for i in range(8):
                    for j in range(8):
                        sum += visited[i][j]
                #print("total visited", sum)


        print(moves[-1])

        print("Results")
        print("Spaces occupied, length", length)
        for i in range(8):
            print(occupy[i])
        sum = 0
        for i in range(8):
            for j in range(8):
                sum += visited[i][j]
        print("Nr. of times visited, total", sum)
        for i in range(8):
            print(visited[i])
        print("Nr. of times captured")
        for i in range(8):
            print(captured[i])

        pieces = ["black rooks", "black knights", "black bishops", "black queen", "black king", "black pawns", "white pawns", "white rooks", "white knights", "white bishops", "white queen", "white king"]
        for i in range(12):
            print("History for", pieces[i])
            print(history[i])
            print("")

            piece_history.append(history[piece_id])

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

