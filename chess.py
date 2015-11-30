def convert(m, board, turn):
    pieces = ['K', 'Q', 'R', 'B', 'N']
    spaces = ['a','b','c','d','e','f','g','h']
    if m[-1] == "+" or m[-1] == "#":
        m = m[0:-1]

    # castling
    if (m == "O-O"):
        if turn == 0:
            return ("C", ((7,4), (7,6)), ((7,7), (7,5)))
        else:
            return ("C", ((0,4), (0,6)), ((0,7), (0,5)))
    if (m == "O-O-O"):
        #print ("long castling")
        if turn == 0:
            return ("C", ((7,4), (7,2)), ((7,0), (7,3)))
        else:
            return ("C", ((0,4), (0,2)), ((0,0), (0,3)))

    # promotions
    if "=" in m:
        col = ord(m[0]) - ord('a')
        row = 7 - ord(m[1]) + ord('1')
        if "x" not in m:
            if turn == 0:
                nRow = 1
            else:
                nRow = 6
            return ("prom", ((nRow, col), (row, col)), m[-1])
        else:
            col = ord(m[2]) - ord('a')
            row = 7 - ord(m[3]) + ord('1')
            if turn == 0:
                if m[0] < m[2]:
                    return ("prom", ((row + 1, col - 1), (row, col)), m[-1])
                else:
                    return ("prom", ((row + 1, col + 1), (row, col)), m[-1])
            else:
                if m[0] < m[2]:
                    return ("prom", ((row - 1, col - 1), (row, col)), m[-1])
                else:
                    return ("prom", ((row - 1, col + 1), (row, col)), m[-1])

    # regular pawn move
    if len(m) == 2:
        col = ord(m[0]) - ord('a')
        row = 7 - ord(m[1]) + ord('1')
        if turn == 0:
            if board[row + 1][col] > 0:
                return ((row + 1, col), (row, col))
            else:
                return ((row + 2, col), (row, col))
        else:
            if board[row - 1][col] > 0:
                return ((row - 1, col), (row, col))
            else:
                return ((row - 2, col), (row, col))

    # piece move/capture
    if m[0] in pieces and (len(m) == 3 or (len(m) == 4 and m[1] == "x")):
        col = ord(m[1]) - ord('a')
        row = 7 - ord(m[2]) + ord('1')

        if m[1] == "x":
            col = ord(m[2]) - ord('a')
            row = 7 - ord(m[3]) + ord('1')

        if m[0] == "K":
            if turn == 0:
                for i in range(9):
                    nCol = col + i // 3 - 1
                    nRow = row + i % 3 - 1
                    if nCol >= 0 and nRow >= 0 and nCol < 8 and nRow < 8 and board[nRow][nCol] == 29:
                        return ((nRow, nCol), (row, col))
            else:
                for i in range(9):
                    nCol = col + i // 3 - 1
                    nRow = row + i % 3 - 1
                    if nCol >= 0 and nRow >= 0 and nCol < 8 and nRow < 8 and board[nRow][nCol] == 5:
                        return ((nRow, nCol), (row, col))

        elif m[0] == "N":
            #print (col, row)
            #print("Knight move")
            adjacent = [(1,2), (1,-2), (2,1), (2,-1), (-1,2), (-1,-2), (-2,1), (-2,-1)]
            if turn == 0:
                for s in adjacent:
                    nCol = col + s[1]
                    nRow = row + s[0]
                    # print(nCol, nRow, board[nRow][nCol])
                    if nCol >= 0 and nRow >= 0 and nCol < 8 and nRow < 8 and (board[nRow][nCol] == 26 or board[nRow][nCol] == 31):
                        return ((nRow, nCol), (row, col))
            else:
                for s in adjacent:
                    nCol = col + s[1]
                    nRow = row + s[0]
                    if nCol >= 0 and nRow >= 0 and nCol < 8 and nRow < 8 and (board[nRow][nCol] == 2 or board[nRow][nCol] == 7):
                        return ((nRow, nCol), (row, col))
        elif m[0] == "R":
            values = [0,0]
            if turn == 0:
                values = [25,32]
            else:
                values = [1,8]
            for i in range(7):
                nCol = col + i + 1
                if nCol > 7:
                    break
                if board[row][nCol] == values[0] or board[row][nCol] == values[1]:
                    return ((row, nCol), (row, col))
                if board[row][nCol] > 0:
                    break
            for i in range(7):
                nCol = col - i - 1
                if nCol < 0:
                    break
                if board[row][nCol] == values[0] or board[row][nCol] == values[1]:
                    return ((row, nCol), (row, col))
                if board[row][nCol] > 0:
                    break
            for i in range(7):
                nRow = row + i + 1
                if nRow > 7:
                    break
                if board[nRow][col] == values[0] or board[nRow][col] == values[1]:
                    return ((nRow, col), (row, col))
                if board[nRow][col] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                if nRow < 0:
                    break
                if board[nRow][col] == values[0] or board[nRow][col] == values[1]:
                    return ((nRow, col), (row, col))
                if board[nRow][col] > 0:
                    break
        elif m[0] == "B":
            values = [0,0]
            if turn == 0:
                values = [27, 30]
            else:
                values = [3,6]
            for i in range(7):
                nRow = row + i + 1
                nCol = col + i + 1
                if nRow > 7 or nCol > 7:
                    break
                if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row + i + 1
                nCol = col - i - 1
                if nRow > 7 or nCol < 0:
                    break
                if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                nCol = col + i + 1
                if nRow < 0 or nCol > 7:
                    break
                if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                nCol = col - i - 1
                if nRow < 0 or nCol < 0:
                    break
                if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
        elif m[0] == "Q":
            value = 0
            if turn == 0:
                value = 28
            else:
                value = 4
            # check diagonal
            for i in range(7):
                nRow = row + i + 1
                nCol = col + i + 1
                if nRow > 7 or nCol > 7:
                    break
                if board[nRow][nCol] == value:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row + i + 1
                nCol = col - i - 1
                if nRow > 7 or nCol < 0:
                    break
                if board[nRow][nCol] == value:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                nCol = col + i + 1
                if nRow < 0 or nCol > 7:
                    break
                if board[nRow][nCol] == value:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                nCol = col - i - 1
                if nRow < 0 or nCol < 0:
                    break
                if board[nRow][nCol] == value:
                    return ((nRow, nCol), (row, col))
                if board[nRow][nCol] > 0:
                    break
            # check straight lines
            for i in range(7):
                nCol = col + i + 1
                if nCol > 7:
                    break
                if board[row][nCol] == value:
                    return ((row, nCol), (row, col))
                if board[row][nCol] > 0:
                    break
            for i in range(7):
                nCol = col - i - 1
                if nCol < 0:
                    break
                if board[row][nCol] == value:
                    return ((row, nCol), (row, col))
                if board[row][nCol] > 0:
                    break
            for i in range(7):
                nRow = row + i + 1
                if nRow > 7:
                    break
                if board[nRow][col] == value:
                    return ((nRow, col), (row, col))
                if board[nRow][col] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                if nRow < 0:
                    break
                if board[nRow][col] == value:
                    return ((nRow, col), (row, col))
                if board[nRow][col] > 0:
                    break

        return "regular piece move"

    # pawn capture
    if len(m) == 4 and m[0] in spaces and m[1] == "x":
        #return "pawn capture"
        col = ord(m[2]) - ord('a')
        row = 7 - ord(m[3]) + ord('1')
        if board[row][col] == 0:
            print("EN PASSANT - SPECIAL CASE")
            # remove pawn in front of space
            if turn == 0:
                board[row + 1][col] = 0
            else:
                board[row - 1][col] = 0

        if turn == 0:
            if m[0] < m[2]:
                return ((row + 1, col - 1), (row, col))
            else:
                return ((row + 1, col + 1), (row, col))
        else:
            if m[0] < m[2]:
                return ((row - 1, col - 1), (row, col))
            else:
                return ((row - 1, col + 1), (row, col))
    # format Nce4 or Nexc4
    if m[0] in pieces and m[1] in spaces and ((m[2] in spaces and len(m) == 4) or (m[2] == "x" and len(m) == 5)):
        piece = -1
        col = ord(m[2]) - ord('a')
        row = 7 - ord(m[3]) + ord('1')
        nCol = ord(m[1]) - ord('a')
        if "x" in m:
            col = ord(m[3]) - ord('a')
            row = 7 - ord(m[4]) + ord('1')


        if m[0] == "N":
            values = [0,0]
            if turn == 0:
                values = [26, 31]
            else:
                values = [2, 7]
            if nCol - col == 2 or nCol - col == -2:
                if row + 1 < 8 and (board[row + 1][nCol] == values[0] or board[row + 1][nCol] == values[1]):
                    return ((row + 1, nCol), (row, col))
                else:
                    return ((row - 1, nCol), (row, col))
            else:
                if row + 2 < 8 and (board[row + 2][nCol] == values[0] or board[row + 2][nCol] == values[1]):
                    return ((row + 2, nCol), (row, col))
                else:
                    return ((row -2, nCol), (row, col))
        if m[0] == "B":
            values = [0,0]
            if turn == 0:
                values = [27,30]
            else:
                values = [3, 6]
            d = nCol - col
            if nCol + d >= 0 and nCol + d < 8 and (board[nCol + d][nCol] == values[0] or board[nCol + d][nCol] == values[1]):
                return ((nCol + d, nCol), (row, col))
            else:
                return ((nCol - d, nCol), (row, col))
        if m[0] == "R":
            values = [0,0]
            if turn == 0:
                values = [25, 32]
            else:
                values = [1,8]
            if nCol != col:
                return ((row, nCol), (row, col))
            else:
                for i in range(7):
                    nRow = row + i + 1
                    if nRow > 7:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
                for i in range(7):
                    nRow = row - i - 1
                    if nRow < 0:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
        if m[0] == "Q":
            value = -1
            if turn == 0:
                value = 28
            else:
                value = 4
            d = nCol - col
            if nCol + d >= 0 and nCol + d < 8 and (board[nCol + d][nCol] == value):
                return ((nCol + d, nCol), (row, col))
            elif nCol - d >= 0 and nCol - d < 8 and board[nCol - d][nCol] == value:
                return ((nCol - d, nCol), (row, col))
            elif nCol != col:
                return ((row, nCol), (row, col))
            else:
                for i in range(7):
                    nRow = row + i + 1
                    if nRow > 7:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
                for i in range(7):
                    nRow = row - i - 1
                    if nRow < 0:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break

    # format N6xd5
    if m[0] in pieces and m[1] not in spaces and ((m[2] in spaces and len(m) == 4)or (m[2] is "x" and m[3] in spaces and len(m) == 5)):
        piece = -1
        col = ord(m[2]) - ord('a')
        row = 7 - ord(m[3]) + ord('1')
        nRow = 7 - ord(m[1]) + ord('1')
        if "x" in m:
            col = ord(m[3]) - ord('a')
            row = 7 - ord(m[4]) + ord('1')
        print (row, col)

        if m[0] == "N":
            if turn == 0:
                values = [26, 31]
            else:
                values = [2, 7]
            if nRow - row == 2 or nRow - row == -2:
                print("hello", nRow)
                if col + 1 < 8 and (board[nRow][col + 1] == values[0] or board[nRow][col + 1] == values[1]):
                    return ((nRow, col + 1), (row, col))
                else:
                    return ((nRow, col - 1), (row, col))
            else:
                print("hey")
                if col + 2 < 8 and (board[nRow][col + 2] == values[0] or board[nRow][col + 2] == values[1]):
                    return ((nRow, col + 2), (row, col))
                else:
                    return ((nRow, col - 2), (row, col))
        if m[0] == "R":
            values = [0,0]
            print (row, col, nRow)
            if turn == 0:
                values = [25, 32]
            else:
                values = [1,8]
            if nRow != row:
                return ((nRow, col), (row, col))
            else:
                for i in range(7):
                    nCol = col + i + 1
                    if nCol > 7:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
                for i in range(7):
                    nCol = col - i - 1
                    if nCol < 0:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
        if m[0] == "B":
            values = [0,0]
            if turn == 0:
                values = [27,30]
            else:
                values = [3, 6]
            d = nRow - row
            if nRow + d >= 0 and nRow + d < 8 and (board[nRow][nRow + d] == values[0] or board[nRow][nRow + d] == values[1]):
                return ((nRow, nRow + d), (row, col))
            elif nRow - d >= 0 and nRow - d < 8 and (board[nRow][nRow - d] == values[0] or board[nRow][nRow - d] == values[1]):
                return ((nRow, nRow - d), (row, col))

        if m[0] == "Q":
            #print (row, col, nRow)
            if turn == 0:
                value = 28
            else:
                value = 4
            if nRow != row:
                return ((nRow, col), (row, col))
            else:
                for i in range(7):
                    nCol = col + i + 1
                    if nCol > 7:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
                for i in range(7):
                    nCol = col - i - 1
                    if nCol < 0:
                        break
                    if board[nRow][nCol] == values[0] or board[nRow][nCol] == values[1]:
                        return ((nRow, nCol), (row, col))
                    if board[nRow][nCol] > 0:
                        break
            # diagonal queen
            # 2 possible spaces
            d = nRow - row
            if nRow + d >= 0 and nRow + d < 8 and board[nRow][nRow + d] == value:
                if nRow - d >= 0 and nRow - d < 8 and board[nRow][nRow - d] == value:
                    print("ERROR: unimplemented edge case")
                return ((nRow, nRow - d), (row, col))
            else:
                return ((nRow, nRow + d), (row, col))



    print("ERROR: MOVE",m,"NOT HANDLED !!!")
    exit()

piece_history = []

for b in range(1,51):
    if b > 9:
        address = 'C:\\Users\\Stefania\\Documents\\Haskoli\\Data\\games\\chess_' + str(b) + '.txt'
    else:
        address = 'C:\\Users\\Stefania\\Documents\\Haskoli\\Data\\games\\chess_0' + str(b) + '.txt'
    f = open(address, 'r')
    game = f.read()
    moves = game.split()

    length = len(moves) // 3

    for i in range(len(moves) // 3):
        moves.pop(2*i)
    print(moves)

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
    #for m in moves[0:-1]:
    for k in range(len(moves) - 1):
        m = moves[k]

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



        r = convert(m, board, turn)
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
                elif r[2] == "K":
                    piece = 26
                else:
                    piece = 27
            else:
                if r[2] == "R":
                    piece = 1
                elif r[2] == "K":
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
            print("ERROR: NO MOVE !!!")
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
    f.close()

    piece_history.append(history[6])

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
    for r in data[i]:
        for s in r:
            print(s,"",end="")
        print("")
    print("");



