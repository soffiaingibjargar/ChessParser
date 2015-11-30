def convert(m, board, turn):

    m = m.replace('{', '')
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
                print("queen move", nRow, col)
                if nRow > 7:
                    break
                if board[nRow][col] == value:
                    return ((nRow, col), (row, col))
                if board[nRow][col] > 0:
                    break
            for i in range(7):
                nRow = row - i - 1
                print("queen move",nRow, col)
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
        #print (row, col)

        if m[0] == "N":
            if turn == 0:
                values = [26, 31]
            else:
                values = [2, 7]
            if nRow - row == 2 or nRow - row == -2:
                if col + 1 < 8 and (board[nRow][col + 1] == values[0] or board[nRow][col + 1] == values[1]):
                    return ((nRow, col + 1), (row, col))
                else:
                    return ((nRow, col - 1), (row, col))
            else:
                if col + 2 < 8 and (board[nRow][col + 2] == values[0] or board[nRow][col + 2] == values[1]):
                    return ((nRow, col + 2), (row, col))
                else:
                    return ((nRow, col - 2), (row, col))
        if m[0] == "R":
            values = [0,0]
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
